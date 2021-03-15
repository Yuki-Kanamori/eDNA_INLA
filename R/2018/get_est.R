path_nasi2 = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/dophなし"
# files1 = list.files(path_nasi, pattern = ".Rdata")


splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")

df_dpm = NULL
df1 = NULL
df2 = NULL
df3 = NULL

for(i in 1:length(splist)){
  setwd("/Users/Yuki/Dropbox/eDNA_INLA")
  
  require(INLA)
  require(tidyverse)
  require(openxlsx)
  
  # 2018 ----------------------------------------------------------
  
  # eDNA & env ----------------------------------------------------------
  # データは4~12月，神奈川は地点によって調査がなかった月がある
  mifish = read.csv("joint_edna2018.csv")
  
  # catch ---------------------------------------------------------
  # データは1-12月
  if(i < 6){
    data = read.csv("joint_cpue2018.csv")
    data = data %>% dplyr::rename(sp = FISH, cpue = CPUE, lon = Lon, lat = Lat)
  }else{
    data = read.csv("joint_cpue2018_tuikasp.csv")
    data$cpue = data$catch/data$m_effort
  }
  
  
  
  # select species ------------------------------------------------
  e_fish = mifish %>% filter(sp_group == paste0(splist[i]), layer == "B")
  summary(e_fish)
  edna = (e_fish$count > 0) + 0
  
  c_fish = data %>% filter(sp == paste0(splist[i]))
  # c_mako = data %>% filter(FISH == "makogarei", Y == 2018, M > 2) #eDNAと月を揃えた方が良い？
  summary(c_fish)
  # catch = (c_fish$catch > 0) + 0
  catch = (c_fish$cpue > 0) + 0
  summary(catch)
  
  
  # INLA ----------------------------------------------------------
  #lonlat
  e_loc = as.matrix(cbind(e_fish$lng, e_fish$lat))
  c_loc = as.matrix(cbind(c_fish$lon, c_fish$lat))
  loc = rbind(e_loc, c_loc)
  
  c_locp = as.matrix(cbind(c_fish %>% filter(catch > 0) %>% select(lon), c_fish %>% filter(catch > 0) %>% select(lat)))
  c_loca = as.matrix(cbind(c_fish %>% filter(catch == 0) %>% select(lon), c_fish %>% filter(catch == 0) %>% select(lat)))
  
  bound2 = inla.nonconvex.hull(loc, convex = 0.05, concave = -0.15)
  mesh2 = inla.mesh.2d(boundary = bound2, max.edge = c(0.04, 0.04), cutoff = 0.08/5)
  plot(mesh2)
  points(c_locp, col = "red", pch = 15, cex = 2) #pdfにする時はcex = 1，その他は.5にする
  points(c_loca, col = "orange", pch = 16, cex = 1) #pdfにする時はcex = 1，その他は.5にする
  points(e_loc, col = "green", pch = 16, cex = 1)
  mesh2$n #618
  
  # projector matricies
  e_A = inla.spde.make.A(mesh2, loc = e_loc)
  dim(e_A) # 101, 618
  table(rowSums(e_A > 0))
  table(rowSums(e_A))
  table(colSums(e_A) > 0)
  c_A = inla.spde.make.A(mesh2, loc = c_loc)
  dim(c_A) # 2321, 618
  table(rowSums(c_A > 0))
  table(rowSums(c_A))
  table(colSums(c_A) > 0)
  
  # for prediction ------------------------------------------------
  setwd('/Users/Yuki/FRA/INLAren/spde-book-files')
  
  ## ----opts, echo = FALSE, results = 'hide', message = FALSE, warning = FALSE----
  source('R/initial_setup.R')
  opts_chunk$set(
    fig.path = 'figs/barrier-'
  )
  library(scales)
  library(rgeos)
  ## High resolution maps when using map()
  library(mapdata) 
  ## Map features, map2SpatialPolygons()
  library(maptools)
  
  
  # Tokyo Bay ---------------------------------------------------------------
  # Select region 
  map <- map("world", "Japan", fill = TRUE,
             col = "transparent", plot = TRUE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(
    map, IDs = IDs,
    proj4string = CRS("+proj=longlat +datum=WGS84")) #緯度経度データ
  summary(map.sp)
  
  # make a polygon ------------------------------------------------------------
  pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
    cbind(c(139.7, 139.5, 139.7, 140.1, 140.3, 139.9), # x-axis 
          c(35.1,  35.4,  35.8,  35.8,  35.4,  35.1)), # y-axis
    FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ
  # 35.2->35に変更->35.1に変更
  summary(pl.sel)
  poly.water <- gDifference(pl.sel, map.sp)
  plot(pl.sel)
  plot(map.sp)
  plot(poly.water)
  
  tok_bor = poly.water@polygons[[1]]@Polygons[[1]]@coords
  
  bb_tok = poly.water@bbox
  x = seq(bb_tok[1, "min"] - 1, bb_tok[1, "max"] + 1, length.out = 150)
  y = seq(bb_tok[2, "min"] - 1, bb_tok[2, "max"] + 1, length.out = 150)
  coop = as.matrix(expand.grid(x, y))
  ind = point.in.polygon(coop[, 1], coop[, 2],
                         tok_bor[, 1], tok_bor[, 2])
  coop = coop[which(ind == 1), ]
  plot(coop, asp = 1)
  
  Ap = inla.spde.make.A(mesh = mesh2, loc = coop)
  dim(Ap) #431, 618
  
  # spde
  spde = inla.spde2.pcmatern(mesh = mesh2, alpha = 2, prior.range = c(0.01, 0.05), prior.sigma = c(1, 0.01))
  
  # stack ---------------------------------------------------------
  #eDNA
  e_stk = inla.stack(data = list(y = cbind(edna, NA)),
                     A = list(e_A, 1),
                     effects = list(list(i.e = 1:mesh2$n, i.e2 = 1:mesh2$n), list(eb.0 = rep(1, length(edna)), temp = scale(e_fish$temp), salinity = scale(e_fish$salinity), DO = scale(e_fish$DO), pH = scale(e_fish$pH))),
                     tag = "e_dat")
  na = as.matrix(cbind(rep(NA, nrow(coop)), rep(NA, nrow(coop))))
  ep_stk = inla.stack(data = list(y = cbind(na[, 1], na[, 2])),
                      A = list(Ap, 1),
                      effects = list(list(i.e = 1:mesh2$n, i.e2 = 1:mesh2$n), 
                                     list(eb.0 = rep(1, nrow(coop)), temp = rep(1, nrow(coop)), salinity = rep(1, nrow(coop)), DO = rep(1, nrow(coop)), pH = rep(1, nrow(coop)))
                      ),
                      tag = "ep_dat")
  
  stk_edna = inla.stack(e_stk, ep_stk)
  
  #cpue
  c_stk = inla.stack(data = list(y = cbind(NA, catch)),
                     A = list(c_A, 1),
                     effects = list(list(i.c = 1:mesh2$n, i.c2 = 1:mesh2$n), list(cb.0 = rep(1, length(catch)))),
                     tag = "c_dat")
  cp_stk = inla.stack(data = list(y = cbind(na[, 1], na[, 2])),
                      A = list(Ap, 1),
                      effects = list(list(i.c = 1:mesh2$n, i.c2 = 1:mesh2$n), 
                                     list(cb.0 = rep(1, nrow(coop)))),
                      tag = "cp_dat")
  stk_catch = inla.stack(c_stk, cp_stk)
  
  stk = inla.stack(stk_edna, stk_catch)
  
  file = list.files(path_nasi, pattern = paste0(splist[i], ".Rdata"))
  setwd(dir = path_nasi)
  load(file)
  
  best_kono = res
  

# estimated eDNA and catch ------------------------------------------------
  index_ep = inla.stack.index(stk, tag = "ep_dat")$data
  index_cp = inla.stack.index(stk, tag = "cp_dat")$data
  
  pred_mean_e = best_kono$summary.fitted.values[index_ep, "mean"]
  pred_mean_c = best_kono$summary.fitted.values[index_cp, "mean"]
  pred_ll_e = best_kono$summary.fitted.values[index_ep, "0.025quant"]
  pred_ul_e = best_kono$summary.fitted.values[index_ep, "0.975quant"]
  pred_ll_c = best_kono$summary.fitted.values[index_cp, "0.025quant"]
  pred_ul_c = best_kono$summary.fitted.values[index_cp, "0.975quant"]
  
  dpm_e = rbind(data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_mean_e, variable = "pred_mean_eDNA"),
                data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_ll_e, variable = "pred_ll_eDNA"),
                data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_ul_e, variable = "pred_ul_eDNA"))
  dpm_c = rbind(data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_mean_c, variable = "pred_mean_catch"),
                data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_ll_c, variable = "pred_ll_catch"),
                data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_ul_c, variable = "pred_ul_catch"))
  
  dpm_e$variable = as.factor(dpm_e$variable)
  dpm_c$variable = as.factor(dpm_c$variable)
  dpm = rbind(dpm_e, dpm_c)
  dpm$sp = paste0(splist[i])
  df_dpm = rbind(df_dpm, dpm)
  
  
  
  # latent distribution -------------------------------------------
  range_e = apply(mesh2$loc[, c(1, 2)], 2, range)
  proj_e = inla.mesh.projector(mesh2, xlim = range_e[, 1], ylim = range_e[, 2], dims = c(50, 50))
  mean_s_ie = inla.mesh.project(proj_e, best_kono$summary.random$i.c$mean)
  sd_s_ie = inla.mesh.project(proj_e, best_kono$summary.random$i.c$sd)
  
  df_ie = expand.grid(x = proj_e$x, y = proj_e$y)
  df_ie$mean_s = as.vector(mean_s_ie)
  df_ie$sd_s = as.vector(sd_s_ie)
  df_ic = df_ie
  df_ic$sp = paste0(splist[i])
  df1 = rbind(df1, df_ic)
  
  
  
  
  # latent fisheries pattern -------------------------------------------
  range_e = apply(mesh2$loc[, c(1, 2)], 2, range)
  # range_e = apply(coop, 2, range)
  proj_e = inla.mesh.projector(mesh2, xlim = range_e[, 1], ylim = range_e[, 2], dims = c(50, 50))
  mean_s_ic2 = inla.mesh.project(proj_e, best_kono$summary.random$i.c2$mean)
  sd_s_ic2 = inla.mesh.project(proj_e, best_kono$summary.random$i.c2$sd)
  
  df_ic2 = expand.grid(x = proj_e$x, y = proj_e$y)
  df_ic2$mean_s = as.vector(mean_s_ic2)
  df_ic2$sd_s = as.vector(sd_s_ic2)
  df_ic2$sp = paste0(splist[i])
  df2 = rbind(df2, df_ic2)
  
  
  
  
  # latent pom pattern -------------------------------------------
  range_e = apply(mesh2$loc[, c(1, 2)], 2, range)
  # range_e = apply(coop, 2, range)
  proj_e = inla.mesh.projector(mesh2, xlim = range_e[, 1], ylim = range_e[, 2], dims = c(50, 50))
  mean_s_ie2 = inla.mesh.project(proj_e, best_kono$summary.random$i.e2$mean)
  sd_s_ie2 = inla.mesh.project(proj_e, best_kono$summary.random$i.e2$sd)
  
  df_ie2 = expand.grid(x = proj_e$x, y = proj_e$y)
  df_ie2$mean_s = as.vector(mean_s_ie2)
  df_ie2$sd_s = as.vector(sd_s_ie2)
  df_ie2$sp = paste0(splist[i])
  df3 = rbind(df3, df_ie2)
  
}


setwd(dir = path_nasi2)
write.csv(df_dpm, "df_dpm.csv")
write.csv(df1, "df_ic.csv")
write.csv(df2, "df_ic2.csv")
write.csv(df3, "df_ie2.csv")
