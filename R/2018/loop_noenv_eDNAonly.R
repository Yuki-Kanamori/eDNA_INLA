#df_env = NULL
df_waic = NULL
df_dpm = NULL
df1 = NULL
df2 = NULL
df3 = NULL
splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")

for(i in 1:length(splist)){
  dirname = "/Users/Yuki/Dropbox/eDNA_INLA"
  setwd(dir = dirname)
  
  require(INLA)
  require(tidyverse)
  require(openxlsx)
  
  # 2018 ----------------------------------------------------------
  
  # eDNA & env ----------------------------------------------------------
  # データは4~12月，神奈川は地点によって調査がなかった月がある
  mifish = read.csv("joint_edna2018.csv")
  
  # # catch ---------------------------------------------------------
  # # データは1-12月
  # if(i < 6){
  #   data = read.csv("joint_cpue2018.csv")
  #   data = data %>% dplyr::rename(sp = FISH, cpue = CPUE, lon = Lon, lat = Lat)
  # }else{
  #   data = read.csv("joint_cpue2018_tuikasp2.csv")
  #   data$cpue = data$catch/data$m_effort
  # }
  # 
  # 
  
  # select species ------------------------------------------------
  e_fish = mifish %>% filter(sp_group == paste0(splist[i]), layer == "B")
  summary(e_fish)
  edna = (e_fish$count > 0) + 0
  
  # c_fish = data %>% filter(sp == paste0(splist[i]))
  # # c_mako = data %>% filter(FISH == "makogarei", Y == 2018, M > 2) #eDNAと月を揃えた方が良い？
  # summary(c_fish)
  # # catch = (c_fish$catch > 0) + 0
  # catch = (c_fish$cpue > 0) + 0
  # summary(catch)
  
  
  # INLA ----------------------------------------------------------
  #lonlat
  e_loc = as.matrix(cbind(e_fish$lng, e_fish$lat))
  # c_loc = as.matrix(cbind(c_fish$lon, c_fish$lat))
  # loc = rbind(e_loc, c_loc)
  # 
  # c_locp = as.matrix(cbind(c_fish %>% filter(catch > 0) %>% select(lon), c_fish %>% filter(catch > 0) %>% select(lat)))
  # c_loca = as.matrix(cbind(c_fish %>% filter(catch == 0) %>% select(lon), c_fish %>% filter(catch == 0) %>% select(lat)))
  
  bound2 = inla.nonconvex.hull(e_loc, convex = 0.05, concave = -0.15)
  mesh2 = inla.mesh.2d(boundary = bound2, max.edge = c(0.04, 0.04), cutoff = 0.08/5)
  plot(mesh2)
  # points(c_locp, col = "red", pch = 15, cex = 2) #pdfにする時はcex = 1，その他は.5にする
  # points(c_loca, col = "orange", pch = 16, cex = 1) #pdfにする時はcex = 1，その他は.5にする
  points(e_loc, col = "green", pch = 16, cex = 1)
  mesh2$n #618
  
  # projector matricies
  e_A = inla.spde.make.A(mesh2, loc = e_loc)
  dim(e_A) # 101, 618
  table(rowSums(e_A > 0))
  table(rowSums(e_A))
  table(colSums(e_A) > 0)
  # c_A = inla.spde.make.A(mesh2, loc = c_loc)
  # dim(c_A) # 2321, 618
  # table(rowSums(c_A > 0))
  # table(rowSums(c_A))
  # table(colSums(c_A) > 0)
  
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
                     effects = list(list(i.e2 = 1:mesh2$n), list(eb.0 = rep(1, length(edna)))),
                     tag = "e_dat")
  na = as.matrix(cbind(rep(NA, nrow(coop)), rep(NA, nrow(coop))))
  ep_stk = inla.stack(data = list(y = cbind(na[, 1], na[, 2])),
                      A = list(Ap, 1),
                      effects = list(list(i.e2 = 1:mesh2$n), 
                                     list(eb.0 = rep(1, nrow(coop)))),
                      tag = "ep_dat")
  
  stk_edna = inla.stack(e_stk, ep_stk)
  
  # #cpue
  # c_stk = inla.stack(data = list(y = cbind(NA, catch)),
  #                    A = list(c_A, 1),
  #                    effects = list(list(i.c = 1:mesh2$n, i.c2 = 1:mesh2$n), list(cb.0 = rep(1, length(catch)))),
  #                    tag = "c_dat")
  # cp_stk = inla.stack(data = list(y = cbind(na[, 1], na[, 2])),
  #                     A = list(Ap, 1),
  #                     effects = list(list(i.c = 1:mesh2$n, i.c2 = 1:mesh2$n), 
  #                                    list(cb.0 = rep(1, nrow(coop)))),
  #                     tag = "cp_dat")
  # stk_catch = inla.stack(c_stk, cp_stk)
  # 
  # stk = inla.stack(stk_edna, stk_catch)
  
  # formula
  formula = y ~ 0 + eb.0 + f(i.e2, model = spde)
  
  res = inla(formula, 
             data = inla.stack.data(stk_edna), 
             family = c("binomial", "binomial"), 
             control.predictor = list(compute = TRUE, A = inla.stack.A(stk_edna), link = 1), 
             control.results = list(return.marginals.random = FALSE, return.marginals.predictor = FALSE), 
             control.compute = list(waic = TRUE, dic = TRUE))
  
  res[[53]] = "eDNAonly/no_env"
  res[[54]] = paste0(splist[i])
  
  
  
  
  
  dir_save = paste0(dirname, "/", Sys.Date(), "noenv_eDNA")
  dir.create(dir_save)
  setwd(dir = dir_save)
  save(res, file = paste0(splist[i], ".Rdata"))
  
  best_kono = res
  
  index_ep = inla.stack.index(stk, tag = "ep_dat")$data
  
  pred_mean_e = best_kono$summary.fitted.values[index_ep, "mean"]
  pred_ll_e = best_kono$summary.fitted.values[index_ep, "0.025quant"]
  pred_ul_e = best_kono$summary.fitted.values[index_ep, "0.975quant"]
  
  dpm_e = rbind(data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_mean_e, variable = "pred_mean_eDNA"),
                data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_ll_e, variable = "pred_ll_eDNA"),
                data.frame(east = coop[, 1], north = coop[, 2],
                           value = pred_ul_e, variable = "pred_ul_eDNA"))
  dpm_e$variable = as.factor(dpm_e$variable)

  
  # # with map
  # world_map <- map_data("world")
  # jap <- subset(world_map, world_map$region == "Japan")
  # jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
  # pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")
  # c_map = coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
  
  dpm = dpm_e
  m_dpm = dpm %>% filter(str_detect(variable, "mean"))
  unique(m_dpm$variable)
  dpm$sp = paste0(splist[i])
  df_dpm = rbind(df_dpm, dpm)
  
  # #eDNA
  # g = ggplot(data = m_dpm %>% filter(variable == "pred_mean_eDNA"), aes(east, north, fill = value))
  # t = geom_tile()
  # c = coord_fixed(ratio = 1)
  # s = scale_fill_gradient(name = "encounter prob. (logit)", low = "blue", high = "orange")
  # edna = g+t+c+s+pol+c_map+theme_bw()+labs(title = paste0(splist[i]))
  # 
  # #pred_mean_catch
  # g = ggplot(data = m_dpm %>% filter(variable == "pred_mean_catch"), aes(east, north, fill = value))
  # t = geom_tile()
  # c = coord_fixed(ratio = 1)
  # s = scale_fill_gradient(name = "encounter prob. (logit)", low = "blue", high = "orange")
  # catch = g+t+c+s+pol+c_map+theme_bw()+labs(title = paste0(splist[i]))
  # 
  # ggsave(file = paste0(dir_save, "/edna_", splist[i], ".pdf"), plot = edna, units = "in", width = 11.69, height = 8.27) 
  # ggsave(file = paste0(dir_save, "/catch_", splist[i], ".pdf"), plot = catch, units = "in", width = 11.69, height = 8.27) 
  
  
  # projecting the spatial field ----------------------------------
  # latent distribution -------------------------------------------
  # range_e = apply(mesh2$loc[, c(1, 2)], 2, range)
  # # range_e = apply(coop, 2, range)
  # proj_e = inla.mesh.projector(mesh2, xlim = range_e[, 1], ylim = range_e[, 2], dims = c(50, 50))
  # mean_s_ie = inla.mesh.project(proj_e, best_kono$summary.random$i.c$mean)
  # sd_s_ie = inla.mesh.project(proj_e, best_kono$summary.random$i.c$sd)
  # 
  # df_ie = expand.grid(x = proj_e$x, y = proj_e$y)
  # df_ie$mean_s = as.vector(mean_s_ie)
  # df_ie$sd_s = as.vector(sd_s_ie)
  # 
  # # require(viridis)
  # # require(cowplot)
  # # require(gridExtra)
  # # 
  # # g1 = ggplot(df_ie, aes(x = x, y = y, fill = mean_s_ie))
  # # g2 = ggplot(df_ie, aes(x = x, y = y, fill = sd_s_ie))
  # # # r = geom_raster()
  # # t = geom_tile()
  # # v = scale_fill_viridis(na.value = "transparent")
  # # c = coord_fixed(ratio = 1)
  # # labs1 = labs(x = "Longitude", y = "Latitude", title = "Mean", fill = "mean_theta")
  # # labs2 = labs(x = "Longitude", y = "Latitude", title = "SD", fill = "SD_theta")
  # # m = g1+t+v+c+pol+c_map+labs1+theme_bw()
  # # ggsave(file = paste0(dir_save, "/dist_", splist[i], ".pdf"), plot = m, units = "in", width = 11.69, height = 8.27) 
  # 
  # df_ic = df_ie
  # df_ic$sp = paste0(splist[i])
  # df1 = rbind(df1, df_ic)
  # 
  # # latent fisheries pattern -------------------------------------------
  # range_e = apply(mesh2$loc[, c(1, 2)], 2, range)
  # # range_e = apply(coop, 2, range)
  # proj_e = inla.mesh.projector(mesh2, xlim = range_e[, 1], ylim = range_e[, 2], dims = c(50, 50))
  # mean_s_ic2 = inla.mesh.project(proj_e, best_kono$summary.random$i.c2$mean)
  # sd_s_ic2 = inla.mesh.project(proj_e, best_kono$summary.random$i.c2$sd)
  # 
  # df_ic2 = expand.grid(x = proj_e$x, y = proj_e$y)
  # df_ic2$mean_s = as.vector(mean_s_ic2)
  # df_ic2$sd_s = as.vector(sd_s_ic2)
  # 
  # # g1 = ggplot(df_ic2, aes(x = x, y = y, fill = mean_s_ic2))
  # # g2 = ggplot(df_ic2, aes(x = x, y = y, fill = sd_s_ic2))
  # # # r = geom_raster()
  # # t = geom_tile()
  # # v = scale_fill_viridis(na.value = "transparent")
  # # c = coord_fixed(ratio = 1)
  # # labs1 = labs(x = "Longitude", y = "Latitude", title = "Mean", fill = "Mean_u2")
  # # labs2 = labs(x = "Longitude", y = "Latitude", title = "SD", fill = "SD_u2")
  # # m = g1+t+v+c+pol+c_map+labs1+theme_bw()
  # # ggsave(file = paste0(dir_save, "/fish_", splist[i], ".pdf"), plot = m, units = "in", width = 11.69, height = 8.27) 
  # 
  # df_ic2$sp = paste0(splist[i])
  # df2 = rbind(df2, df_ic2)
  # 
  # latent pom pattern -------------------------------------------
  range_e = apply(mesh2$loc[, c(1, 2)], 2, range)
  # range_e = apply(coop, 2, range)
  proj_e = inla.mesh.projector(mesh2, xlim = range_e[, 1], ylim = range_e[, 2], dims = c(50, 50))
  mean_s_ie2 = inla.mesh.project(proj_e, best_kono$summary.random$i.e2$mean)
  sd_s_ie2 = inla.mesh.project(proj_e, best_kono$summary.random$i.e2$sd)
  
  df_ie2 = expand.grid(x = proj_e$x, y = proj_e$y)
  df_ie2$mean_s = as.vector(mean_s_ie2)
  df_ie2$sd_s = as.vector(sd_s_ie2)
  
  # g1 = ggplot(df_ie2, aes(x = x, y = y, fill = mean_s_ie))
  # g2 = ggplot(df_ie2, aes(x = x, y = y, fill = sd_s_ie))
  # # r = geom_raster()
  # t = geom_tile()
  # v = scale_fill_viridis(na.value = "transparent")
  # c = coord_fixed(ratio = 1)
  # labs1 = labs(x = "Longitude", y = "Latitude", title = "Mean", fill = "Mean_u1")
  # labs2 = labs(x = "Longitude", y = "Latitude", title = "SD", fill = "Mean_u1")
  # m = g1+t+v+c+pol+c_map+labs1+theme_bw()
  # ggsave(file = paste0(dir_save, "/pom_", splist[i], ".pdf"), plot = m, units = "in", width = 11.69, height = 8.27) 
  
  df_ie2$sp = paste0(splist[i])
  df3 = rbind(df3, df_ie2)
  
  # environmental effect -----------------------------------------------------------
  # effect = rbind(data.frame(x = best_kono$summary.random$temp$ID, y = best_kono$summary.random$temp$mean, variable = "Temp"),
  #                data.frame(x = best_kono$summary.random$salinity$ID, y = best_kono$summary.random$salinity$mean, variable = "Sal"))
  # effect = effect %>% filter(x != 1)
  # effect$variable = factor(effect$variable, levels = c("Temp", "Sal"))
  # effect$sp = paste0(splist[i])
  # df_env = rbind(df_env, effect)
  
  # g = ggplot(effect, aes(x = x, y = y))
  # l = geom_line()
  # f = facet_wrap(~ variable, scales = "free")
  # labs = labs(x = "Environmental variable", y = "Effect of environment", title = paste0(splist[i]))
  # env = g+l+f+labs+theme_bw()
  # ggsave(file = paste0(dir_save, "/env_", splist[i], ".pdf"), plot = env, units = "in", width = 11.69, height = 8.27) 
  
  waic = data.frame(waic = res$waic$waic, sp = paste0(splist[i]))
  df_waic = rbind(df_waic, waic)
}

setwd(dir = dir_save)
#write.csv(df_env, "df_env.csv")
write.csv(df_waic, "df_waic.csv")
write.csv(df_dpm, "df_dpm.csv")
write.csv(df1, "df_ic.csv")
write.csv(df2, "df_ic2.csv")
write.csv(df3, "df_ie.csv")
