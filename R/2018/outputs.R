# setwd("/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし")


# do なし ---------------------------------------------------------
path_nasi = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし"
files1 = list.files(path_nasi, pattern = ".Rdata")

# splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")
df_waic1 = NULL
for(i in 1:length(files1)){
  setwd(dir = path_nasi)
  load(files1[i])
  waic1 = data.frame(waic = res$waic$waic, dic = res$dic$dic, sp = paste0(res[[54]]), do = "-")
  df_waic1 = rbind(df_waic1, waic1)
}


# do あり ---------------------------------------------------------
path_ari = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/doあり"
files2 = list.files(path_ari, pattern = ".Rdata")

# splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")
df_waic2 = NULL
for(i in 1:length(files2)){
  setwd(dir = path_ari)
  load(files2[i])
  waic2 = data.frame(waic = res$waic$waic, dic = res$dic$dic, sp = paste0(res[[54]]), do = "+")
  df_waic2 = rbind(df_waic2, waic2)
}

df_model = rbind(df_waic1, df_waic2)



# 作図 ------------------------------------------------------------
# environmental factors
path_nasi2 = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/dophなし"
env = read.csv(paste0(path_nasi2, "/df_env.csv"))
env = env %>% mutate(variable = ifelse(env$variable == "Temp", "Temperature", "Salinity"))

# all
g = ggplot(env, aes(x = x, y = y, color = sp))
l = geom_line()
f = facet_wrap(~ variable, scales = "free", ncol = 2)
labs = labs(x = "Environmental variable", y = "Effect of environment", color = "Species")
fig_env_all = g+l+f+labs+theme_bw()

# temp.
g = ggplot(env %>% filter(variable == "Temperature"), aes(x = x, y = y, color = sp))
l = geom_line()
f = facet_wrap(~ sp, scales = "free", ncol = 4)
labs = labs(x = "Temperature (scaled)", y = "Effect of environment", color = "Species")
fig_env_temp = g+l+f+labs+theme_bw()

# sal.　クロダイとスズキではピークが低い
g = ggplot(env %>% filter(variable == "Salinity"), aes(x = x, y = y, color = sp))
l = geom_line()
f = facet_wrap(~ sp, scales = "free", ncol = 4)
labs = labs(x = "Salinity (scaled)", y = "Effect of environment", color = "Species")
fig_env_sal = g+l+f+labs+theme_bw()

# # pH
# g = ggplot(env %>% filter(variable == "pH"), aes(x = x, y = y, color = sp))
# l = geom_line()
# f = facet_wrap(~ sp, scales = "free", ncol = 4)
# labs = labs(x = "pH (scaled)", y = "Effect of environment", color = "Species")
# fig_env_ph = g+l+f+labs+theme_bw()

ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/dophなし/temp.pdf"), plot = fig_env_temp, units = "in", width = 11.69, height = 8.27) 
ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/dophなし/sal.pdf"), plot = fig_env_sal, units = "in", width = 11.69, height = 8.27) 
#ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし/ph.pdf"), plot = fig_env_ph, units = "in", width = 11.69, height = 8.27) 




# estimated eDNA & catch map ----------------------------------------------------------------
path_nasi2 = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/dophなし"
dpm = read.csv("df_dpm.csv")
m_dpm = dpm %>% filter(str_detect(variable, "mean"))
unique(m_dpm$variable)
m_dpm$prob = exp(m_dpm$value)/(1+exp(m_dpm$value))

# with map
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gracy 50", fill="gray 50")
c_map = coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))

#eDNA
splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")
for(i in 1:length(splist)){
  g = ggplot(data = m_dpm %>% filter(sp == splist[i], variable == "pred_mean_eDNA"), aes(east, north, fill = prob))
  t = geom_tile()
  c = coord_fixed(ratio = 1)
  s = scale_fill_gradient(name = "Occurrence", low = "white", high = "red")
  f = facet_wrap(~ sp, ncol = 4)
  edna = g+t+c+s+f+pol+c_map+theme_bw()+labs(title = "Estimated eDNA")
  
  ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/edna_", splist[i], ".pdf"), plot = edna, units = "in", width = 11.69, height = 8.27) 
}


#pred_mean_catch
#何か変
splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")
for(i in 1:length(splist)){
  g = ggplot(data = m_dpm %>% filter(sp == files[i], variable == "pred_mean_catch"), aes(east, north, fill = prob))
  t = geom_tile()
  c = coord_fixed(ratio = 1)
  s = scale_fill_gradient(name = "Occurrence", low = "white", high = "red")
  catch = g+t+c+s+pol+c_map+theme_bw()+labs(title = paste0(splist[i]))
  
  ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/catch_", splist[i], ".pdf"), plot = edna, units = "in", width = 11.69, height = 8.27) 
}





# latent variables map -----------------------------------------------------------
# distribution
path_nasi2 = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/dophなし"
df_ic = read.csv("df_ic.csv")

require(viridis)
require(cowplot)
require(gridExtra)

for(i in 1:length(splist)){
  g1 = ggplot(df_ic %>% filter(sp == splist[i]), aes(x = x, y = y, fill = mean_s_ie))
  t = geom_tile()
  v = scale_fill_viridis(na.value = "transparent")
  c = coord_fixed(ratio = 1)
  s = scale_fill_gradient(name = "Occurrence", low = "white", high = "red")
  labs1 = labs(x = "Longitude", y = "Latitude", title = "", fill = "")
  
  m = g1+t+c+v+pol+c_map+labs1+theme_bw()
  ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/dist_", splist[i], ".pdf"), plot = m, units = "in", width = 11.69, height = 8.27) 
  
}


# error of fisheries -----------------------------------------------
path_nasi2 = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/dophなし"
df_ic2 = read.csv("df_ic2.csv")

require(viridis)
require(cowplot)
require(gridExtra)

for(i in 1:length(splist)){
  g1 = ggplot(df_ic2 %>% filter(sp == splist[i]), aes(x = x, y = y, fill = mean_s_ie))
  t = geom_tile()
  v = scale_fill_viridis(na.value = "transparent")
  c = coord_fixed(ratio = 1)
  s = scale_fill_gradient(name = "Occurrence", low = "white", high = "red")
  labs1 = labs(x = "Longitude", y = "Latitude", title = "", fill = "")
  
  m = g1+t+c+v+pol+c_map+labs1+theme_bw()
  ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/fish_", splist[i], ".pdf"), plot = m, units = "in", width = 11.69, height = 8.27) 
  
}


