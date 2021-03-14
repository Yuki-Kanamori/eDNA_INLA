setwd("/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし")


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
env = read.csv(paste0(path_nasi, "/df_env.csv"))
env = env %>% mutate(variable = ifelse(env$variable == "ph", "pH", ifelse(env$variable == "temp", "Temperature", "Salinity")))
env$variable = factor(env$variable, levels = c("Temperature", "Salinity", "pH"))

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

# pH
g = ggplot(env %>% filter(variable == "pH"), aes(x = x, y = y, color = sp))
l = geom_line()
f = facet_wrap(~ sp, scales = "free", ncol = 4)
labs = labs(x = "pH (scaled)", y = "Effect of environment", color = "Species")
fig_env_ph = g+l+f+labs+theme_bw()

ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし/temp.pdf"), plot = fig_env_temp, units = "in", width = 11.69, height = 8.27) 
ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし/sal.pdf"), plot = fig_env_sal, units = "in", width = 11.69, height = 8.27) 
ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし/ph.pdf"), plot = fig_env_ph, units = "in", width = 11.69, height = 8.27) 




# estimated eDNA & catch map ----------------------------------------------------------------
path_nasi = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし"
dpm = read.csv("df_dpm.csv")
m_dpm = dpm %>% filter(str_detect(variable, "mean"))
unique(m_dpm$variable)
m_dpm$prob = exp(m_dpm$value)/(1+exp(m_dpm$value))

# with map
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")
c_map = coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))

#eDNA
g = ggplot(data = m_dpm %>% filter(variable == "pred_mean_eDNA"), aes(east, north, fill = prob))
t = geom_tile()
c = coord_fixed(ratio = 1)
s = scale_fill_gradient(name = "Occurrence", low = "white", high = "red")
edna = g+t+c+s+pol+c_map+theme_bw()+labs(title = paste0(splist[i]))

#pred_mean_catch
g = ggplot(data = m_dpm %>% filter(variable == "pred_mean_catch"), aes(east, north, fill = prob))
t = geom_tile()
c = coord_fixed(ratio = 1)
s = scale_fill_gradient(name = "Occurrence", low = "white", high = "orange")
catch = g+t+c+s+pol+c_map+theme_bw()+labs(title = paste0(splist[i]))

ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/edna_", splist[i], ".pdf"), plot = edna, units = "in", width = 11.69, height = 8.27) 
ggsave(file = paste0("/Users/Yuki/Dropbox/eDNA_INLA/est0314/catch_", splist[i], ".pdf"), plot = catch, units = "in", width = 11.69, height = 8.27) 


