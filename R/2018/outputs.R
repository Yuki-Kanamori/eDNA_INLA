setwd("/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし")


# do なし ---------------------------------------------------------
path_nasi = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/doなし"
files1 = list.files(path_nasi, pattern = ".Rdata")

# splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")
df_waic1 = NULL
for(i in 1:length(files)){
  load(files[i])
  waic = data.frame(waic = res$waic$waic, dic = res$dic$dic, sp = paste0(res[[54]]), do = "-")
  df_waic1 = rbind(df_waic1, waic)
}


# do あり ---------------------------------------------------------
path_ari = "/Users/Yuki/Dropbox/eDNA_INLA/est0314/doあり"
files2 = list.files(path_ari, pattern = ".Rdata")

# splist = c("konosiro", "makogarei", "maanago", "isigarei", "suzuki", "kurodai", "kamasu-rui", "isimoti-rui")
df_waic2 = NULL
for(i in 1:length(files2)){
  load(files2[i])
  waic = data.frame(waic = res$waic$waic, dic = res$dic$dic, sp = paste0(res[[54]]), do = "+")
  df_waic2 = rbind(df_waic2, waic)
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

# sal.
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