dir_input = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/"
dir_output = "/Users/Yuki/Dropbox/eDNA_INLA/"

require(tidyverse)
require(openxlsx)
require(gdata)
require(abind)

# 2019 ----------------------------------------------------------
# eDNA ----------------------------------------------------------
setwd(dir = dir_input)
d2019_1 <- read.table("Data/COUNT.mifish1.nrOTU.id97.unoise.txt", header = T)
d2019_2 <- read.table("Data/COUNT.mifish2.nrOTU.id97.unoise.txt", header = T)

setwd(dir = dir_output)
sp1 = read.xls("splist2019.xlsx", sheet = 1)
sp2 = read.xls("splist2019.xlsx", sheet = 2)

d2019_1 = inner_join(sp1, d2019_1, by = "ID")
d2019_1 = d2019_1 %>% gather(key = tag, value = copy, 5:ncol(d2019_1)) 
d2019_1 = d2019_1 %>% mutate(year = 2019, month = as.numeric(str_sub(tag, 4, 5)), day = as.numeric(str_sub(tag, 6, 7)), site = str_sub(tag, 9, -5), SorB = str_sub(tag, -3, -3), rep = str_sub(tag, -1, -1))

d2019_2 = inner_join(sp2, d2019_2, by = "ID")
d2019_2 = d2019_2 %>% gather(key = tag, value = copy, 5:ncol(d2019_2)) 
d2019_2 = d2019_2 %>% mutate(year = 2019, month = as.numeric(str_sub(tag, 4, 5)), day = as.numeric(str_sub(tag, 6, 7)), site = str_sub(tag, 9, -5), SorB = str_sub(tag, -3, -3), rep = str_sub(tag, -1, -1))

summary(d2019_1)
summary(d2019_2)

edna = rbind(d2019_1, d2019_2)

lonlat = read.table("2018/sampling_points.txt", header = T)
lonlat = lonlat %>% dplyr::rename(site = pop)
mifish = left_join(edna, lonlat, by = "site")
#%>% mutate(tag = paste(month, day, site, layer, sep = "_"))

mifish = mifish %>% select(-tag) %>% mutate(tag = paste(year, month, day, site, SorB, sep = "_"))



# env. data ------------------------------------------------------
setwd(dir = dir_input)
# env = read_csv("Data/Env_data_merged_unique.csv")
# summary(env)

#========== chiba ==========#
#========== env1 ==========#
dir = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/env1"
setwd(dir = dir)
path = dir
files = list.files(path)

env1 = NULL
for(i in 1:length(files)){
  temp = read.csv(paste0(files[i]))
  temp = temp[, 1:16]
  temp = temp %>% mutate(year = 2019, month = as.numeric(str_sub(files[i], 9, 10)), day = str_sub(colnames(temp)[1], -2, -1))
  colnames(temp)[1] = "pop"
  env1 = rbind(env1, temp)
}




#========== kanagawa ==========#
# #========== env_129 ==========#
# dir = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/env_129"
# setwd(dir = dir)
# path = dir
# files = list.files(path)
# 
# env_129 = NULL
# for(i in 1:length(files)){
#   temp = read.xls(paste0(files[i]), sheet = 6, skip = 43)
#   temp = temp %>% mutate(year = 2019, month = as.numeric(str_sub(日付情報, 6, 7)), day = as.numeric(str_sub(日付情報, 9, 10)), site = 129)
#   env_129 = rbind(env_129, temp)
# }




# #========== env_27 ==========#
# dir = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/env_27"
# setwd(dir = dir)
# path = dir
# files = list.files(path)
# 
# env_27 = NULL
# for(i in 1:length(files)){
#   temp = read.table(paste0(files[i]), header = T)
#   temp = temp %>% mutate(year = 2019, month = as.numeric(str_sub(files[i], 5, 6)), day = NA, site = 27)
#   env_27 = rbind(env_27, temp)
# }
# 
# env_27_1 = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/201901-27.csv", skip = 43, fileEncoding = "CP932")
# env_27_1 = env_27_1 %>% mutate(year = 2019, month = 1, day = NA, site = 27)
# 
# 
# #========== env_134 ==========#
# dir = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/env_134"
# setwd(dir = dir)
# path = dir
# files = list.files(path)
# 
# env_134 = NULL
# for(i in 1:length(files)){
#   temp = read.table(paste0(files[i]), header = T)
#   temp = temp %>% mutate(year = 2019, month = as.numeric(str_sub(files[i], 5, 6)), day = NA, site = 134)
#   env_134 = rbind(env_134, temp)
# }
# 
# env_134_1 = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/201901-134.csv", skip = 43, fileEncoding = "CP932")
# env_134_1 = env_134_1 %>% mutate(year = 2019, month = 1, day = NA, site = 134)
# 
# 
# #========== env_136 ==========#
# dir = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/env_136"
# setwd(dir = dir)
# path = dir
# files = list.files(path)
# 
# env_136 = NULL
# for(i in 1:length(files)){
#   temp = read.table(paste0(files[i]), header = T)
#   temp = temp %>% mutate(year = 2019, month = as.numeric(str_sub(files[i], 5, 6)), day = NA, site = 136)
#   env_136 = rbind(env_136, temp)
# }
# 
# env_136_1 = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/201901-136.csv", skip = 43, fileEncoding = "CP932")
# env_136_1 = env_136_1 %>% mutate(year = 2019, month = 1, day = NA, site = 136)


# st27, 134, 136 ------------------------------------------------
dir = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/"
st = c("env_27", "env_134", "env_136")
t2 = NULL
t3 = NULL

for(i in 1:length(st)){
  path = paste0(dir, st[i])
  setwd(path)
  files = list.files()
  
  for(j in 1:length(files)){
    t = read.table(files[j], header = T) %>% mutate(file = paste0(files[j]))
    min = min(t$DepSM)
    max = max(t$DepSM)
    s = t %>% filter(DepSM == min) %>% mutate(SorB = "S")
    b = t %>% filter(DepSM == max) %>% mutate(SorB = "B")
    sb = rbind(s, b)
    t2 = abind(t2, sb, along = 1)
  }
  #t3 = abind(t3, t2, along = 1)
}
envk1 = data.frame(t2) %>% mutate(year = 2019, month = as.numeric(str_sub(file, 5, 6)), day = NA, site = as.numeric(str_sub(file, 8, 10)))
(colnames(envk1))
envk1 = envk1 %>% select("T090C", "Ph", "Sal00", "Sbeox0Mg.L","SorB", "year", "month", "day", "site")
colnames(envk1) = c("temp", "pH", "salinity", "DO_mg","SorB", "year", "month", "day", "site")
summary(envk1) #pHの値が変（負の値）

### Jan.
env_27_1 = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/201901-27.csv", skip = 43, fileEncoding = "CP932")
env_27_1 = env_27_1 %>% mutate(year = 2019, month = 1, day = NA, site = 27)
t = env_27_1
min = min(t$深度..m.)
max = max(t$深度..m.)
s = t %>% filter(深度..m. == min) %>% mutate(SorB = "S")
b = t %>% filter(深度..m. == max) %>% mutate(SorB = "B")
env_27_1 = rbind(s, b)

env_134_1 = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/201901-134.csv", skip = 43, fileEncoding = "CP932")
env_134_1 = env_134_1 %>% mutate(year = 2019, month = 1, day = NA, site = 134)
t = env_134_1
min = min(t$深度..m.)
max = max(t$深度..m.)
s = t %>% filter(深度..m. == min) %>% mutate(SorB = "S")
b = t %>% filter(深度..m. == max) %>% mutate(SorB = "B")
env_134_1 = rbind(s, b)

env_136_1 = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/Data/201901-136.csv", skip = 43, fileEncoding = "CP932")
env_136_1 = env_136_1 %>% mutate(year = 2019, month = 1, day = NA, site = 136)
t = env_136_1
min = min(t$深度..m.)
max = max(t$深度..m.)
s = t %>% filter(深度..m. == min) %>% mutate(SorB = "S")
b = t %>% filter(深度..m. == max) %>% mutate(SorB = "B")
env_136_1 = rbind(s, b)

jan = rbind(env_27_1, env_134_1, env_136_1)

(colnames(jan))
jan = jan %>% select("水温....", "塩分....", "DO..mg.l.","SorB", "year", "month", "day", "site")
colnames(jan) = c("temp", "salinity", "DO_mg","SorB", "year", "month", "day", "site")
jan$pH = NA

envk1 = rbind(jan, envk1)



# st129 ---------------------------------------------------------
st2 = "env_129"
path2 = paste0(dir, st2)
setwd(path2)
(files2 = list.files())
t2 = NULL

for(i in 1:length(files2)){
  t = read.xls(paste0(files2[i]), sheet = 6, skip = 43) %>% dplyr::rename(dep = "深度..m.")
  min = min(t[, 3])
  max = max(t[, 3])
  s = t %>% filter(dep == min) %>% mutate(SorB = "S")
  b = t %>% filter(dep == max) %>% mutate(SorB = "B")
  sb = rbind(s, b)
  t2 = abind(t2, sb, along = 1)
}
envk2 = data.frame(t2) %>% mutate(date = as.Date(日付情報)) %>% mutate(year = 2019, month = as.numeric(str_sub(date, 6, 7)), day = as.numeric(str_sub(date, 9, 10)), site = "129")
colnames(envk2) #pHがない！！

envk2 = envk2 %>% select("水温....", "塩分....", "DO..mg.l.","SorB", "year", "month", "day", "site")
colnames(envk2) = c("temp", "salinity", "DO_mg","SorB", "year", "month", "day", "site")
envk2 = envk2 %>% mutate(pH = NA)

env_kana = rbind(envk1, envk2)







# ---------------------------------------------------------------
# chiba ---------------------------------------------------------
# ---------------------------------------------------------------
#eDNA
e_chi = mifish %>% filter(site != "27") %>% filter(site != "134") %>% filter(site != "136") %>% filter(site != "129")
unique(e_chi$site)

unique(env1$pop)
env1 = env1 %>% dplyr::rename(site = pop) 
env1 = env1 %>% mutate(site = if_else(site %in% c("ANESAKI", "ANE"), "AZ", site)) %>%
  mutate(site = if_else(site %in% c("FN", "FUNABASI"), "FB", site)) %>%
  mutate(site = if_else(site == "FUTTU", "FT", site)) %>%
  mutate(site = if_else(site %in% c("URA", "URAYASU"), "UY", site)) %>%
  mutate(SorB = rep(c("S", "B"), nrow(env1)/2)) %>% 
  mutate(tag = paste(year, month, day, site, SorB, sep = "_"))

unique(env1$site)

colnames(env1)

temp = env1 %>% select(水温, tag)
sal = env1 %>% select(塩分, tag)
do = env1 %>% select(DO.mg., tag)
ph = env1 %>% select(ｐＨ, tag)

e_chi = merge(e_chi, temp, by = "tag", all = T)
e_chi = merge(e_chi, sal, by = "tag", all = T)
e_chi = merge(e_chi, do, by = "tag", all = T)
e_chi = merge(e_chi, ph, by = "tag", all = T)



# ---------------------------------------------------------------
# kanagawa ---------------------------------------------------------
# ---------------------------------------------------------------
#st27
st27 = mifish %>% filter(site == "27") %>% select(-tag) %>% mutate(tag = paste(year, month, SorB, sep = "_"))
env27 = env_kana %>% filter(site == "27") %>% mutate(tag = paste(year, month, SorB, sep = "_")) %>% select(-year, -month, -day, -site, -SorB)
st27 = merge(st27, env27, by = "tag")

#st134
st134 = mifish %>% filter(site == "134") %>% select(-tag) %>% mutate(tag = paste(year, month, SorB, sep = "_"))
env134 = env_kana %>% filter(site == "134") %>% mutate(tag = paste(year, month, SorB, sep = "_")) %>% select(-year, -month, -day, -site, -SorB)
st134 = merge(st134, env134, by = "tag")

#st136
st136 = mifish %>% filter(site == "136") %>% select(-tag) %>% mutate(tag = paste(year, month, SorB, sep = "_"))
env136 = env_kana %>% filter(site == "136") %>% mutate(tag = paste(year, month, SorB, sep = "_")) %>% select(-year, -month, -day, -site, -SorB)
st136 = merge(st136, env136, by = "tag")

#st129
st129 = mifish %>% filter(site == "129") %>% select(-tag) %>% mutate(tag = paste(year, month, SorB, sep = "_"))
env129 = env_kana %>% filter(site == "129") %>% mutate(tag = paste(year, month, SorB, sep = "_")) %>% select(-year, -month, -day, -site, -SorB)
st129 = merge(st129, env129, by = "tag")

e_kana = rbind(st27, st134)
e_kana = rbind(e_kana, st136)
e_kana = rbind(e_kana, st129)
unique(e_kana$site)


# combine -------------------------------------------------------
colnames(e_chi)
colnames(e_kana)
mifish = rbind(e_chi %>% dplyr::rename(temp = 水温, salinity = 塩分, DO_mg = DO.mg., pH = ｐＨ), e_kana)

setwd(dir = dir_output)
write.csv(mifish, "joint_edna2019.csv", fileEncoding = "CP932")
