setwd("/Users/Yuki/Dropbox/eDNA_INLA")

require(tidyverse)
require(openxlsx)

# 2018 ----------------------------------------------------------
# eDNA ----------------------------------------------------------
# データは4-12月
edna = read.table("2018_Mifish/COUNT.mifish.unoise.2018.v2.txt", header = T)
unique(edna$layer)
edna = edna %>% gather(key = zotu, value = count, 6:ncol(edna))
(unique(edna$zotu))
analize = data.frame(zotu = c("Zotu62", "Zotu59", "Zotu120", "Zotu6", "Zotu1", "Zotu13"), sp = c("maanago", "makogarei", "isigarei", "suzuki", "konosiro", "kurodai"))
edna2 = edna %>% filter(zotu %in% analize$zotu)
edna2 = merge(edna2, analize, by = "zotu")
(unique(edna2$site))

lonlat = read.table("2018/sampling_points.txt", header = T)
(unique(lonlat$pop))
head(lonlat, 2)
lonlat = lonlat %>% dplyr::rename(site = pop)
mifish = left_join(edna2, lonlat, by = "site") %>% mutate(year = as.numeric(str_sub(date, 1, 2)), month = as.numeric(str_sub(date, 3, 4)), day = as.numeric(str_sub(date, 5, 6))) %>% mutate(tag = paste(month, day, site, layer, sep = "_"))

# chiba ---------------------------------------------------------
#eDNA
e_chi = mifish %>% filter(site != "27") %>% filter(site != "134") %>% filter(site != "136") %>% filter(site != "129")
unique(e_chi$site)

#env
env = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/env2018.csv")
env = env %>% mutate(tag = paste(Month, Day, Point, SorB, sep = "_"))
(unique(env$Point))
temp = env %>% select(temp, tag)
sal = env %>% select(salinity, tag)
do = env %>% select(DO_mg, tag)
ph = env %>% select(pH, tag)

e_chi = merge(e_chi, temp, by = "tag", all = T)
e_chi = merge(e_chi, sal, by = "tag", all = T)
e_chi = merge(e_chi, do, by = "tag", all = T)
e_chi = merge(e_chi, ph, by = "tag", all = T)



# kanagawa ------------------------------------------------------
# メモ ------------------------------------------------------------
# st27 = mifish %>% filter(site == "27") #envはdayがNA
# unique(st27$date) #6回調査
# st134 = mifish %>% filter(site == "134") #envはdayがNA
# unique(st134$date) #7回調査
# st136 = mifish %>% filter(site == "136") #envはdayがNA
# unique(st136$date) #7回調査
# st129 = mifish %>% filter(site == "129") #envはpHがNA
# unique(st129$date) #8回調査

env2 = read.csv("/Users/Yuki/Dropbox/eDNA_INLA/env2018kanagawa.csv")

#st27
st27 = mifish %>% filter(site == "27") %>% mutate(tag = paste(month, layer, sep = "_"))
env27 = env2 %>% filter(site == "27") %>% mutate(tag = paste(month, SorB, sep = "_")) %>% select(-year, -month, -day, -site)
st27 = merge(st27, env27, by = "tag", all = T)

#st134
st134 = mifish %>% filter(site == "134") %>% mutate(tag = paste(month, layer, sep = "_"))
env134 = env2 %>% filter(site == "134") %>% mutate(tag = paste(month, SorB, sep = "_")) %>% select(-year, -month, -day, -site)
st134 = merge(st134, env134, by = "tag", all = T)

#st136
st136 = mifish %>% filter(site == "136") %>% mutate(tag = paste(month, layer, sep = "_"))
env136 = env2 %>% filter(site == "136") %>% mutate(tag = paste(month, SorB, sep = "_")) %>% select(-year, -month, -day, -site)
st136 = merge(st136, env136, by = "tag", all = T)

#st129
st129 = mifish %>% filter(site == "129") %>% mutate(tag = paste(month, layer, sep = "_"))
env129 = env2 %>% filter(site == "129") %>% mutate(tag = paste(month, SorB, sep = "_")) %>% select(-year, -month, -day, -site)
st129 = merge(st129, env129, by = "tag", all = T)

e_kana = rbind(st27, st134)
e_kana = rbind(e_kana, st136)
e_kana = rbind(e_kana, st129)
unique(e_kana$site)


# eDNA2 ----------------------------------------------------------
colnames(e_chi)
colnames(e_kana)
mifish = rbind(e_chi, e_kana %>% select(-X, -SorB))

# catch ---------------------------------------------------------
# データは1-12月
setwd("/Users/Yuki/Dropbox/Network2020")
data = read.csv("VASTdata.csv") %>% filter(Y == 2018)
head(data, 2)
summary(data)
# data = data %>% mutate(lon = Lon %/% 1 + (Lon %% 1)*60 + 0.05, lat = Lat %/% 1 + (Lat %% 1)*60 + 0.05)

setwd("/Users/Yuki/Dropbox/eDNA_INLA")
write.csv(mifish, "joint_edna2018.csv", fileEncoding = "CP932")
write.csv(data, "joint_cpue2018.csv", fileEncoding = "CP932")
