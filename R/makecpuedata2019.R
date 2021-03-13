dir_input = "/Users/Yuki/Dropbox/eDNA_INLA/cpue2019/追加データ2020年3月まで/"
dir_output = "/Users/Yuki/Dropbox/eDNA_INLA/cpue2019/"

require(tidyverse)
require(openxlsx)
require(gdata)
require(abind)


# combine the added data ----------------------------------------------------------
# (catch only data; not include zero data) ----------------------------------------------------------
setwd(dir = dir_input)
path = dir_input
files = list.files(path)

all = NULL
for(i in 1:length(files)){
  temp = read.xlsx(paste0(files[i]), colNames = T)
  temp = temp %>% select(年月日, 緯度, 経度, 回数, CPUE, 全銘柄) %>%
    mutate(check = as.Date(temp$年月日, origin = "1899-12-30")) %>% 
    mutate(year = as.numeric(str_sub(check, 1, 4)), month = as.numeric(str_sub(check, 6, 7)), day = as.numeric(str_sub(check, 9, 10))) 
  
  # gear
  if(str_detect(files[i], pattern = "ビーム")){
    temp = temp %>% mutate(gear = "beam")
  }else{
    temp = temp %>% mutate(gear = "keta")
  }
  
  #species name
  if(str_detect(files[i], pattern = "イシガレイ")){
    temp$sp = "isigarei"
  }
  if(str_detect(files[i], pattern = "ｲｼﾓﾁ")){
    temp$sp = "isimoti-rui"
  }
  if(str_detect(files[i], pattern = "カマス")){
    temp$sp = "kamasu-rui"
  }
  if(str_detect(files[i], pattern = "クルマエビ")){
    temp$sp = "kurumaebi"
  }
  if(str_detect(files[i], pattern = "クロダイ")){
    temp$sp = "kurodai"
  }
  if(str_detect(files[i], pattern = "コウイカ")){
    temp$sp = "kouika"
  }
  if(str_detect(files[i], pattern = "コノシロ")){
    temp$sp = "konosiro"
  }
  if(str_detect(files[i], pattern = "スズキ")){
    temp$sp = "suzuki"
  }
  if(str_detect(files[i], pattern = "トラフグ")){
    temp$sp = "torafugu"
  }
  if(str_detect(files[i], pattern = "マアナゴ")){
    temp$sp = "maanago"
  }
  if(str_detect(files[i], pattern = "マコガレイ")){
    temp$sp = "makogarei"
  }
  
  
  all = rbind(all, temp)
  
}

# check
unique(all$sp)
unique(all$gear)

all = all %>% dplyr::rename(catch = 全銘柄, effort = 回数) %>% 
  mutate(lat = 緯度 %/% 100 + 緯度 %% 100/60, lon = 経度 %/% 100 + 経度 %% 100/60) %>% 
  select(year, month, day, lat, lon, sp, effort, CPUE, catch, gear, -check, -年月日, -緯度, -経度)

setwd(dir = dir_output)
write.csv(all, "added_data.csv")








# check spatial location -------------------------------
require(maps)
require(mapdata)
library(ggrepel)

p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
t2 + geom_point(data = all, aes(x = lon, y = lat), shape = 16, size = 1)
summary(all)

tent = all[all$lon > 139.7, ]
summary(tent)
t2 + geom_point(data = tent, aes(x = lon, y = lat), shape = 16, size = 1)

ue = tent %>% filter(lat > 35.4)
sita = tent %>% filter(lat <= 35.4)
t2 + geom_point(data = sita, aes(x = lon, y = lat), shape = 16, size = 1)
summary(sita)
sita = sita[sita$lon != max(sita$lon), ]
t2 + geom_point(data = sita, aes(x = lon, y = lat), shape = 16, size = 1)

tent2 = rbind(ue, sita)
setwd(dir = dir_output)
write.csv(tent2, "tent.csv")



# add zero data -------------------------------
tent2 = tent2 %>% mutate(tag = paste(year, formatC(tent2$month, width=2, flag="0"), formatC(tent2$day, width=2, flag="0"), format(as.numeric(str_sub(tent2$lon, 1, 8)), nsmall = 4), format(as.numeric(str_sub(tent2$lat, 1, 8)), nsmall = 4), sep = "_"))
effort = tent2 %>% filter(year == 2019) %>% group_by(tag) %>% summarize(m_effort = mean(effort)) 
# tent3 = left_join(tent2 %>% filter(year == 2019), effort, by = "tag")

splist = unique(tent2$sp)
for(i in 1:length(splist)){
  assign(paste(splist[i]),
         tent2 %>% filter(sp == splist[i]))
}

# test = left_join(konosiro %>% filter(year == 2019) %>% select(tag, effort, CPUE, catch), 
#                  suzuki %>% filter(year == 2019) %>% select(tag, effort, CPUE, catch), by = "tag", all = T)

tent3 = tent2 %>% filter(year == 2019)
taglist = data.frame(tag = unique(tent3$tag)) 
taglist = taglist %>% 
  mutate(year = as.numeric(str_sub(tag, 1, 4)), month = as.numeric(str_sub(tag, 6, 7)), day = as.numeric(str_sub(tag, 9, 10)), 
         lon = as.numeric(str_sub(tag, 12, 19)), lat = as.numeric(str_sub(tag, 21, 28)))
taglist = left_join(taglist, effort, by = "tag")

# konosiro = left_join(taglist, konosiro %>% select(-year, -month, -day, -lon, -lat), by = "tag", all = T) %>% select(-tag) %>% mutate(sp = "konosiro")
# kono1 = konosiro %>% select(- CPUE, -catch)
# kono2 = konosiro %>% select(CPUE, catch)
# kono2[is.na(kono2)] = 0
# kono = cbind(kono1, kono2)

splist = unique(tent3$sp)
for(i in 1:length(splist)){
  sp = tent3 %>% filter(sp == splist[i])
  sp = left_join(taglist, sp %>% select(-year, -month, -day, -lon, -lat), by = "tag", all = T) %>% select(-tag) %>% mutate(sp = paste0(splist[i]))
  sp1 = sp %>% select(- CPUE, -catch)
  sp2 = sp %>% select(CPUE, catch)
  sp2[is.na(sp2)] = 0
  assign(paste(splist[i]),
         cbind(sp1, sp2))
}

cpue2019 = NULL
for(i in 1:length(splist)){
  temp = get(splist[i])
  cpue2019 = rbind(cpue2019, temp)
}

setwd(dir = dir_output)
write.csv(cpue2019, "cpue2019.csv")
