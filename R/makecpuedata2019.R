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
