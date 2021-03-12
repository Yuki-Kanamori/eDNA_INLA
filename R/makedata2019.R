dir_input = "/Users/Yuki/Dropbox/eDNA_INLA/篠原さん由来/"
dir_output = "/Users/Yuki/Dropbox/eDNA_INLA/"

require(tidyverse)
require(openxlsx)
require(gdata)

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
d2019_1 = d2019_1 %>% mutate(year = 2019, month = as.numeric(str_sub(tag, 4, 5)), day = as.numeric(str_sub(tag, 6, 7)), site = str_sub(tag, 9, -5), depth = str_sub(tag, -3, -3), rep = str_sub(tag, -1, -1))

d2019_2 = inner_join(sp2, d2019_2, by = "ID")
d2019_2 = d2019_2 %>% gather(key = tag, value = copy, 5:ncol(d2019_2)) 
d2019_2 = d2019_2 %>% mutate(year = 2019, month = as.numeric(str_sub(tag, 4, 5)), day = as.numeric(str_sub(tag, 6, 7)), site = str_sub(tag, 9, -5), depth = str_sub(tag, -3, -3), rep = str_sub(tag, -1, -1))

summary(d2019_1)
summary(d2019_2)

df = rbind(d2019_1, d2019_2)

setwd(dir = dir_output)
write.csv(df, "joint_edna2019.csv")
