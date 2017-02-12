install.packages("ggplot2")
install.packages("plyr")
install.packages("choroplethr")
install.packages("dplyr")
install.packages("choroplethrMaps")
install.packages("ggplot2")
install.packages("hexbin")

library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(choroplethrMaps)
library(ggplot2)
library(hexbin)

##### Download data
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)

states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()
dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008", "COUNTY_CODE_003", 
         "LAT_016", "LONG_017", "YEAR_BUILT_027",   "FRACTURE_092A", 
         "HISTORY_037", "STRUCTURE_LEN_MT_049")
M = select(x16,one_of(keep))
M = as.tbl(M)

#add fips to the tibble and adjust the longitude and latitude by 
#   adding decimal point. Then plot the position of the bridges, 
#   rule out the out-lying points.
M = mutate(M, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
min2dec = function(x){
  n = nchar(x)
  as.numeric(substr(x,1,n-6)) + as.numeric(substr(x,n-5,n))/6e+05 %>% return
}
M = mutate(M,lat = min2dec(LAT_016), lon = -min2dec(LONG_017))
ggplot(data = M) + geom_hex(mapping = aes(y = lat, x = lon))
M = M %>% filter(lon>-150,lat<50,lat>24.5)
ggplot(data = M) + geom_hex(mapping = aes(y = lat, x = lon))
M = M %>% filter(lon< -50)
ggplot(data = M) + geom_hex(mapping = aes(y = lat, x = lon)) + coord_equal()
####Visualization
pdf("USbridge.pdf",family="GB1")
M %>% filter(YEAR_BUILT_027>1900) %>%
  group_by(YEAR_BUILT_027) %>% 
  summarise(histSign=mean(HISTORY_037)) %>%
  ggplot(mapping = aes(x=YEAR_BUILT_027,y=histSign)) +
  geom_point() +
  ggtitle("Mean of Historical Significance Level")

M %>% group_by(fips) %>%
  summarise(historyBridges = sum(HISTORY_037<=3)) %>%
  transmute(region = fips, value = historyBridges) %>% 
  county_choropleth(state_zoom = tolower(states$Alberta[a])) +
  ggtitle("Number Of Historical Bridges")

M %>% filter(YEAR_BUILT_027>1900) %>%
  group_by(YEAR_BUILT_027) %>% 
  summarise(histSign=min(HISTORY_037)) %>%
  ggplot(mapping = aes(x=YEAR_BUILT_027,y=histSign)) +
  geom_point() +
  ggtitle("Minimum of Historical Significance Level")

M %>% filter(YEAR_BUILT_027>1900) %>%
  group_by(YEAR_BUILT_027) %>% 
  summarise(meanLength=sum(STRUCTURE_LEN_MT_049)) %>%
  ggplot(mapping = aes(x=YEAR_BUILT_027,y=meanLength)) +
  geom_point() +
  ggtitle("Total Length Of Bridge Built Each Year From 1900")

a <- c(1,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,
       21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,
       39,40,41,42,43,44,45,46,47,48,49,50)
M %>% mutate(Roosevelt=(YEAR_BUILT_027>1933)&(YEAR_BUILT_027<1939)) %>% 
  group_by(fips) %>%
  summarise(number = sum(Roosevelt)) %>%
  transmute(region = fips, value = number) %>% 
  county_choropleth(state_zoom = tolower(states$Alberta[a])) +
  ggtitle("#Bridges Built Between 1933 and 1939")
dev.off()







