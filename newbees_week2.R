#Step 0: Setup env --------------
library(tidyverse)
library(geohash)
library(geoR)
library(ggmap)
library(DT)
library(knitr)
library(rgdal)
library(lubridate)
library(maps)
library(corrplot)
library(fBasics)
library(geosphere)
library(rgeos)
library(sp)
library(tseries)
library(imputeTS)
library(gmm)
library(tseries)
library(forecast)
library(ggplot2)

rm(list=ls())
setwd("G:\\Misc\\documents\\stopansko\\masters\\1_semester\\Monthly Challenge")
load("newbees_week1.RData")

#Step 2.1: Find the closest station from topo and assign Elevation ------------------
sp.SofiaStationsCoords = SofiaStationsCoords
coordinates(sp.SofiaStationsCoords) = ~long+lat
for(i in 1:nrow(topo)){
  topo$pnt[i]=i
}
sp.topo = topo
coordinates(sp.topo) = ~long+lat
Distance = gDistance(spgeom1 = sp.SofiaStationsCoords, spgeom2 = sp.topo, byid=T)
minDistance = apply(Distance, 2, function(x) order(x, decreasing=F)[1] )
SofiaStationsCoordsElev = cbind(SofiaStationsCoords, topo[minDistance,], apply(Distance,2, function(x) sort(x, decreasing = F)[1] ))  
colnames(SofiaStationsCoordsElev) = c(colnames(SofiaStationsCoords), 'n_lat', 'n_long', 'Elev', 'pnt', 'distance')  
SofiaStationsCoordsElev$lat_err = NULL
SofiaStationsCoordsElev$lng_err = NULL
colnames(topo)[1:2] = c('n_lat', 'n_long')

SofiaStationsCoordsElev$distance = NULL  
citSofia_elev = merge(citSofia, SofiaStationsCoordsElev, by=c("geohash","lat","long"))  
citSofia_elev$lat_err = NULL
citSofia_elev$lon_err = NULL
citSofia_elev = citSofia_elev[,c(1, 15, 14, 2:13)]
citSofia_elev = citSofia_elev[order(citSofia_elev$n_lat, citSofia_elev$n_long, citSofia_elev$geohash),]
colnames(citSofia_elev)[2] = 'Clust.ID'

clusters = SofiaMap +
  geom_point(data = SofiaStationsCoordsElev, aes(x = long, y = lat), color = "white", size = 5.5) +
  geom_point(data = SofiaStationsCoordsElev, aes(x = long, y = lat), color = SofiaStationsCoordsElev$pnt, size = 3.5)

rm(cit_merged, citSofia, Distance, SofiaStationsCoords, sp.SofiaStationsCoords, clusters, sp.topo, topo, i, minDistance)

#Step 2.2: Summarize information by clusters ----------------
Summary_clust = citSofia_elev %>% 
  group_by(Clust.ID) %>% 
  summarise(
    obs = n(), 
    Elev = mean(Elev), 
    stations = length(unique(geohash)),
    min.p1 = min(P1), first.quart.p1 = quantile(P1,.25), third.quart.p1 = quantile(P1,.75), max.p1 = max(P1), 
    min.p2 = min(P2),first.quart.p2 = quantile(P2,.25), third.quart.p2 = quantile(P2,.75), max.p2 = max(P2), 
    first.obs = min(time), last.obs = max(time), days = max(time)-min(time), 
    min.temp = min(temperature),first.quart.temp = quantile(temperature,.25), third.quart.temp = quantile(temperature,.75), max.temp = max(temperature), 
    min.hum = min(humidity),first.quart.hum = quantile(humidity,.25), third.quart.hum = quantile(humidity,.75), max.hum = max(humidity), 
    min.press = min(pressure),first.quart.press = quantile(pressure,.25), third.quart.press = quantile(pressure,.75), max.press = max(pressure))

#Step 2.3: Detect and treat outliers ------------------------ 
#Import official European data
eu = list.files(path="EEA Data", full.names=T, pattern = "*.csv")
eudatasets = lapply(eu, read.csv, na.string = c("", "NA"," ","-999"), 
                    stringsAsFactors = FALSE, fileEncoding="UTF-16LE")

for (i in 1:length(eu)){
  eu[i]=gsub("BG_5_","st", eu[i])
  eu[i]=gsub("_timeseries.csv","",eu[i])
  names(eudatasets)[i]=eu[i]
}
rm(eu,i)

#Import meteorology data
meteo = read.csv("METEO-data\\lbsf_20120101-20180917_IP.csv", na.strings=c("",".","NA"," ",-9999), stringsAsFactors = FALSE)
#Drop all obs. where year < 2017 because our citizen observations start in 2017
meteo = meteo[meteo$year>=2017,]

#Create a vector, which is going to serve as a "rules vecotor"
rules = rep(NA, 8)
names(rules)=c("P1min", "P1max", "Tmin", "Tmax", "Hmin", "Hmax", "Pmin","Pmax")
rules["P1min"] = min(sapply(eudatasets, function(x,y) min(y[,x]), x = "Concentration"))
rules["P1max"] = max(sapply(eudatasets, function(x,y) max(y[,x]), x = "Concentration"))
rules["Tmin"] = min(meteo$TASMIN) 
rules["Tmax"] = max(meteo$TASMAX) 
rules["Hmin"] = min(meteo$RHMIN)
rules["Hmax"] = max(meteo$RHMAX)
rules["Pmin"] = min(meteo$PSLMIN)
rules["Pmax"] = max(meteo$PSLMAX)

rm(meteo)

#All values outside 1.5*IQR are treated as outliers
outlier_temp_dd = data.frame(boxplot.stats(citSofia_elev$temperature)$out)
colnames(outlier_temp_dd)="value"
outlier_press_dd = data.frame(boxplot.stats(citSofia_elev$pressure, coef = 2.75)$out)
colnames(outlier_press_dd)="value"
outlier_hum_dd = data.frame(boxplot.stats(citSofia_elev$humidity)$out)
colnames(outlier_hum_dd) = "value"

#Subset only unique outliers to save time in next steps
outlier_temp_dd = subset(outlier_temp_dd, !duplicated(outlier_temp_dd$value))
outlier_press_dd = subset(outlier_press_dd,!duplicated(outlier_press_dd$value))
outlier_hum_dd = subset(outlier_hum_dd, !duplicated(outlier_hum_dd))

#Check whether some of these detected outliers are in fact within the boundaries, determined by the "rules vector"
outlier_temp_dd$check = NA
for(i in 1:nrow(outlier_temp_dd)){
  if(outlier_temp_dd$value[i] > rules["Tmin"] & 
     outlier_temp_dd$value[i] < rules["Tmax"]){
    outlier_temp_dd$check[i] = TRUE
  }
  else outlier_temp_dd$check[i] = FALSE
}
sum(outlier_temp_dd$check) # 1 outlier that is not true outlier: KEEP IT
outlier_temp_dd=outlier_temp_dd[outlier_temp_dd$check!= TRUE,]

outlier_press_dd$check = NA
for(i in 1:nrow(outlier_press_dd)){
  if(outlier_press_dd$value[i] > rules["Pmin"]*100 &
     outlier_press_dd$value[i] < rules["Pmax"]*100){
    outlier_press_dd$check[i] = TRUE
  }
  else outlier_press_dd$check[i] = FALSE
}
sum(outlier_press_dd$check) # 945 outliers that are not true outliers: KEEP THEM
outlier_press_dd=outlier_press_dd[outlier_press_dd$check!= TRUE,]

outlier_hum_dd$check = NA
for(i in 1:nrow(outlier_hum_dd)){
  if(outlier_hum_dd$value[i] > rules["Hmin"] &
     outlier_hum_dd$value[i] < rules["Hmax"]){
    outlier_hum_dd$check[i] = TRUE
  }
  else outlier_hum_dd$check[i] = FALSE
}
sum(outlier_hum_dd$check) #0 - all outliers are true outliers

outlier_P1_dd = citSofia_elev[(citSofia_elev$P1<rules["P1min"] |
                                 citSofia_elev$P1>rules["P1max"]),]
outlier_P1_dd = data.frame(outlier_P1_dd[,7])
colnames(outlier_P1_dd) = "value"
outlier_P1_dd = subset(outlier_P1_dd, !duplicated(outlier_P1_dd$value))

#Replace all outliers in the dataset with NA
outlier_hum = as.vector(outlier_hum_dd[,1]) #<= -13
outlier_P1 = as.vector(outlier_P1_dd[,1]) #according to official measurements
outlier_press = as.vector(outlier_press_dd[,1]) # <= 89,802 $ >=105,116
outlier_temp = as.vector(outlier_temp_dd[,1]) #<= -21 & >= 46

rules["Pmin"] = 90000
rules["Pmax"] = 105116
rules["Tmin"] = -21
rules["Tmax"] = 46

rm(outlier_temp_dd, outlier_hum_dd, outlier_P1_dd, outlier_press_dd)

citSofia_clean = citSofia_elev

citSofia_clean$P1 = ifelse((citSofia_clean$P1 <rules["P1min"] | citSofia_clean$P1>rules["P1max"]), NA, citSofia_clean$P1)
sum(is.na(citSofia_clean$P1)) #70,032
citSofia_clean$temperature <- ifelse((citSofia_clean$temperature<rules["Tmin"] | citSofia_clean$temperature >rules["Tmax"]), NA, citSofia_clean$temperature)
sum(is.na(citSofia_clean$temperature)) #11,161
citSofia_clean$pressure = ifelse((citSofia_clean$pressure <= rules["Pmin"] | citSofia_clean$pressure >= rules["Pmax"]),NA,citSofia_clean$pressure) 
sum(is.na(citSofia_clean$pressure)) #248,330
citSofia_clean$humidity = ifelse((citSofia_clean$humidity<rules["Hmin"]|citSofia_clean$humidity>rules["Hmax"]), NA, citSofia_clean$humidity)
sum(is.na(citSofia_clean$humidity)) #101,616

#Remove all stations, showing systematic errors w/ P1
citSofia_clean$mism = ifelse(is.na(citSofia_clean$P1), 1, 0)
Summary_clean = citSofia_clean %>% 
  group_by(geohash) %>%
  summarise(freq=n(), mism = sum(mism), perc = mism/freq) %>%
  arrange(desc(perc))
list_mism = Summary_clean$geohash[Summary_clean$perc == 1]
citSofia_clean = citSofia_clean[!citSofia_clean$geohash %in% list_mism,]

rm(citSofia_elev, i, list_mism, outlier_hum, outlier_press, outlier_P1, outlier_temp, rules, Summary_clean, Summary_clust)
save(list=ls(), file="newbees_week2a.RData")

#Step 2.4: Aggregate by cluster and time and return means---------------
t = list()
gu = list()  
k = unique(citSofia_clean$Clust.ID)
for(i in 1:length(k)){
  gu[[i]] = citSofia_clean[citSofia_clean$Clust.ID == k[i],]
  t[[i]] = as.data.frame(seq.POSIXt(from = min(gu[[i]]$time), to = max(gu[[i]]$time), by="hour"))
  colnames(t[[i]])[1] = "time" 
} 

gua = list()
for(i in 1:length(k)){
  gua[[i]] = aggregate(gu[[i]][,c("Clust.ID", "Elev", "time", "P1", "P2", 
                                  "temperature", "humidity", "pressure")], 
                       by = list(gu[[i]]$time),FUN = mean)
}

for(i in 1:length(k)){
  t[[i]] = merge(t[[i]], gua[[i]], all.x = T)
  t[[i]] = t[[i]][,-2]
}

rm(gu, gua, k)

#Step 2.5: Summarize most important statistics -----------------

#NA's, Skewness and Kurtosis
DS = list()
DS$P1 = list() 
DS$temp = list() 
DS$press = list() 
DS$hum = list() 

for(i in 1:length(t)){
  DS$P1[[i]] = basicStats(t[[i]][,c("P1")])
  DS$temp[[i]] = basicStats(t[[i]][,c("temperature")])
  DS$press[[i]] = basicStats(t[[i]][,c("pressure")])
  DS$hum[[i]] = basicStats(t[[i]][,c("humidity")])
}

DS$P1 = as.data.frame(DS$P1)
plot(t(DS$P1["NAs",])/t(DS$P1["nobs",]))  
plot(t(DS$P1["Skewness",])) #assymetric  
plot(t(DS$P1["Kurtosis",])) #fat tails

DS$temp = as.data.frame(DS$temp)
plot(t(DS$temp["NAs",])/t(DS$temp["nobs",]))  
plot(t(DS$temp["Skewness",])) #rather symmetric  
plot(t(DS$temp["Kurtosis",])) #<= |3| in most of the cases

DS$hum = as.data.frame(DS$hum)
plot(t(DS$hum["NAs",])/t(DS$hum["nobs",]))  
plot(t(DS$hum["Skewness",])) #rather symmetric  
plot(t(DS$hum["Kurtosis",])) #<= |3| in most of the cases

DS$press = as.data.frame(DS$press)
plot(t(DS$press["NAs",])/t(DS$press["nobs",])) #a lot of missings
plot(t(DS$press["Skewness",])) #rather symmetric  
plot(t(DS$press["Kurtosis",])) #<= |3| in most of the cases

#Autocorrelation
ACF_c = list()
ACF_c$P1 = list()
ACF_c$temp = list() 
ACF_c$hum = list() 
ACF_c$press = list() 

for(i in 1:length(t)){
  ACF_c$P1 = acf(t[[i]][,c("P1")], na.action = na.pass)
  ACF_c$temp = acf(t[[i]][,c("temperature")], na.action = na.pass)
  ACF_c$hum = acf(t[[i]][,c("humidity")], na.action = na.pass)
  ACF_c$press = acf(t[[i]][,c("pressure")], na.action = na.pass)
} 
plot(ACF_c$P1, main = "P1 Autocorrelation")
plot(ACF_c$temp, main = "Temperature Autocorrelation")
plot(ACF_c$press, main = "Pressure Autocorrelation")
plot(ACF_c$hum, main = "Humidity Autocorrelation")
#Peaks around the 24th hour except with pressure

#Check for stationarity 
Augm_DF = rep(NA, length(t))
for(i in 1:length(t)){
  a = adf.test(na.omit(t[[i]][,"P1"]))
  Augm_DF[i] = a$p.value
  rm(a)
}  
plot(Augm_DF) #mostly stationary
which(Augm_DF>0.05) #non-stationarity at 62 and 105 cluster
save(list=ls(), file="newbees_week2b.RData")
rm(Augm_DF, ACF_c, DS, i)
save(list=ls(), file="newbees_week2.RData")
