library(tidyverse)
library(geohash)
library(geoR)
library(ggmap)
library(DT)
library(knitr)
library(rgdal)
library(lubridate)
library(maps)

library(geosphere)

setwd("G:\\Misc\\documents\\stopansko\\masters\\1_semester\\Monthly Challenge")
citSofa = read.csv("citSofia.csv")
topo = read.csv("TOPO-DATA\\sofia_topo.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
meteo = read.csv("METEO-data\\lbsf_20120101-20180917_IP.csv")

# Clustering ----
uniqueStationsCoords = unique(citSofa[,c("geohash", "lat", "long")])

# res = lapply(uniqueStationsCoords, FUN = distGeo, p1 = c(uniqueStationsCoords$long, uniqueStationsCoords$lat), p2 = test)
res = as.data.frame(matrix(,ncol=3,nrow=0))
names(res) = c("geohash", "topoIndex", "Elev")
res$geohash = as.character(res$geohash)
for (i in 1:dim(uniqueStationsCoords)[1]) {
  res[i,1] = as.character(uniqueStationsCoords[i,1])
  minDist = distGeo(c(uniqueStationsCoords[i,"long"], uniqueStationsCoords[i, "lat"]), data.frame(topo$Lon, topo$Lat))
  res[i,2] = which.min(minDist)
  res[i,3] = topo[res[i,2],"Elev"]
}

citSofiaElev = merge(res, citSofa, by = "geohash")
citSofiaElev$time = ymd_hms(citSofiaElev$time)
rm(citSofa)
clusters = citSofiaElev %>% group_by(topoIndex)   %>% summarise(
    max(Elev), n(), n_distinct(geohash), min(time), max(time), min(P1), max(P1),
    min(P2), max(P2), min(temperature), max(temperature), min(pressure), max(pressure),
    min(humidity), max(humidity))
names(clusters) = c("topoIndex", "Elev", "obs", "st.num", "t.min", "t.max", "p1.min", "p2.max", "temp.min", "temp.max", "press.min", "pres.max", "hum.min", "hum.max")
# sx8d2x0w6jp

# citSofiaByStation = citSofiaElev %>% group_by(geohash) %>% 
#  summarise(min(temperature), max(temperature), min(pressure), max(pressure), min(humidity), max(humidity))

# Outliers----
which.min(citSofiaElev$temperature) # 838745
citSofiaElev[838745,"geohash"] # sx8dev34hpv
max(citSofiaElev[citSofiaElev$geohash == "sx8dev34hpv",]$temperature) # 23
min(citSofiaElev[citSofiaElev$geohash == "sx8dev34hpv",]$temperature) # -5573
mean(citSofiaElev[citSofiaElev$geohash == "sx8dev34hpv",]$temperature) # -59.50932
# so probably not a random event, the whole station is bad

meanTemp = citSofiaElev %>% group_by(geohash) %>% summarise(mean(temperature), median(temperature))
names(meanTemp) = c("geohash", "t.mean", "t.median")
quantile(meanTemp$t.mean, 0.0025)
meanTempOut = meanTemp[meanTemp$t.mean < -30,]

hist(citSofiaElev$temperature) # reallly left skewed
min(citSofiaElev$temperature) # -5573 wow buddy
hist(citSofiaElev$temperature[citSofiaElev$temperature > -45 & citSofiaElev$temperature < 100])

sx8ddwkkhj4Freq = data.frame(table(citSofiaElev[citSofiaElev$geohash == "sx8ddwkkhj4",]$temperature)) # over 1900 obs -143

citSofiaElev = citSofiaElev[!citSofiaElev$geohash %in% meanTempOut$geohash,]
rm(meanTemp, meanTempOut, sx8ddwkkhj4Freq)

outlier_temp = boxplot.stats(citSofiaElev$temperature)$out
outlier_temp = unique(outlier_temp)
hist(outlier_temp)

outlier_press = boxplot.stats(citSofiaElev$pressure)$out
outlier_press = unique(outlier_press)
hist(outlier_press)

outlier_hum = boxplot.stats(citSofiaElev$humidity)$out
outlier_hum = unique(outlier_hum)
hist(outlier_hum)

meteo$date = as.Date(paste(meteo$year, meteo$Month, meteo$day, sep="-"))
meteo$date = as.POSIXct(meteo$date)
meteoMerg = merge(citSofiaElev[,c("geohash", "time")], meteo, by.y="date", by.x="time")

library(deducorrect)
rules = correctionRules(expression(
  if ( temperature %in% outlier_temp ) {
    meteoVal = meteo[meteo$year==year(time) & meteo$Month==month(time) & meteo$day==day(time)]$TASAVG
    temperature = 0
  }
))
