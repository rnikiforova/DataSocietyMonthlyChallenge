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
load("newbees_week2.RData")

#Step 3.1: Bind the rows of official datasets by station ------------------------
VarClassOfficTime = rep(NA,9)
for(i in 1:length(eudatasets)){
  VarClassOfficTime[[i]] = class(eudatasets[[i]][,c("DatetimeEnd")])
}  #character - we need it to be POSIXt
#Sample obs: "2018-01-03 03:00:00 +01:00"
for(i in 1:length(eudatasets)){
  eudatasets[[i]]$DatetimeEnd = ymd_hms(eudatasets[[i]]$DatetimeEnd, tz="Europe/Athens")
}
#Sample obs: "2018-01-03 04:00:00 EET" - in Eastern-European Time
rm(VarClassOfficTime, i)

#In the EU dataset, leave only the columns we need, i.e. Concentration and DatetimeEnd
for(i in 1:length(eudatasets)){
  eudatasets[[i]] = eudatasets[[i]][,c("Concentration","DatetimeEnd")]
}

st9421 = bind_rows(eudatasets["EEA Data/st9421_2017"], eudatasets["EEA Data/st9421_2018"])
colnames(st9421)=c("P1_st9421","time")
st9572 = bind_rows(eudatasets["EEA Data/st9572_2017"], eudatasets["EEA Data/st9572_2018"])
colnames(st9572)=c("P1_st9572","time")
st9616 = bind_rows(eudatasets["EEA Data/st9616_2017"], eudatasets["EEA Data/st9616_2018"])
colnames(st9616)=c("P1_st9616","time")
st9642 = bind_rows(eudatasets["EEA Data/st9642_2017"], eudatasets["EEA Data/st9642_2018"])
colnames(st9642)=c("P1_st9642","time")
st60881 = as.data.frame(eudatasets["EEA Data/st60881_2018"])
for(i in 1:ncol(st60881)){
  colnames(st60881)[i] = gsub("EEA.Data.st60881_2018.","", colnames(st60881)[i])
}
colnames(st60881)=c("P1_st60881","time")
rm(eudatasets) 

t_st60881 = list()
t_st60881=as.data.frame(seq.POSIXt(from=min(st60881$time),to=max(st60881$time), by="hour"))
colnames(t_st60881) = "time"
t_st60881 = left_join(t_st60881, st60881, by="time")
sum(duplicated(t_st60881$time)) #7
t_st60881 = t_st60881[!(duplicated(t_st60881$time)==T),]
sum(is.na(t_st60881$P1_st60881)) / dim(t_st60881)[1] # 2.68% missings

t_st9421 = list()
t_st9421=as.data.frame(seq.POSIXt(from=min(st9421$time),to=max(st9421$time), by="hour"))
colnames(t_st9421) = "time"
t_st9421 = left_join(t_st9421, st9421, by="time")
sum(duplicated(t_st9421$time)) #92
t_st9421 = t_st9421[!(duplicated(t_st9421$time)==T),]
sum(is.na(t_st9421$P1_st9421)) / dim(t_st9421)[1] # 4.59% missings

t_st9572 = list()
t_st9572=as.data.frame(seq.POSIXt(from=min(st9572$time),to=max(st9572$time), by="hour"))
colnames(t_st9572) = "time"
t_st9572 = left_join(t_st9572, st9572, by="time")
sum(duplicated(t_st9572$time)) #92
t_st9572 = t_st9572[!(duplicated(t_st9572$time)==T),]
sum(is.na(t_st9572$P1_st9572)) / dim(t_st9572)[1] # 2.71% missings

t_st9616 = list()
t_st9616=as.data.frame(seq.POSIXt(from=min(st9616$time),to=max(st9616$time), by="hour"))
colnames(t_st9616) = "time"
t_st9616 = left_join(t_st9616, st9616, by="time")
sum(duplicated(t_st9616$time)) #92
t_st9616 = t_st9616[!(duplicated(t_st9616$time)==T),]
sum(is.na(t_st9616$P1_st9616)) / dim(t_st9616)[1] # 12.02% missings
plot(t_st9616$time, t_st9616$P1_st9616, col = "#333399") #looks linear

t_st9642 = list()
t_st9642=as.data.frame(seq.POSIXt(from=min(st9642$time),to=max(st9642$time), by="hour"))
colnames(t_st9642) = "time"
t_st9642 = left_join(t_st9642, st9642, by="time")
sum(duplicated(t_st9642$time)) #92
t_st9642 = t_st9642[!(duplicated(t_st9642$time)==T),]
sum(is.na(t_st9642$P1_st9642)) / dim(t_st9642)[1] # 3.7% missings

#Interpolate missings
t_st60881$P1_st60881int = na.interpolation(t_st60881$P1_st60881, option = "linear")
t_st9421$P1_st9421int = na.interpolation(t_st9421$P1_st9421, option = "linear")
t_st9572$P1_st9572int = na.interpolation(t_st9572$P1_st9572, option = "linear")
t_st9616$P1_st9616int = na.interpolation(t_st9616$P1_st9616, option = "linear")
t_st9642$P1_st9642int = na.interpolation(t_st9642$P1_st9642, option = "linear")

rm(st60881, st9421, st9572, st9616, st9642)
#Step 3.2.1: Add info from official stations to citizen stations---------------------
#Since we're going to merge by "time", we make sure t's elements' time is also EET
for(i in 1:length(t)){  
  t[[i]]$time = ymd_hms(t[[i]]$time, tz = "Europe/Athens")
  t[[i]] = t[[i]][!is.na(t[[i]]$time)==T,] #Suumer/ Winter time missing
}
#Sample: "2018-03-09 01:00:00 EET"

#Make a left join, missings from official datasets to appear as NAs in the new dataset
for(i in 1:length(t)){
  t[[i]] = merge(t[[i]],t_st9421, by="time", all.x=T)
  t[[i]] = merge(t[[i]],t_st60881, by = "time", all.x=T)
  t[[i]] = merge(t[[i]],t_st9572, by="time", all.x=T)
  t[[i]] = merge(t[[i]],t_st9616, by="time", all.x=T)
  t[[i]] = merge(t[[i]],t_st9642, by="time",all.x=T)
}
#Step 3.2.2: Interpolate missing values---------------------------------------
#Make sure we have Cluster ID everywhere:
for(i in 1:length(t)){
  for(j in 1:dim(t[[i]])[1]){
    if(is.na(t[[i]]$Clust.ID[j]) == T){
      t[[i]]$Clust.ID[j] = t[[i]]$Clust.ID[j-1]
    }
  }
}
rm(i,j)

#Make a table which specifies the % missings in each column for each cluster
Missings = as.data.frame(matrix(nrow = length(t), ncol = 11))
colnames(Missings) = c("Clust.ID", "nobs", "Missing P1", "Missing temp", "Missing hum", "Missing press", "Missing P1_st9421", "Missing P1_st60881", "Missing P1_st9572", "Missing P1_st9616", "Missing P1_st9642")
for(i in 1:dim(Missings)[1]){
  Missings[i,1] = t[[i]]$Clust.ID[1]
  Missings[i,2] = dim(t[[i]])[1]
  Missings[i,3] = round((sum(is.na(t[[i]]$P1)) / Missings[i,2]),2)
  Missings[i,4] = round((sum(is.na(t[[i]]$temperature)) / Missings[i,2]),2)
  Missings[i,5] = round((sum(is.na(t[[i]]$humidity)) / Missings[i,2]),2)
  Missings[i,6] = round((sum(is.na(t[[i]]$pressure)) / Missings[i,2]),2)
  Missings[i,7] = round((sum(is.na(t[[i]]$P1_st9421int)) / Missings[i,2]),2)
  Missings[i,8] = round((sum(is.na(t[[i]]$P1_st60881int)) / Missings[i,2]),2)
  Missings[i,9] = round((sum(is.na(t[[i]]$P1_st9572int)) / Missings[i,2]),2)
  Missings[i,10] = round((sum(is.na(t[[i]]$P1_st9616int)) / Missings[i,2]),2)
  Missings[i,11] = round((sum(is.na(t[[i]]$P1_st9642int)) / Missings[i,2]),2)
}
rm(i)  
#Plot the missings in P1 and decide which ones to interpolate
plot(Missings$Clust.ID, Missings$`Missing P1`, col="#333399") #some clusters - a lot of missings
#We don't wish to interpolate a lot of missings, so we check which ones are worrying
sum(with(Missings,`Missing P1` > 0.15 )) #Only 16 clusters with missings more than 15% --> drop them
IDs_to_drop = list()
IDs_to_drop = which(Missings$`Missing P1`>0.15)
for(i in 1:length(IDs_to_drop)){
  IDs_to_drop[[i]] = Missings$Clust.ID[(IDs_to_drop[[i]])]
}
for(i in 1:length(t)){
  names(t)[i] = t[[i]]$Clust.ID[1]
}
t = t[-(which(names(t) %in% IDs_to_drop))]
rm(i, IDs_to_drop)

#We are going to use linear interpolation to interpolate missing values in the dependent variable
for(i in 1:length(t)){
  t[[i]]$P1int = na.interpolation(t[[i]]$P1, option = "linear")
}
rm(i)


#Step 3.3: Decomposition procedure---------------------
#Figure out the coordinates of the official stations
coords=rep(NA,5)
coords = as.data.frame(coords)
coords$coords = NULL
coords$stations = c("st60881", "st9421","st9572","st9616","st9642")
coords$name = c("Mladost", "Druzhba", "Krasno selo", "Pavlovo", "Nadezhda")
coords$lat = c(42.655488, 42.6665199,42.6823739, 42.6697911, 42.7323862 )
coords$long = c(23.383271, 23.3979793, 23.2940636 ,23.2677177, 23.3106282 )

OfficStat     = SofiaMap +
  geom_point(data = coords, aes(x = long, y = lat), color = "white", size = 9) +
  geom_point(data = coords, aes(x = long, y = lat), color = "red", size = 8)
plot(OfficStat)

#We need the topography data again to determine the relative distance of each cluster to the official stations
topo = read.csv("TOPO-DATA\\sofia_topo.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
colnames(topo)[1:2] = c("lat", "long")                
for(i in 1:nrow(topo)){
  topo$Clust.ID[i]=i
}
sp.topo = topo
coordinates(sp.topo) = ~long+lat
sp.coords = coords  
coordinates(sp.coords)= ~long+lat
Distance = gDistance(spgeom1 = sp.coords, spgeom2 = sp.topo, byid=T)
Distance = as.data.frame(Distance)
for(i in 1:nrow(Distance)){
  Distance$Clust.ID[i]=i
}
colnames(Distance)[1:5] = c("d_st60881", "d_st9421", "d_st9572", "d_st9616","d_st9642")

for(i in 1:length(t)){
  t[[i]]=merge(t[[i]], Distance, by ="Clust.ID", all.x = T)
}

rm(i,Distance, sp.coords, sp.topo, topo)

#Check the correlation between the P1 cit measurements and the official ones
CorrMatrix = data.frame(matrix(NA, length(t), 7))
colnames(CorrMatrix) = c("Clust.ID", "Corr st9421", "Corr st60881", "Corr st9572", "Corr st9616", "Corr st9642", "Max Corr")
for(i in 1:dim(CorrMatrix)[1]){
  CorrMatrix[i,1] = names(t[i])
  CorrMatrix[i,2:6] = cor(na.omit(t[[i]][,c(9,11,13,15,17,18)]))[1:5, 6]
}
for(i in 1:dim(CorrMatrix)[1]){
  CorrMatrix$`Max Corr`[i] = which.max(CorrMatrix[i,2:6])
  CorrMatrix$Max[i] = max(CorrMatrix[i,2:6])
}
sum(CorrMatrix$Max>0.75)
CorrMatrixSub = CorrMatrix[CorrMatrix$Max > 0.75,]
rm(t_st60881, t_st9421, t_st9572, t_st9616, t_st9642)
#Check for stationarity of the dependent variable
DFT=rep(NA,dim(CorrMatrixSub)[1])
for(i in 1:dim(CorrMatrixSub)[1]){
  a=adf.test(na.omit(t[[CorrMatrixSub$Clust.ID[i]]][,"P1int"]))
  DFT[i] = a$p.value
  rm(a)
}
plot(DFT, type="l") #all stationary at 10%, and all but one stationary at 1% => we can assume stationarity
rm(DFT,i)
closest_offic = as.data.frame(matrix(nrow = dim(CorrMatrixSub)[1]))
colnames(closest_offic)="Clust.ID"
closest_offic$Clust.ID = CorrMatrixSub$Clust.ID
closest_offic$max_corr = CorrMatrixSub$`Max Corr`
for(i in 1:dim(closest_offic)[1]){
  for(j in 1:length(t)){
    if(names(t)[j] == closest_offic$Clust.ID[i]){
      closest_offic$min_dist[i] = which.min(t[[j]][1,19:23])
    }
  }
}
closest_offic$max_corr = ifelse(closest_offic$max_corr == 1, "st9421", closest_offic$max_corr)
closest_offic$max_corr = ifelse(closest_offic$max_corr == 2, "st60881", closest_offic$max_corr)
closest_offic$max_corr = ifelse(closest_offic$max_corr == 3, "st9572", closest_offic$max_corr)
closest_offic$max_corr = ifelse(closest_offic$max_corr == 4, "st9616", closest_offic$max_corr)
closest_offic$max_corr = ifelse(closest_offic$max_corr == 5, "st9642", closest_offic$max_corr)

closest_offic$min_dist = ifelse(closest_offic$min_dist == 1, "st60881", closest_offic$min_dist)
closest_offic$min_dist = ifelse(closest_offic$min_dist == 2, "st9421", closest_offic$min_dist)
closest_offic$min_dist = ifelse(closest_offic$min_dist == 3, "st9572", closest_offic$min_dist)
closest_offic$min_dist = ifelse(closest_offic$min_dist == 4, "st9616", closest_offic$min_dist)
closest_offic$min_dist = ifelse(closest_offic$min_dist == 5, "st9642", closest_offic$min_dist)

closest_offic$same = ifelse(closest_offic$max_corr == closest_offic$min_dist, T, F)  
sum(closest_offic$same) #31 w/ same min dist and max corr

eq_max_corr=list()
eq_min_dist=list()
for(i in 1:(dim(closest_offic)[1])){
  data = na.omit(t[[which(names(t)==closest_offic$Clust.ID[i])]][,c("P1int", paste("P1_",closest_offic$max_corr[i],"int",sep=""), paste("P1_",closest_offic$min_dist[i],"int",sep="") )])
  colnames(data) = c("P1", "max_corr", "min_dist")
  eq_max_corr[[i]] = lm(P1~max_corr, data=data) 
  eq_min_dist[[i]] = lm(P1~min_dist,data=data)
  rm(data)
}  
names(eq_max_corr) = closest_offic$Clust.ID
names(eq_min_dist)=closest_offic$Clust.ID

eqsm_max_corr=data.frame(matrix(NA,length(eq_max_corr),3))
colnames(eqsm_max_corr)=c("Estimate","t value","Pr(>|t|)")
for (i in 1:dim(eqsm_max_corr)[1]){
  eqsm_max_corr[i,]=summary(eq_max_corr[[i]])$coefficients[2,c("Estimate","t value","Pr(>|t|)")]
}
eqsm_max_corr$Clust.ID = names(eq_max_corr)
windows()
par(mfrow= c(1,2))
plot(eqsm_max_corr$Clust.ID, eqsm_max_corr$Estimate)
plot(eqsm_max_corr$Clust.ID, eqsm_max_corr$`Pr(>|t|)`)

eqsm_min_dist=data.frame(matrix(NA,length(eq_min_dist),3))
colnames(eqsm_min_dist)=c("Estimate","t value","Pr(>|t|)")
for (i in 1:dim(eqsm_min_dist)[1]){
  eqsm_min_dist[i,]=summary(eq_min_dist[[i]])$coefficients[2,c("Estimate","t value","Pr(>|t|)")]
}
eqsm_min_dist$Clust.ID = names(eq_min_dist)

windows()
par(mfrow= c(1,2))
plot(eqsm_min_dist$Clust.ID, eqsm_min_dist$Estimate)
plot(eqsm_min_dist$Clust.ID, eqsm_min_dist$`Pr(>|t|)`)

for(i in 1:length(t)){
  t[[i]]$P1adj_maxcorr = NA
  t[[i]]$P1adj_mindist = NA
}

for (i in 1:dim(closest_offic)[1]){
  a=which(is.na(t[[which(names(t)==closest_offic$Clust.ID[i])]][,paste("P1_",closest_offic$max_corr[i],"int",sep="")])==F)[1]
  b=tail(which(is.na(t[[which(names(t)==closest_offic$Clust.ID[i])]][,paste("P1_",closest_offic$max_corr[i],"int",sep="")])==F), n=1)
  t[[which(names(t)==closest_offic$Clust.ID[i])]]$P1adj_maxcorr=NA
  t[[which(names(t)==closest_offic$Clust.ID[i])]]$P1adj_maxcorr[c(a:b)]=eq_max_corr[[which(names(eq_max_corr)==closest_offic$Clust.ID[i])]]$fitted.values
  rm(a,b)
}

for (i in 1:dim(closest_offic)[1]){
  a=which(is.na(t[[which(names(t)==closest_offic$Clust.ID[i])]][,paste("P1_",closest_offic$min_dist[i],"int",sep="")])==F)[1]
  b=tail(which(is.na(t[[which(names(t)==closest_offic$Clust.ID[i])]][,paste("P1_",closest_offic$min_dist[i],"int",sep="")])==F), n=1)
  t[[which(names(t)==closest_offic$Clust.ID[i])]]$P1adj_mindist=NA
  t[[which(names(t)==closest_offic$Clust.ID[i])]]$P1adj_mindist[c(a:b)]=eq_min_dist[[which(names(eq_min_dist)==closest_offic$Clust.ID[i])]]$fitted.values
  rm(a,b)
}

for(i in 1:length(t)){
  t[[i]]$P1adj = ifelse(t[[i]]$P1adj_maxcorr == t[[i]]$P1adj_mindist, t[[i]]$P1adj_maxcorr, (t[[i]]$P1adj_maxcorr+t[[i]]$P1adj_mindist)/2)
}

for(i in 1:length(t)){
  for(j in 1:dim(t[[i]])[1]){
    t[[i]]$avg[j] = mean(c(t[[i]][j,9],t[[i]][j,11],t[[i]][j,13],t[[i]][j,15],t[[i]][j,17]))
  }
}
save(list=ls(), file="newbees_week3.RData")
windows()
plot((t[[76]]$avg))
points(t[[76]]$P1adj,col="red")

cor(t[["135"]]$avg, t[["135"]]$P1adj, use = "complete.obs") #94% corelation
corr = cor(t[[i]][,c("P1adj", "temperature", "humidity","pressure")], use="complete.obs")

