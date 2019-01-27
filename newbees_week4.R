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
load("newbees_week3.RData")

#Step 4.1: Modelling -------------------
for(i in 1:length(t)){
  t[[i]]$time_date = as.Date(t[[i]]$time)
}  

windows()  
ggplot(t[[76]], aes(time_date, P1int)) + geom_line() + scale_x_date('day')  + ylab("Daily P1 conc") +
  xlab("")

count_ma = ts(na.omit(t[[76]]$P1int), frequency=24)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

for(i in 1:length(t)){
  t[[i]][,c(8:17, 19:25)] = NULL
}

for(i in 1:length(t)){
  t[[i]]$ts_var = ts(data=t[[i]]$P1adj, start = 1, freq = 24)
}

t_model = list()
for(i in 1:length(t)){
  if(names(t)[i] %in% closest_offic$Clust.ID){
    t_model[[i]] = t[[i]][c(which.min(t[[i]]$P1adj!=0):dim(t[[i]])[1]),]
    names(t_model)[i] = names(t)[i]
  }
}
'%ni%' <- Negate('%in%')
for(i in 1:length(t_model)){
  if(names(t_model)[i] %ni% closest_offic$Clust.ID){
    t_model[[i]] = NULL
    i = i - 1  
  }
} #you have to run it 4 times for it to work wtf

for(i in 1:length(t_model)){
  t_model[[i]]$ts_var = ts(t_model[[i]]$ts_var, start = 1, freq = 24)
}

#set a training set with the last 24 hours left for test
t_train = list()
for(i in 1:length(t_model)){
  t_train[[i]] = t_model[[i]][c(1:(dim(t_model[[i]])[1]-24)),]
  t_train[[i]]$ts_var = ts(t_train[[i]]$ts_var, start = 1, freq = 24)
}
names(t_train) = names(t_model)

t_fit_snaive = list()
for(i in 1:length(t_train)){
  t_fit_snaive[[i]] = snaive(t_train[[i]]$ts_var, h=24, biasadj=TRUE)
}

for(i in 1:length(t_fit_snaive)){
  t_fit_snaive[[i]] = as.data.frame(t_fit_snaive[[i]])
}
for(i in 1:length(t_model)){
  t_model[[i]]$ts_var_pred[c((dim(t_model[[i]])[1]-24):dim(t_model[[i]])[1])] = t_fit_snaive[[i]]$`Point Forecast`
  t_model[[i]]$ts_var_pred = ts(t_model[[i]]$ts_var_pred, freq = 24)
}

windows()
autoplot((t_model[[1]]$ts_var)) +
  autolayer(t_model[[1]]$ts_var_pred)

autoplot(t_model[[1]]$ts_var_pred)
