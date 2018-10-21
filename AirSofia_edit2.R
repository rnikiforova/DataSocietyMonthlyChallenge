#Step 1: Import datasets -----------------------
  library(tidyverse)
  library(geohash)
  library(geoR)
  library(ggmap)
  library(DT)
  library(knitr)
  library(rgdal)
  library(lubridate)
  library(maps)
  #Import citizen data for 2017
  cit17 = read.csv("D:\\SAVE DELL\\D\\SU\\Monthly Challenge\\Air Tube\\data_bg_2017.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
  cit18 = read.csv("D:\\SAVE DELL\\D\\SU\\Monthly Challenge\\Air Tube\\data_bg_2018.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
  topo = read.csv("D:\\SAVE DELL\\D\\SU\\Monthly Challenge\\TOPO-DATA\\sofia_topo.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
  colnames(topo)[1:2] = c("lat", "long")
  
#Step 2: Check variables' classes ----------------------------
  #class of all variables in the cit17 dataset using the apply() function
  VarClass17 = data.frame(names(cit17))
  VarClass17[,2] = rapply(cit17, class)
  VarClass18 = data.frame(names(cit18))
  VarClass18[,2] = rapply(cit18, class)
  VarClassTopo = data.frame(names(topo))
  VarClassTopo[,2] = rapply(topo, class)

  cit17$time = ymd_hms(cit17$time) #Changes the class to POSIXt
  cit18$time = ymd_hms(cit18$time) 
  
#Step 3: Identify unique stations and merge datasets ------------------------
  length(unique(cit17[,2])) #383 unique stations in 2017
  length(unique(cit18[,2])) #1254 unique geoh in 2018
  
  diff = setdiff(cit17$geohash, cit18$geohash) #finds geohash from '17 not in '18
  length(unique(diff)) #11 values
  cit17 = cit17[!cit17$geohash %in% diff,] #removes all obs from cit17 w/ geohash in diff
  rm(diff)
  
  cit_merged = bind_rows(cit17, cit18) #merge the two datasets together
  rm(cit17, cit18)
#Step 4: Summarize availability by stations-----------------------
  sum(is.na(cit_merged$geohash)) #4 missings geohash
  cit_merged = cit_merged[!is.na(cit_merged$geohash),] #remove missings
 
  Summary_cit_merged = cit_merged %>% group_by(geohash) %>% summarise(n(),min(time), max(time), max(time)-min(time))
  colnames(Summary_cit_merged) = c("geohash", "Freq","tmin", "tmax","days" )  

  cit_merged = merge(cit_merged, Summary_cit_merged, by = 'geohash')
  rm(Summary_cit_merged)
  cit_merged$tmin = NULL
  cit_merged$tmax = NULL
  
  cit_merged = cit_merged[(cit_merged$days > 7 & cit_merged$Freq >100),] #we delete all obs. w/ fewer than 7 day long obs. window
  cit_merged = cit_merged[order(cit_merged$Freq, cit_merged$time, decreasing = TRUE),]
#Step 5: Summarize on map---------------------------
  cit_merged = transform(cit_merged, latlng = gh_decode(cit_merged$geohash))
  colnames(cit_merged)[10:13] = c("lat", "long", "lat_err", "lon_err")    
  
  Bulgaria = map_data(map = "world",region="Bulgaria")
  BulgariaMap = ggplot() + geom_polygon(data = Bulgaria, aes(x=long, y = lat, group = group)) + coord_fixed(1.3)
  
  #get unique stations' coordinates
  StationsCoords = data.frame(unique(cit_merged$geohash), stringsAsFactors = FALSE)
  colnames(StationsCoords) = "geohash"
  StationsCoords = transform(StationsCoords, latlng = gh_decode(StationsCoords$geohash))
  colnames(StationsCoords)[2:5] = c("lat", "long", "lat_err", "lng_err")    
  
  StationsMapBG = BulgariaMap + 
                 geom_point(data = StationsCoords, aes(x = long, y = lat), color = "white", size = 2) +
                 geom_point(data = StationsCoords, aes(x = long, y = lat), color = "yellow", size = 1)
  SofiaMapBG    = StationsMapBG +
                 geom_point(data = topo, aes(x = long, y = lat), color = "blue", size = 2) +
                 geom_point(data = topo, aes(x = long, y = lat), color = "red", size = 1)

  SofiaMap     =ggplot(data = topo) + 
                geom_point(aes(x = long, y = lat), color = "black", size = 6) +
                geom_point(aes(x = long, y = lat), color = "green", size = 4) +
                coord_fixed(1.3)
  
  SofiaStationsCoords = StationsCoords[((StationsCoords$lat+StationsCoords$lat_err) > min(topo$lat) & 
                                      (StationsCoords$lat-StationsCoords$lat_err) < max(topo$lat) &
                                      (StationsCoords$long+StationsCoords$lng_err) > min(topo$long) &
                                      (StationsCoords$long-StationsCoords$lng_err) < max(topo$long)), ]
  
  SofiaStationsMap = SofiaMap +
                     geom_point(data = SofiaStationsCoords, aes(x = long, y = lat), color = "#666666", size = 5.5) +
                     geom_point(data = SofiaStationsCoords, aes(x = long, y = lat), color = "#FFFF66", size = 3.5)
#Step6: Filter by space --------------------
  citSofia = cit_merged[cit_merged$geohash %in% SofiaStationsCoords$geohash,]
  
  