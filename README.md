# DataSocietyMonthlyChallenge
Monthly Challenge – Sofia Air – Solution - New!Bees

First we start off by importing data from the citizens - the data from year 2017 in the ```cit17``` variable and from year 2018 - in the ```cit18``` variable. Then we import the topographical data for Sofia and rename its columns for convenience.

```r
cit17 = read.csv("G:\\Misc\\documents\\stopansko\\masters\\1_semester\\Monthly Challenge\\Air Tube\\data_bg_2017.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
cit18 = read.csv("G:\\Misc\\documents\\stopansko\\masters\\1_semester\\Monthly Challenge\\Air Tube\\data_bg_2018.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
topo = read.csv("G:\\Misc\\documents\\stopansko\\masters\\1_semester\\Monthly Challenge\\TOPO-DATA\\sofia_topo.csv", na.strings=c("",".","NA"," "), stringsAsFactors = FALSE)
colnames(topo)[1:2] = c("lat", "long")
```

Then we take a quick peek into the data just to get an idea of how it looks like using the console.

```r
View(head(cit17, n=10))
```

|    | time                 | geohash     | P1 | P2 | temperature | humidity | pressure | 
|----|----------------------|-------------|----|----|-------------|----------|----------| 
| 1  | 2017-09-06T20:00:00Z | sx8d5r7wmxr | 9  | 8  | 14          | 55       | 0        | 
| 2  | 2017-09-06T20:00:00Z | sx8d6zjg5h8 | 9  | 8  | 0           | 0        | 0        | 
| 3  | 2017-09-06T20:00:00Z | sx8dk3k2wr6 | 8  | 7  | 15          | 52       | 92655    | 
| 4  | 2017-09-06T20:00:00Z | sx2rj28e0gs | 5  | 5  | 18          | 47       | 97448    | 
| 5  | 2017-09-06T20:00:00Z | sx82v69m2jt | 1  | 1  | 11          | 68       | 94823    | 
| 6  | 2017-09-06T20:00:00Z | sx86k506zr9 | 29 | 15 | 14          | 63       | 93785    | 
| 7  | 2017-09-06T20:00:00Z | sx8dem6pp1h | 15 | 11 | 14          | 54       | 94841    | 
| 8  | 2017-09-06T20:00:00Z | sxevg0mdhp9 | 12 | 10 | 19          | 67       | 101342   | 
| 9  | 2017-09-06T20:00:00Z | sxevg3e7n2w | 21 | 16 | 19          | 55       | 101163   | 
| 10 | 2017-09-06T20:00:00Z | sx8devjgkk2 | 14 | 13 | 18          | 10       |          | 

```r
View(head(cit18, n=10))
```

|    | time                 | geohash     | P1  | P2  | temperature | humidity | pressure | 
|----|----------------------|-------------|-----|-----|-------------|----------|----------| 
| 1  | 2018-01-01T00:00:00Z | sx3wvzu7f6h | 55  | 34  | 4           | 64       | 99161    | 
| 2  | 2018-01-01T00:00:00Z | sx3wypu7fdn | 103 | 51  | 9           | 53       | 99109    | 
| 3  | 2018-01-01T00:00:00Z | sx86yxxv72r | 265 | 130 | 0           | 78       | 94085    | 
| 4  | 2018-01-01T00:00:00Z | sx2qvybw9tb | 227 | 94  | -1          | 93       | 96814    | 
| 5  | 2018-01-01T00:00:00Z | sxdhz58xssf | 187 | 110 | 4           | 67       | 100114   | 
| 6  | 2018-01-01T00:00:00Z | sx99f4w2geb | 23  | 11  | 3           | 69       | 97352    | 
| 7  | 2018-01-01T00:00:00Z | sx3rq2qvw53 | 42  | 24  | 7           | 64       | 98556    | 
| 8  | 2018-01-01T00:00:00Z | sx8dfgvfgj8 | 371 | 208 | 4           | 66       | 95532    | 
| 9  | 2018-01-01T00:00:00Z | sx8dfvjgen8 | 493 | 223 | 1           | 75       | 95527    | 
| 10 | 2018-01-01T00:00:00Z | sx8df8vvdxj | 420 | 235 | 1           | 73       | 9515     | 

Seems like we have available information about the geohash of the station, the concentrations of PM10 (column P1) and PM2.5 (column P2) and also the temperature, humidity and pressure measured at a point in time (column time). Our goal is to predict the 24h average PM10 concentration at each station.

Next we check the type of each column.

```r
VarClass17 = data.frame(names(cit17))
VarClass17[,2] = rapply(cit17, class)
VarClass18 = data.frame(names(cit18))
VarClass18[,2] = rapply(cit18, class)
VarClassTopo = data.frame(names(topo))
VarClassTopo[,2] = rapply(topo, class)
```

Example (```VarClass17```):

|   | names.cit17 | V2        | 
|---|--------------|-----------| 
| 1 | time         | character | 
| 2 | geohash      | character | 
| 3 | P1           | integer   | 
| 4 | P2           | integer   | 
| 5 | temperature  | integer   | 
| 6 | humidity     | integer   | 
| 7 | pressure     | integer   | 

It seems like the type of the ```time``` column is not correct - it's ```character``` but it represents time.
