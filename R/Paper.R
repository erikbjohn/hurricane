library(spData)
library(spdep)
library(ggmap)
library(rgeos)
library(sp)
library(raster)
library(rgdal)
library(dismo)
library(geosphere)
library(XML)
library(tiff)
library(rtiff)
library(magick)
library(xlsx)
library(data.table)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LETS TRY ON OUR OWN NOW~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data for 1960~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Census Data\\lacd1960")


c60<-"lacd1960.tif"
Raster60<- raster(c60)
Raster60
#Plots the entire region 
plot(Raster60, breaks = c(0, 100, 150, 200), pch=20, cex=2, col='black')


#Projection file
crs(Raster60) <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 
#

  
#extract points from raster file

longlat60 <- rasterToPoints(Raster60)

colnames(longlat60)<-c("dat","Long","Lat")

#Get points at ceter of data

spts60 <- rasterToPoints(Raster60, spatial = TRUE)

llprj60 <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  #
 
llpts60 <- spTransform(spts60, CRS(llprj60))

print(llpts60)

Cen60<- abs(as.data.frame(llpts60))        #Took absolute value so as to not have to work with the minus sign on longitudes. The minus sign is to indicate that we are in the western hemesphere
colnames(Cen60)<-c("dat","Long","Lat")

CenEJ<-readRDS("lacd1960_centroids.rds")



#cOLLECT THE DATA IN EXCEL FORMAT

write.table(CenEJ, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen60, file="Cen_RDG.csv",sep=",",row.names=F)

write.table(longlat60, file="LongLat.csv",sep=",",row.names=F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data for 1970~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Census Data\\lacd1970")


c70<-"lacd1970.tif"
Raster70<- raster(c70)
Raster70
#Plots the entire region 
plot(Raster70, breaks = c(0, 100, 150, 200), pch=20, cex=2, col='black')


#Projection file
crs(Raster70) <-  "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#extract points from raster file

longlat70 <- rasterToPoints(Raster70)

colnames(longlat70)<-c("Long","Lat", "dat")

#Get points at ceter of data

spts70 <- rasterToPoints(Raster70, spatial = TRUE)

llprj70 <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts70 <- spTransform(spts70, CRS(llprj70))

print(llpts70)

Cen70<- abs(as.data.frame(llpts70))
colnames(Cen70)<-c("dat","Long","Lat")

#Cen<-readRDS("lacd1970_centroids.rds")



#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(Cen, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen70, file="Cen_RDG70.csv",sep=",",row.names=F)

write.table(longlat70, file="LongLat70.csv",sep=",",row.names=F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data for 1980~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Census Data\\lacd1980")


c80<-"lacd1980.tif"
Raster80<- raster(c80)
Raster80
#Plots the entire region 
plot(Raster80, breaks = c(0, 100, 150, 200), pch=20, cex=2, col='black')


#Projection file
crs(Raster80) <-  "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#extract points from raster file

longlat80 <- rasterToPoints(Raster80)

colnames(longlat80)<-c("Long","Lat", "dat")

#Get points at ceter of data

spts80 <- rasterToPoints(Raster80, spatial = TRUE)

llprj80 <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts80 <- spTransform(spts80, CRS(llprj80))

print(llpts80)

Cen80<- abs(as.data.frame(llpts80))
colnames(Cen80)<-c("dat","Long","Lat")

#Cen<-readRDS("lacd1980_centroids.rds")



#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(Cen, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen80, file="Cen_RDG80.csv",sep=",",row.names=F)

write.table(longlat80, file="LongLat80.csv",sep=",",row.names=F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data for 1990~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Census Data\\lacd1990")


c90<-"lacd1990.tif"
Raster90<- raster(c90)
Raster90
#Plots the entire region 
plot(Raster90, breaks = c(0, 100, 150, 200), pch=20, cex=2, col='black')


#Projection file
crs(Raster90) <-  "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#extract points from raster file

longlat90 <- rasterToPoints(Raster90)

colnames(longlat90)<-c("Long","Lat", "dat")

#Get points at ceter of data

spts90 <- rasterToPoints(Raster90, spatial = TRUE)

llprj90 <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts90 <- spTransform(spts90, CRS(llprj90))

print(llpts90)

Cen90<- abs(as.data.frame(llpts90))
colnames(Cen90)<-c("dat","Long","Lat")

#Cen<-readRDS("lacd1990_centroids.rds")



#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(Cen, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen90, file="Cen_RDG90.csv",sep=",",row.names=F)

write.table(longlat90, file="LongLat90.csv",sep=",",row.names=F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data for 2000~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Census Data\\lacd2000")


c00<-"lacd2000.tif"
Raster00<- raster(c00)
Raster00
#Plots the entire region 
plot(Raster00, breaks = c(0, 100, 150, 200), pch=20, cex=2, col='black')


#Projection file
crs(Raster00) <-  "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#extract points from raster file

longlat00 <- rasterToPoints(Raster00)

colnames(longlat00)<-c("Long","Lat", "dat")

#Get points at ceter of data

spts00 <- rasterToPoints(Raster00, spatial = TRUE)

llprj00 <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts00 <- spTransform(spts00, CRS(llprj00))

print(llpts00)

Cen00<- abs(as.data.frame(llpts00))
colnames(Cen00)<-c("dat","Long","Lat")

#Cen<-readRDS("lacd1900_centroids.rds")



#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(Cen, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen00, file="Cen_RDG00.csv",sep=",",row.names=F)

write.table(longlat00, file="LongLat00.csv",sep=",",row.names=F)



##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Call in hurricane data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################

hurr<-read.csv("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Hurricane data\\Extracted data.csv")

head(hurr)



###################~~~~~~~~~~~~~Creating a small dataset to test the distance thing~~~~~~~~~~~~~~~~~##################
mloc60<-abs(Cen60[,2:3])
hloc<-hurr[,(6:5)]


testh<-head(hloc)

testm<-head(mloc60)



##########################~~~~~~~~~~~~~~~~~~~~~~~~~~Data tabel format~~~~~~~~~~~~~~~~~~~~~~~~~~~##########################

Cen60 <-data.table::data.table(Cen60)
Cen70 <-data.table::data.table(Cen70)
Cen80 <-data.table::data.table(Cen80)
Cen90 <-data.table::data.table(Cen90)
Cen00 <-data.table::data.table(Cen00)

dt <- Cen60[dat != 0]
#^^^^^^^^^^^^^^^^^^^^^^^^~~~~~~~~~~~~~~~~~~~~~~~~~~Data tabel format~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^



#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~testing ID~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###############

n<-matrix(NA,6,2) #Creating empty matrix 

n<-testm <- #Placing testm in test matrix to make the longer marix with repeated entries
  
n1<-rbind(testm, n)

n1[, ID := .GRP, by = .(Long, Lat)] #adding unique id per location so as to test if they are actually unique

#Answer: YES
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###############



####################~~~~~~~~~~~~~~~~Create location ID for each long/lat combination~~~~~~~~~~~~~~~~~#################



Cen60[, ID := .GRP, by = .(Long, Lat)]

Cen70[, ID := .GRP, by = .(Long, Lat)]


Cen80[, ID := .GRP, by = .(Long, Lat)]


Cen90[, ID := .GRP, by = .(Long, Lat)]


Cen00[, ID := .GRP, by = .(Long, Lat)]







##############################~~~~~~~~~~~~~Calulate the distance to centroids~~~~~~~~~~~~~~~~~########################

#Distance from huricane 1 in 1960  after its first 6 hours to each centroid in that year's census 

ddis<-as.data.frame((distCosine(Cen60[,2:3], c(44.3, 13.3), r=6378137))/1000) #This finds the shortest distance between two points
                                                                               #In meters
ddis<-cbind(ddis, Cen60$dat, Cen60$ID)

colnames(ddis)<-c("distance(km)", "dat","ID")

ddis<-data.table::data.table(ddis)

ddis <- ddis[dat != 0]    #Removing all the zeros which would indicate no populaition. 


####################~~~~~~~~~~~~~~~~~~Attatching the location to the distance calculations~~~~~~~~~~~~~~~###################

Cen60<-Cen60[dat != 0]


ddis<-cbind(ddis, Cen60$Long,Cen60$Lat)
colnames(ddis)<-c("distance(m)", "dat", "ID", "Long","Lat")


