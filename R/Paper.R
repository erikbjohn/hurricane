rm(list = ls())

install.packages("spData",dependencies = TRUE)
install.packages("sp",dependencies = TRUE)
install.packages("raster", dependencies = TRUE)
install.packages("rgdal",dependencies = TRUE)
install.packages("rgeos",dependencies = TRUE)
install.packages("dismo",dependencies = TRUE)
install.packages("XML",dependencies = TRUE)
install.packages("ggmap",dependencies = TRUE)
install.packages("spdep",dependencies = TRUE)
install.packages("tiff", dependencies = TRUE)
install.packages("rtiff", dependencies = TRUE)
install.packages("magick", dependencies = TRUE)
install.packages("xlsx", dependencies = TRUE)
install.packages("geosphere", dependencies = TRUE)
install.packages("lmap",dependencies = TRUE)
install.packages('data.table', dependencies = TRUE) #Dr. Johnson says this is better than data.frame
install.packages("RColorBrewer", dependencies = TRUE)
install.packages('rasterVis', dependencies = TRUE)
install.packages("maps", dependencies = TRUE)
install.packages("maptools", dependencies = TRUE)
install.packages("rworldmap", dependencies = TRUE)
install.packages("mapdata",dependencies = TRUE)
install.packages("geonames", dependencies = TRUE)

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
#library(xlsx)
library(data.table)
library(RColorBrewer)
library(rasterVis)
library(maps)
library(maptools)
library(rworldmap)
library(geonames)
library(mapdata)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LETS TRY ON OUR OWN NOW~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data for 1960~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Census Data\\lacd1960")


c60<-"lacd1960.tif"
Raster60<- raster(c60)
Raster60
#Plots the entire region 
plot(Raster60, breaks = c(1, 100, 150, 200), pch=20, cex=2, col='black')



#Projection file
crs(Raster60) <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 



#extract points from raster file

longlat60 <- rasterToPoints(Raster60)


colnames(longlat60)<-c("dat","Long","Lat")

#Get points at ceter of data

spts60 <- rasterToPoints(Raster60, spatial = TRUE)

llprj60 <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "# "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs" #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # 
# #

llpts60 <- spTransform(spts60, CRS(llprj60))


print(llpts60)

Cen60<- abs(as.data.frame(llpts60))        #Took absolute value so as to not have to work with the minus sign on longitudes. The minus sign is to indicate that we are in the western hemesphere
colnames(Cen60)<-c("dat","Long","Lat")


##########################~~~~~~~~~~~~~~~~~~~~~~~~~~Data tabel format~~~~~~~~~~~~~~~~~~~~~~~~~~~##########################

Cen60 <-data.table::data.table(Cen60)

#Create location ID for each long/lat combination

Cen60[, ID := .GRP, by = .(Long, Lat)]
######################################################################################################################

CenEJ<-readRDS("lacd1960_centroids.rds")



#cOLLECT THE DATA IN EXCEL FORMAT

write.table(CenEJ, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen60, file="Cen_RDG.csv",sep=",",row.names=F)

write.table(longlat60, file="LongLat.csv",sep=",",row.names=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Cropped 1960 data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Lets crop the map to only include the Caribbean and central america

Raster60_crop<-crop(Raster60, extent(-119, -5, 0, 35))
plot(Raster60_crop, breaks = c(1, 100, 150, 200), pch=20, cex=2, col='black')



#Projection file
crs(Raster60_crop) <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 




#extract points from raster file

longlat60_crop <- rasterToPoints(Raster60_crop)


colnames(longlat60_crop)<-c("Long","Lat","dat")

#Get points at ceter of data

spts60_crop <- rasterToPoints(Raster60_crop, spatial = TRUE)

llprj60_crop <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "# "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs" #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # 
# #

llpts60_crop <- spTransform(spts60_crop, CRS(llprj60_crop))


print(llpts60_crop)

Cen60_crop<- abs(as.data.frame(llpts60_crop))        #Took absolute value so as to not have to work with the minus sign on longitudes. The minus sign is to indicate that we are in the western hemesphere
colnames(Cen60_crop)<-c("dat","Long","Lat")

#buff60<-gBuffer(llpts60_crop, width = 10)
#gBuffer(llpts60_crop, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)


#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(CenEJ, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen60_crop, file="Cen_RDG.csv",sep=",",row.names=F)

write.table(longlat60_crop, file="LongLat.csv",sep=",",row.names=F)



Cen60_crop <-data.table::data.table(Cen60_crop)
Cen60_crop[, ID := .GRP, by = .(Long, Lat)]

Cen60_crop<-Cen60_crop[dat != 0] #Remove locations where no population exist 
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Call in hurricane data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################

setwd("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material")
source('R/get_hurr.R')
hurr <- get_hurr()



##############################~~~~~~~~~~Correct panel format of dataset~~~~~~########################################

hurr$Year<-sub("^$", "NA", hurr$Year) #Replace empty cells with NA

for (row in 2:nrow(hurr)){ # 2 so you don't affect column names
  if(is.na(hurr$Year[row]) ) {    # if its empty...
    hurr$Year[row] = hurr$Year[row-1] # ...replace with previous row's value
  }
}



#~~~~~~~~~~Split year and hurricane number~~~~~~~~~~#


ysplit<-matrix(unlist(strsplit( hurr$Year , "\\." ) ),ncol=2,byrow = TRUE)

hurr$Year<- ysplit[,1]

hurr$Hurricane <- ysplit[,2]

#REMOVE YEAR2 AND COUNTRY and rename v13
hurr$category<-hurr$V13

hurr$Country<-NULL

hurr$V13<-NULL

head(hurr)


###########################~~~~~~~~~~~~~~~~~~~~~Linear interpolation of data~~~~~~~~~~~~~~~~~~~~~~##########################

# setwd("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material")
# source('R/get_hurr_in.R')
# hurr_in <- get_hurr_in()

######################################################
#When I used the source command to call in the data 
#The ID numbers kept altering the number of hurricanes 
#That occurred in the years. Specifically, instead of 
#recording say for 1961, 7 hurricanes, it skips hurricane 
#6 for no appaent reason. So I will run the codes outside 
#of the source function.
######################################################

###########################~~~~~~~~~~~~~~~~~~~~~Linear interpolation of data~~~~~~~~~~~~~~~~~~~~~~##########################

setwd("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Hurricane data")

na.df <- data.frame(num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, let = NA, num = NA, let = NA, num = NA, let = NA)

hurr_in<-do.call(rbind, apply(hurr, 1, function(x) {rbind(x, na.df)}))
colnames(hurr_in)<-colnames(hurr)



#Fill empty cells with linearly interolated values

#hurr_in$Year<-sub("NULL", "NA", hurr_in$Year) #Replace empty cells with NA

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>YEAR<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


for (row in 2:nrow(hurr_in["Year"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Year"])) {    # if its empty...
    hurr_in[row,"Year"] <- hurr_in[row-1,"Year"]
  }
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Month<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


for (row in 2:nrow(hurr_in["Mo.th"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Mo.th"])) {    # if its empty...
    hurr_in[row,"Mo.th"] <- hurr_in[row-1,"Mo.th"]
  }
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>day<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Day"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Day"])) {    # if its empty...
    hurr_in[row,"Day"] <- hurr_in[row-1,"Day"]
  }
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Category<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["category"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"category"])) {    # if its empty...
    hurr_in[row,"category"] <- hurr_in[(row-1),"category"]
  }
}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>3h interval<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["6h interval UTC"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"6h interval UTC"])) {    # if its empty...
    hurr_in[row,"6h interval UTC"] <- 3 + as.numeric(hurr_in[row-1,"6h interval UTC"])
  }
}




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>hurricane<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Hurricane"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Hurricane"])) {    # if its empty...
    hurr_in[row,"Hurricane"] <- hurr_in[row-1,"Hurricane"]
  }
}




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Latitudes<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Lat.N"])){ # 2 so you don't affect column names

  
  
   if(is.na(hurr_in[row,"Lat.N"])) {    # if its empty...
    hurr_in[row,"Lat.N"] <- as.numeric((as.numeric(hurr_in[row-1,"Lat.N"]) + as.numeric(hurr_in[row+1,"Lat.N"]))/2)
    
 
}

}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Longitudes<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Long.W"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Long.W"])) {    # if its empty...
    hurr_in[row,"Long.W"] <- as.numeric((as.numeric(hurr_in[row-1,"Long.W"]) + as.numeric(hurr_in[row+1,"Long.W"]))/2)
  }
}



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Degrees<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Direction in degrees"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Direction in degrees"])) {    # if its empty...
    hurr_in[row,"Direction in degrees"] <- (as.numeric(hurr_in[row-1,"Direction in degrees"]) + as.numeric(hurr_in[row+1,"Direction in degrees"]))/2
  }
}




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Speed in mph<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Speed in MPH"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Speed in MPH"])) {    # if its empty...
    hurr_in[row,"Speed in MPH"] <- (as.numeric(hurr_in[row-1,"Speed in MPH"]) + as.numeric(hurr_in[row+1,"Speed in MPH"]))/2
  }
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Speed in kmph<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Speed in KPH"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Speed in KPH"])) {    # if its empty...
    hurr_in[row,"Speed in KPH"] <- (as.numeric(hurr_in[row-1,"Speed in KPH"]) + as.numeric(hurr_in[row+1,"Speed in KPH"]))/2
  }
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Wind in MPH<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Wind Speed in MPH"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Wind Speed in MPH"])) {    # if its empty...
    hurr_in[row,"Wind Speed in MPH"] <- (as.numeric(hurr_in[row-1,"Wind Speed in MPH"]) + as.numeric(hurr_in[row+1,"Wind Speed in MPH"]))/2
  }
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>Wind in kmph<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (row in 2:nrow(hurr_in["Wind Speed in KPH"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"Wind Speed in KPH"])) {    # if its empty...
    hurr_in[row,"Wind Speed in KPH"] <- (as.numeric(hurr_in[row-1,"Wind Speed in KPH"]) + as.numeric(hurr_in[row+1,"Wind Speed in KPH"]))/2
  }
}




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Fix ID<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
for (row in 2:nrow(hurr_in["hurr_id"])){ # 2 so you don't affect column names
  if(is.na(hurr_in[row,"hurr_id"])) {    # if its empty...
    hurr_in[row,"hurr_id"] <- (1 + as.numeric(hurr_in[row-1,"hurr_id"]))
  }
}



##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Create csv file for hurr_in~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################


write.table(hurr_in, file="hurr_in.csv",sep=",",row.names=F)

hurr_in <-fread("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Hurricane data\\hurr_in.csv")
hurr_in  <- hurr_in [, Lat.N := as.numeric(stringr::str_replace(Lat.N, '\\.$', ''))]
hurr_in  <- hurr_in [, Long.W := as.numeric(stringr::str_replace(Long.W, '\\.$', ''))]
hurr_in$hurr_id <- 1:nrow(hurr_in )

############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###############


#Drop last row of data. This was restricting the distance calculations

hurr_in <- hurr_in[-nrow(hurr_in),] 


##############Distance with cropped file############################


#Distance from huricanes in 1960 for  each centroid in that year's census 

dt <- data.table()
dts_crop60 <- data.table()
for(iRow in 1:nrow(hurr_in)){
  if((iRow %% 20)==0){
    cat(iRow, 'of', nrow(hurr_in), '\n')
  }
  hurr_lat<-hurr_in[iRow]$Lat.N
  hurr_long<-hurr_in[iRow]$Long.W
  hurr_id <- hurr_in[iRow]$hurr_id
  ddis_crop<-(distCosine(Cen60_crop[,2:3], c(hurr_long, hurr_lat), r=6378137))/1000 #This finds the shortest distance between two points
  #In meters
  dt <- data.table(hurr_id = hurr_id, cen60_id = Cen60_crop$ID, ddis_crop )
  dt <- dt[ddis_crop<=500]
  dts_crop60 <- rbindlist(list(dts_crop60, dt), use.names = TRUE, fill=TRUE)
}
#colnames(ddis)<-c("distance(km)", "dat","ID")

ddis_crop<-data.table::data.table(ddis_crop)


#####################################
#Calculations are faster since the
#program doesn't have to check
#locations that are far outside of the 
#area of interest such as the southern 
#tip of SOuth America
######################################


#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Draw polygon for all hurricane~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#########

cropset<-merge(dts_crop60, hurr_in, by = "hurr_id", all=T)



#Drop variables that I dont need in the dataset

cropset$Mo.th<-NULL
cropset$Day<-NULL
cropset$`6h interval UTC`<-NULL
cropset$`Direction in degrees`<-NULL
cropset$`Speed in MPH`<-NULL
cropset$`Speed in KPH`<-NULL
cropset$`Wind Speed in MPH`<-NULL
cropset$`Wind Speed in KPH`<-NULL
cropset$`Pressure in Millibars`<-NULL





#cropset<-cropset[-c(is.na(cropset$cen60_id)),]

complete.cases(cropset)
cropset_full<-na.omit(cropset)
#cropset_full<-as.data.table(cropset_full)




#merge by centroid ID to plot better
colnames(cropset_full)[2]<- "ID"

cropset_full<-merge(cropset_full, Cen60_crop, by = "ID", all=T)


complete.cases(cropset_full)
cropset_full<-na.omit(cropset_full) #delete NA entries



####################################################
#The dataset is organized by centroid and year.
#So for each centroid which looks like its in 
#the order of country, it shows each year it was hit.

#For each centroid the data begins at the earliest hit
# and runs to the last hit then we change centroid and 
#restart the process.
# My challenge now is to organize the data so I can 
#identify the countries themselves from the coordinates.
######################################################


#############################################Get country names into the dataset######################################


coords2country = function(points)
{
  # prepare a SpatialPolygons object with one poly per country
  countries = map('worldHires', fill=TRUE, col="transparent", plot=FALSE)
  names = sapply(strsplit(countries$names, ":"), function(x) x[1])
  
  
  #clean up polygons that are out of bounds
  filter = countries$x < -180 & !is.na(countries$x)
  countries$x[filter] = -180
  
  filter = countries$x > 180 & !is.na(countries$x)
  countries$x[filter] = 180
  
  countriesSP <- getMap(resolution='high')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  #gBuffer(pointsSP, width=1000)
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
}


points=data.frame(-cropset_full$Long,cropset_full$Lat)

# plot them on a map
map("worldHires", xlim=c(-119, -40), ylim=c(0, 30))
points(points$X.cropset_full.Long, points$cropset_full.Lat, col="Red")

# get a list of country names
nam<-as.data.frame(coords2country(points), stringsAsFactors=FALSE)

#Change Frnace to Guadalupe
nam$`coords2country(points)`<- gsub('France', 'Guadalupe', nam$`coords2country(points)`)


cropset_full<-cbind(cropset_full,nam)



#########################Check those locations that produce NA entries############################


plot(getMap()) 
points(-76.85417, 17.97917,col='green')





########################################Collect data by country######################################################
mexico<-data.table()

mexico<-cropset_full[which(cropset_full$`coords2country(points)`=='Mexico')]








######################################Using world map to plot hurricanes in the Caribbean######################





coords2country = function(points_hur)
{
  # prepare a SpatialPolygons object with one poly per country
  countries = map('worldHires', fill=TRUE, col="transparent", plot=FALSE)
  names = sapply(strsplit(countries$names, ":"), function(x) x[1])
  
  
  #clean up polygons that are out of bounds
  filter = countries$x < -180 & !is.na(countries$x)
  countries$x[filter] = -180
  
  filter = countries$x > 180 & !is.na(countries$x)
  countries$x[filter] = 180
  
  countriesSP <- getMap(resolution='high')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  pointsSP = SpatialPoints(points_hur, proj4string=CRS(" +proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points_hur, proj4string=CRS(proj4string(countriesSP)))  
  
  #gBuffer(pointsSP, width=1000)
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
}




points_hur=data.frame(-hurr_in$Long.W[1999:2017],hurr_in$Lat.N[1999:2017])

# plot them on a map
map("worldHires", xlim=c(-119, -40), ylim=c(0, 30))
points(points_hur$X.hurr_in.Long.W.1999.2017., points_hur$hurr_in.Lat.N.1999.2017., col="Red")
points(-hurr_in$Long.W[2019:2051], hurr_in$Lat.N[2019:2051], col="blue")
points(-hurr_in$Long.W[2053:2069], hurr_in$Lat.N[2053:2069], col="green")
points(-hurr_in$Long.W[2073:2097], hurr_in$Lat.N[2073:2097], col="purple")
points(-hurr_in$Long.W[2099:2135], hurr_in$Lat.N[2099:2135], col="black")




#############################################
#So this worked. What I need now is to find a
#a way to map all the hurricanes by year
#and then we can disaggrigate it by the countries
#individually.
############################################








#################################################Subsetting data by year############################################
# 
#hurr60<-subset(hurr_in, Year == 1960)
# 
# 
# 
# #Lets get some distances for that year
# 
# dt <- data.table()
# dtstry<- data.table()
# for(iRow in 1:nrow(hurr60)){
#   if((iRow %% 20)==0){
#     cat(iRow, 'of', nrow(hurr60), '\n')
#   }
#   hurr_lat <- hurr60[iRow]$Lat.N
#   hurr_long <- hurr60[iRow]$Long.W
#   hurr_id <- hurr60[iRow]$hurr_id
#   ddistry<- (distCosine(Cen60[,2:3], c(hurr_long, hurr_lat), r=6378137))/1000 #This finds the shortest distance between two points
#   #In meters
#   dt <- data.table(hurr_id = hurr_id, cen60_id = Cen60$ID, ddistry )
#   dt <- dt[ddistry<=500]
#   dtstry<- rbindlist(list(dtstry, dt), use.names = TRUE, fill=TRUE)
# }
# 
# colnames(dtstry)<-c("Hurr_ID","ID","distance(km)")
# 
# ddistry<-data.table::data.table(ddistry)
# 
# 
# 
# 
# 
# 
# hurr61<-subset(hurr_in, Year == 1961)
# hurr62<-subset(hurr_in, Year == 1962)
# hurr63<-subset(hurr_in, Year == 1963)
# hurr64<-subset(hurr_in, Year == 1964)
####################################################################################################################



####################################Using shapefile from puerto rico#################################################


prico <- system.file("external/tl_2016_72_cousub.shp", package="raster")
prico


shape_prico <- shapefile(prico)
shape_prico
n<-length(shape_prico)


crs(shape_prico)



plot(shape_prico)#, col=rainbow(n))

#Get raster points and centrid locations


shape_prico<-raster(shape_prico)




raspoint<-rasterToPoints(shape_prico, spatial = TRUE)


buf_file<-spTransform(raspoint, CRS(proj_PR))
plot(buf_file, col=rainbow(n))

#Trying to understand what gBuffer is and what it does actually

buff_pr<-gBuffer(buf_file, width=1000, byid = TRUE, quadsegs = 5, capStyle="ROUND",joinStyle="ROUND")

plot(buff_pr)

bufpoints<-as.data.frame(buf_file)


raspoint<-as.data.frame(raspoint)

colnames(raspoint)<-c("Long","Lat")

raspoint<-abs(data.table::data.table(raspoint))

raspoint[, ID := .GRP, by = .(Long, Lat)]


#Test to see if points are in Puerto Rico only and not in the water#

coords2country = function(points)
{
  # prepare a SpatialPolygons object with one poly per country
  countries = map('worldHires', fill=TRUE, col="transparent", plot=FALSE)
  names = sapply(strsplit(countries$names, ":"), function(x) x[1])
  
  
  #clean up polygons that are out of bounds
  filter = countries$x < -180 & !is.na(countries$x)
  countries$x[filter] = -180
  
  filter = countries$x > 180 & !is.na(countries$x)
  countries$x[filter] = 180
  
  countriesSP <- getMap(resolution='high')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  #gBuffer(pointsSP, width=1000)
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
}


points=data.frame(-raspoint$Long,raspoint$Lat)

# plot them on a map
map("worldHires", xlim=c(-68, -65), ylim=c(5, 30))
points(points$X.raspoint.Long, points$raspoint.Lat, col="Red")

# get a list of country names
nam<-as.data.frame(coords2country(points), stringsAsFactors=FALSE)

raspoint<-cbind(raspoint,nam)






##############~~~~~~~~~~~~~~~~~~~~~~Do distance calculations~~~~~~~~~~~~~~~~#####################
##########################################
#Had trouble at first with the calculations
#But the problem was that I  didnt take the 
#absolute values of the longs that come in 
#negative terms.



#Now im goig to try to match the hurricane ID with the years which will give us better information.
#Might need to group it in some way so that the year come with the hurricane then I can do the summary stuff.


dt <- data.table()
dts_PR<- data.table()
for(iRow in 1:nrow(hurr_in)){
  if((iRow %% 20)==0){
    cat(iRow, 'of', nrow(hurr_in), '\n')
  }
  hurr_lat <- hurr_in[iRow]$Lat.N
  hurr_long <- hurr_in[iRow]$Long.W
  hurr_id <- hurr_in[iRow]$hurr_id
  ddisPR<- (distCosine(raspoint[,1:2], c(hurr_long, hurr_lat), r=6378137))/1000 #This finds the shortest distance between two points
  #In meters
  dt <- data.table(hurr_id = hurr_id , PR_id = raspoint$ID, ddisPR )
  dt <- dt[ddisPR<=500]
  dts_PR<- rbindlist(list(dts_PR, dt), use.names = TRUE, fill=TRUE)
}

#colnames(dts_PR)<-c("Hurr_ID","ID","distance(km)")

ddisPR<-data.table::data.table(ddisPR)


#Merge distance for Puerto Rico with hurricane by nurricane ID and then drop the extra stuff


mPR<-merge(dts_PR, hurr_in, by = "hurr_id", all=T)





#Drop variables that I dont need in the dataset

mPR<-mPR[-c(is.na(mPR$PR_id)),]

complete.cases(mPR)
merg_PR<-na.omit(mPR)

merg_PR$Mo.th<-NULL
merg_PR$Day<-NULL
merg_PR$`6h interval UTC`<-NULL
merg_PR$`Direction in degrees`<-NULL
merg_PR$`Speed in MPH`<-NULL
merg_PR$`Speed in KPH`<-NULL
merg_PR$`Wind Speed in MPH`<-NULL
merg_PR$`Wind Speed in KPH`<-NULL
merg_PR$`Pressure in Millibars`<-NULL


#merge by centroid ID to plot better
colnames(merg_PR)[2]<- "ID"

pmerg<-merge(merg_PR, raspoint, by = "ID", all=T)








#Plot points for 1960

PR60<-cbind(-pmerg$Long, pmerg$Lat, pmerg$ID, pmerg$Year=="1960")

colnames(PR60)<-c("Long","Lat","ID", "1960")


PR60<-PR60[!(apply(PR60, 1, function(y) any(y == 0))),]


map<-plot(shape_prico)#,  col = rainbow(n))
points(map, centr[[1]], centr[[2]], pch=0, cex = .6, col=rainbow(n))





#IT PLOTS

plot(PR60[,1], PR60[,2], xlim=c(-65,-68), ylim=c(17,19))


#using spatial points from puerto rico to show the hurricanes that have made their way close to or 
#affected the Island over the 50 years of interest.







############################Use unique value in dataset to find number of hurricanes################################

num_hurr<- aggregate(data.frame(count = merg_PR$hurr_id), list(value = merg_PR$hurr_id), length)

#Turned out to be 137 hurricanes for puerto rico

#number of hurricanes for the region on a whole between 1960 and 2010 that were of damageing proportion 

num_hurrfull<- aggregate(data.frame(count = dts$hurr_id), list(value = dts$hurr_id), length)


#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Draw polygon for Puerto Rico hurricane~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#########

hur_cor_PR<-data.frame(cbind(merg_PR$Long.W[1:787], merg_PR$Lat.N[1:787]))   #For one hurricane
colnames(hur_cor_PR)<-c("long", "lat")


my.first.points <- SpatialPoints(hur_cor_PR)  # ..converted into a spatial object
plot(my.first.points)

summary(my.first.points)




crs.geo <- CRS(" +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 ")  # UTM 33N
proj4string(my.first.points) <- crs.geo  # define projection system of our data
is.projected(my.first.points)

#project file

proj_PR<- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 

hurr_points<- spTransform(my.first.points, CRS(proj_PR))

summary(hurr_points)

plot(hurr_points, pch=20, cex=2, col='red')

hurr_points<-raster(hurr_points)





