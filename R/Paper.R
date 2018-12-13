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
install.packages("lubridate", dependencies=TRUE)
install.packages("readr", dependencies=TRUE)
install.packages("‘HURDAT", dependencies = TRUE)
#install.packages("C:/Users/goulb/Downloads/HURDAT_0.2.0.tar.gz", repos = NULL, type = "source")
install.packages("stringr", dependencies = TRUE)

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
library(lubridate)
library(readr)
library(HURDAT)
library(stringr)


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
plot(Raster60_crop, breaks = c(1.5, 100, 150, 200), pch=20, cex=2, col='black')



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



#buff60<-buffer(llpts60_crop, width=0, dissolve=FALSE)#st_buffer(llpts60_crop, 10)
#gBuffer(llpts60_crop, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)


#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(CenEJ, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen60_crop, file="Cen_RDG.csv",sep=",",row.names=F)

write.table(longlat60_crop, file="LongLat.csv",sep=",",row.names=F)



Cen60_crop <-data.table::data.table(Cen60_crop)
Cen60_crop[, ID := .GRP, by = .(Long, Lat)]

Cen60_crop<-Cen60_crop[dat != 0] #Remove locations where no population exist 




#>>>>>>>>>>>>>>>>>>>>>>>>>>>Using 1990 because the documentation says that 1990 onwards<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>Had more precise measurements so it is anticipated that the <<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>Results will be better in trying to name the locations<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data for 1990~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Census Data\\lacd1990")


c90<-"lacd1990.tif"
Raster90<- raster(c90)
Raster90
#Plots the entire region 
plot(Raster90, breaks = c(1, 100, 150, 200), pch=20, cex=2, col='black')



#Projection file
crs(Raster90) <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 



#extract points from raster file

longlat90 <- rasterToPoints(Raster90)


colnames(longlat90)<-c("dat","Long","Lat")

#Get points at ceter of data

spts90 <- rasterToPoints(Raster90, spatial = TRUE)

llprj90 <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "# "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs" #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # 
# #

llpts90 <- spTransform(spts90, CRS(llprj90))


print(llpts90)

Cen90<- abs(as.data.frame(llpts90))        #Took absolute value so as to not have to work with the minus sign on longitudes. The minus sign is to indicate that we are in the western hemesphere
colnames(Cen90)<-c("dat","Long","Lat")


##########################~~~~~~~~~~~~~~~~~~~~~~~~~~Data tabel format~~~~~~~~~~~~~~~~~~~~~~~~~~~##########################

Cen90 <-data.table::data.table(Cen90)

#Create location ID for each long/lat combination

Cen90[, ID := .GRP, by = .(Long, Lat)]
######################################################################################################################

#CenEJ<-readRDS("lacd1990_centroids.rds")



#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(CenEJ, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen90, file="Cen_RDG.csv",sep=",",row.names=F)

write.table(longlat90, file="LongLat.csv",sep=",",row.names=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Cropped 1990 data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Lets crop the map to only include the Caribbean and central america

Raster90_crop<-crop(Raster90, extent(-119, -5, 0, 35))
plot(Raster90_crop, breaks = c(1.5, 100, 150, 200), pch=20, cex=2, col='black')



#Projection file
crs(Raster90_crop) <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs " 
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" 




#extract points from raster file

longlat90_crop <- rasterToPoints(Raster90_crop)


colnames(longlat90_crop)<-c("Long","Lat","dat")

#Get points at ceter of data

spts90_crop <- rasterToPoints(Raster90_crop, spatial = TRUE)

llprj90_crop <- "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "# "+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs" #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"  # 
# #

llpts90_crop <- spTransform(spts90_crop, CRS(llprj90_crop))


print(llpts90_crop)

Cen90_crop<- abs(as.data.frame(llpts90_crop))        #Took absolute value so as to not have to work with the minus sign on longitudes. The minus sign is to indicate that we are in the western hemesphere
colnames(Cen90_crop)<-c("dat","Long","Lat")



#buff90<-buffer(llpts90_crop, width=0, dissolve=FALSE)#st_buffer(llpts90_crop, 10)
#gBuffer(llpts90_crop, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)


#cOLLECT THE DATA IN EXCEL FORMAT

#write.table(CenEJ, file="cen_EJ.csv",sep=",",row.names=F)

write.table(Cen90_crop, file="Cen_RDG.csv",sep=",",row.names=F)

write.table(longlat90_crop, file="LongLat.csv",sep=",",row.names=F)



Cen90_crop <-data.table::data.table(Cen90_crop)
Cen90_crop[, ID := .GRP, by = .(Long, Lat)]

Cen90_crop<-Cen90_crop[dat != 0] #Remove locations where no population exist









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



#Do the same with Category
catsplit<-matrix(unlist(strsplit( hurr$category , "\\("), ncol=2,byrow = TRUE))



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

na.df <- data.frame(num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA)


##################################Splitting by key###################
hurr <- as.data.table(hurr)
hurr<- hurr[, hurr_key:=paste0(Year, '_', Hurricane)]
hurr_list<-list()
hurr_list<-split(hurr, hurr$hurr_key)
View(hurr_list)
####################################################################



hur1960<-as.data.frame(hurr_list$`1960_1`)

for (i in 1:length(hurr_list)){
  #Create space for interpolation
  hurr_list[[i]]<-do.call(rbind, apply(hurr_list[[i]], 1, function(x) {rbind(x, na.df)}))
  
  #Rename variables
  colnames(hurr_list[[i]])<-colnames(hur1960)
  
  #Drop last row of each dataset
  hurr_list[[i]]<-hurr_list[[i]][-nrow(hurr_list[[i]]),] 
  
  
  
}




for(count in 1:length(hurr_list)){
  
  if(length(hurr_list[[count]]$Year)>2) { 
    
    #Interpolate for Year
    for(row in 2:length(hurr_list[[count]]$Year)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$Year[row])) {    # if its empty...
        hurr_list[[count]]$Year[row]<-hurr_list[[count]]$Year[row+1]
        
      }
      
    } 
    
    
    #Interpolate for month
    for (row in 2:length(hurr_list[[count]]$Mo.th)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$Mo.th[row])) {    # if its empty...
        hurr_list[[count]]$Mo.th[row] <- hurr_list[[count]]$Mo.th[row-1]
      }
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>day<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$Day)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$Day[row])) {    # if its empty...
        hurr_list[[count]]$Day[row] <-hurr_list[[count]]$Day[row-1]
      }
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Category<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$category)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$category[row])) {    # if its empty...
        hurr_list[[count]]$category[row] <- hurr_list[[count]]$category[row-1]
      }
    }
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>3h interval<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$'6h interval UTC')){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$'6h interval UTC'[row])) {    # if its empty...
        hurr_list[[count]]$'6h interval UTC'[row]<- 3 + as.numeric(hurr_list[[count]]$'6h interval UTC'[row-1])
      }
    }
    
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>hurricane<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$Hurricane)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$Hurricane[row])) {    # if its empty...
        hurr_list[[count]]$Hurricane[row] <- hurr_list[[count]]$Hurricane[row-1]
      }
    }
    
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Latitudes<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$Lat.N)){ # 2 so you don't affect column names
      
      if(is.na(hurr_list[[count]]$Lat.N[row])) {    # if its empty...
        hurr_list[[count]]$Lat.N[row] <- as.numeric((as.numeric(hurr_list[[count]]$Lat.N[row-1]) + as.numeric(hurr_list[[count]]$Lat.N[row+1]))/2)
        
        
      }
      
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Longitudes<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$Long.W)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$Long.W[row])) {    # if its empty...
        hurr_list[[count]]$Long.W[row]<- as.numeric((as.numeric(hurr_list[[count]]$Long.W[row-1]) + as.numeric(hurr_list[[count]]$Long.W[row+1]))/2)
      }
    }
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Degrees<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$'Direction in degrees')){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$'Direction in degrees'[row])) {    # if its empty...
        hurr_list[[count]]$'Direction in degrees'[row] <- (as.numeric(hurr_list[[count]]$'Direction in degrees'[row-1]) + as.numeric(hurr_list[[count]]$'Direction in degrees'[row+1]))/2
      }
    }
    
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Speed in mph<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$'Speed in MPH')){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$'Speed in MPH'[row])) {    # if its empty...
        hurr_list[[count]]$'Speed in MPH'[row] <- (as.numeric(hurr_list[[count]]$'Speed in MPH'[row-1]) + as.numeric(hurr_list[[count]]$'Speed in MPH'[row+1]))/2
      }
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Speed in kmph<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$'Speed in KPH')){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$'Speed in KPH'[row])) {    # if its empty...
        hurr_list[[count]]$'Speed in KPH'[row] <- (as.numeric(hurr_list[[count]]$'Speed in KPH'[row-1]) + as.numeric(hurr_list[[count]]$'Speed in KPH'[row+1]))/2
      }
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Wind in MPH<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$'Wind Speed in MPH')){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$'Wind Speed in MPH'[row])) {    # if its empty...
        hurr_list[[count]]$'Wind Speed in MPH'[row] <- (as.numeric(hurr_list[[count]]$'Wind Speed in MPH'[row-1]) + as.numeric(hurr_list[[count]]$'Wind Speed in MPH'[row+1]))/2
      }
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Wind in kmph<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$'Wind Speed in KPH')){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$'Wind Speed in KPH'[row])) {    # if its empty...
        hurr_list[[count]]$'Wind Speed in KPH'[row] <- (as.numeric(hurr_list[[count]]$'Wind Speed in KPH'[row-1]) + as.numeric(hurr_list[[count]]$'Wind Speed in KPH'[row+1]))/2
      }
    }
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Fix ID<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    
    
    for (row in 2:length(hurr_list[[count]]$hurr_id)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$hurr_id[row])) {    # if its empty...
        hurr_list[[count]]$hurr_id[row] <- 1+ as.numeric(hurr_list[[count]]$hurr_id[row-1] )
      }
      
      else{
        
        
        hurr_list[[count]]$hurr_id[row] <- 1+ as.numeric(hurr_list[[count]]$hurr_id[row-1] )}
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>hurricane key<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(hurr_list[[count]]$hurr_key)){ # 2 so you don't affect column names
      if(is.na(hurr_list[[count]]$hurr_key[row])) {    # if its empty...
        hurr_list[[count]]$hurr_key[row] <- hurr_list[[count]]$hurr_key[row-1]
      }
    }  
    
    
    
    
  }
}

#############################Rbind all the elements of our list to create one solid dataset#######################
hurr_interpolated<-rbindlist(hurr_list, use.names=TRUE)






##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Create csv file for hurr_in~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################


write.table(hurr_interpolated, file="hurr_interpolated.csv",sep=",",row.names=F)

hurr_interpolated <-fread("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Hurricane data\\hurr_interpolated.csv")
hurr_interpolated <- hurr_interpolated[, Lat.N := as.numeric(stringr::str_replace(Lat.N, '\\.$', ''))]
hurr_interpolated <- hurr_interpolated[, Long.W := as.numeric(stringr::str_replace(Long.W, '\\.$', ''))]
hurr_interpolated$hurr_id<-1:nrow(hurr_interpolated )

############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###############



##############Distance with cropped file############################


#Distance from huricanes in 1960 for  each centroid in that year's census 

dt <- data.table()
dts_crop60 <- data.table()
for(iRow in 1:nrow(hurr_interpolated)){
  if((iRow %% 20)==0){
    cat(iRow, 'of', nrow(hurr_interpolated), '\n')
  }
  hurr_lat<-hurr_interpolated[iRow]$Lat.N
  hurr_long<-hurr_interpolated[iRow]$Long.W
  hurr_id <- hurr_interpolated[iRow]$hurr_id
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

cropset<-merge(dts_crop60, hurr_interpolated, by = "hurr_id", all=T)



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
  
  pointsSP = SpatialPoints(points, proj4string=CRS("+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "))
  
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
vec_names <- coords2country(points)
nam <- data.table:data.table(vec_names)
nam<-as.data.frame(coords2country(points), stringsAsFactors=FALSE)

#Change Frnace to Guadalupe
nam$`coords2country(points)`<- gsub('France', 'Guadalupe', nam$`coords2country(points)`)
#Change Naval base to Cuba
nam$`coords2country(points)`<- gsub('US Naval Base Guantanamo Bay', 'Cuba', nam$`coords2country(points)`)


cropset_full<-cbind(cropset_full,nam)


options(geonamesUsername="rushaine")
source(system.file("tests","testing.R",package="geonames"),echo=TRUE)
GNcountryCode(51.5,0)$'countryName'

#names<-list()

names2 <-GNcountryCode(-cropset_full$Long[1],cropset_full$Lat[1])

GNcountryCode(10.2,47.03, maxRows=10)


#########################Check those locations that produce NA entries############################


plot(getMap()) 
points(-66.47917,18.47917,col='green') #This are is being recorded as NA but is highly populated and is in 
#Puerto Rico. Same situation with a number of other countries. 


###################################################
#A number of locations that are populated on 
#the peripheries of countries are being recorded 
#as NA so I need to find a way to account for these
#lcations so that our estimates can be as precise
#as possible.
###################################################






########################################Collect data by country######################################################
mexico<-data.table()
mexico<-cropset_full[which(cropset_full$`coords2country(points)`=='Mexico')]
mexico_list<-split(mexico, mexico$hurr_key)

puerto<-data.table()
puerto<-cropset_full[which(cropset_full$`coords2country(points)`=='Puerto Rico')]
puerto_list<-split(puerto, puerto$hurr_key)

bahamas<-data.table()
bahamas<-cropset_full[which(cropset_full$`coords2country(points)`=='The Bahamas')]
bahamas_list<-split(bahamas, bahamas$hurr_key)

haiti<-data.table()
haiti<-cropset_full[which(cropset_full$`coords2country(points)`=='Haiti')]
haiti_list<-split(haiti,haiti$hurr_key)

cayman<-data.table()
cayman<-cropset_full[which(cropset_full$`coords2country(points)`=='Cayman Islands')]
cayman_list<-split(cayman, cayman$hurr_key)

anguilla<-data.table()
anguilla<-cropset_full[which(cropset_full$`coords2country(points)`=='Anguilla')]
anguilla_list<-split(anguilla, anguilla$hurr_key)

guatemala<-data.table()
guatemala<-cropset_full[which(cropset_full$`coords2country(points)`=='Guatemala')]
guatemala_list<-split(guatemala, guatemala$hurr_key)

montserrat<-data.table()
montserrat<-cropset_full[which(cropset_full$`coords2country(points)`=='Montserrat')]
montserrat_list<-split(montserrat, montserrat$hurr_key)

dominica<-data.table()
dominica<-cropset_full[which(cropset_full$`coords2country(points)`=='Dominica')]
dominica_list<-split(dominica, dominica$hurr_key)


stlucia<-data.table()
stlucia<-cropset_full[which(cropset_full$`coords2country(points)`=='Saint Lucia')]
stlucia_list<-split(stlucia, stlucia$hurr_key)

aruba<-data.table()
aruba<-cropset_full[which(cropset_full$`coords2country(points)`=='Aruba')]
aruba_list<-split(aruba, aruba$hurr_key)

tt<-data.table()
tt<-cropset_full[which(cropset_full$`coords2country(points)`=='Trinidad and Tobago')]
tt_list<-split(tt, tt$hurr_key)

guyana<-data.table()
guyana<-cropset_full[which(cropset_full$`coords2country(points)`=='Guyana')]
guyana_list<-split(guyana, guyana$hurr_key)

cuba<-data.table()
cuba<-cropset_full[which(cropset_full$`coords2country(points)`=='Cuba')]
cuba_list<-split(cuba, cuba$hurr_key)

bvi<-data.table()
bvi<-cropset_full[which(cropset_full$`coords2country(points)`=='British Virgin Islands')]
bvi_list<-split(bvi, bvi$hurr_key)

belize<-data.table()
belize<-cropset_full[which(cropset_full$`coords2country(points)`=='Belize')]
belize_list<-split(belize, belize$hurr_key)

stmartin<-data.table()
stmartin<-cropset_full[which(cropset_full$`coords2country(points)`=='Saint Martin')]
stmartin_list<-split(stmartin, stmartin$hurr_key)

AB<-data.table()
AB<-cropset_full[which(cropset_full$`coords2country(points)`=='Antigua and Barbuda')]
AB_list<-split(AB, AB$hurr_key)

honduras<-data.table()
honduras<-cropset_full[which(cropset_full$`coords2country(points)`=='Honduras')]
honduras_list<-split(honduras, honduras$hurr_key)

nicaragua<-data.table()
nicaragua<-cropset_full[which(cropset_full$`coords2country(points)`=='Nicaragua')]
nicaragua_list<-split(nicaragua, nicaragua$hurr_key)

svg<-data.table()
svg<-cropset_full[which(cropset_full$`coords2country(points)`=='Saint Vincent and the Grenadines')]
svg_list<-split(svg, svg$hurr_key)

grenada<-data.table()
grenada<-cropset_full[which(cropset_full$`coords2country(points)`=='Grenada')]
grenada_list<-split(grenada, grenada$hurr_key)

costa_rica<-data.table()
costa_rica<-cropset_full[which(cropset_full$`coords2country(points)`=='Costa Rica')]
costa_rica_list<-split(costa_rica, costa_rica$hurr_key)

turks<-data.table()
turks<-cropset_full[which(cropset_full$`coords2country(points)`=='Turks and Caicos Islands')]
turks_list<-split(turks, turks$hurr_key)

domrep<-data.table()
domrep<-cropset_full[which(cropset_full$`coords2country(points)`=='Dominican Republic')]
domrep_list<-split(domrep, domrep$hurr_key)

jamaica<-data.table()
jamaica<-cropset_full[which(cropset_full$`coords2country(points)`=='Jamaica')]
jamaica_list<-split(jamaica, jamaica$hurr_key)

virgin<-data.table()
virgin<-cropset_full[which(cropset_full$`coords2country(points)`=='United States Virgin Islands')]
virgin_list<-split(virgin, virgin$hurr_key)

skn<-data.table()
skn<-cropset_full[which(cropset_full$`coords2country(points)`=='Saint Kitts and Nevis')]
skn_list<-split(skn, skn$hurr_key)

elsalvador<-data.table()
elsalvador<-cropset_full[which(cropset_full$`coords2country(points)`=='El Salvador')]
elsalvador_list<-split(elsalvador, elsalvador$hurr_key)

barbados<-data.table()
barbados<-cropset_full[which(cropset_full$`coords2country(points)`=='Barbados')]
barbados_list<-split(barbados, barbados$hurr_key)

columbia<-data.table()
columbia<-cropset_full[which(cropset_full$`coords2country(points)`=='Colombia')]
columbia_list<-split(columbia, columbia$hurr_key)

panama<-data.table()
panama<-cropset_full[which(cropset_full$`coords2country(points)`=='Panama')]
panama_list<-split(panama, panama$hurr_key)



#########################################
#with the above, we now have a clear 
#collection of hurricanes and their
#respective distances from each centroid
#in each relevant country
#########################################





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



#Plotting hurricanes for 2005, one fo the most active hurricane seasons in the period of focus.

points_hur=data.frame(-hurr_interpolated$Long.W[1897:1915],hurr_interpolated$Lat.N[1897:1915])

# plot them on a map
map("worldHires", xlim=c(-119, -40), ylim=c(0, 30))
lines(points_hur$X.hurr_interpolated.Long.W.1897.1915., points_hur$hurr_interpolated.Lat.N.1897.1915., col="Red")
lines(-hurr_interpolated$Long.W[1916:1948], hurr_interpolated$Lat.N[1916:1948], col="blue")
lines(-hurr_interpolated$Long.W[2053:2069], hurr_interpolated$Lat.N[2053:2069], col="green") 
lines(-hurr_interpolated$Long.W[1967:1991], hurr_interpolated$Lat.N[1967:1991], col="purple")
lines(-hurr_interpolated$Long.W[1992:2028], hurr_interpolated$Lat.N[1992:2028], col="pink")




#############################################
#So this worked.
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


proj4string(df) <- CRS("+init=epsg:4326") 
df_sf <- st_as_sf(df) %>% st_transform(3488) #transform to NAD83(NSRS2007) / California Albers
df_sf_buff <- st_buffer(df_sf, 500)
plot(df_sf_buff)



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


# Interpolation

hurr_in <- as.data.table(hurr_in)
hurr_in <- hurr_in[, hurr_key:=paste0(Year, '_', Hurricane)]
hurr_in <- hurr_in[, Lat_lag := shift(Lat.N,1,fill=NA,type = 'lag'),by=hurr_key][is.na(Lat_lag),Lat_lag:=Lat.N]




###############################################HURDAT DATASET#######################################################

#Call HURRDAT2 data from package. This dataset replaces the old dataset from which the previous literature 
#conducted research. This dataset is updated to include tropical cyclones between 1851 and 2017.

al<- get_hurdat(basin = "AL")

#Collect tropical cyclones that were classifies under the SS scale as hurricanes(1-3); 
#111-129 mph,96-112 kt,178-208 km/h.

ATLANTIC<-al[which(al$Status=="HU"),]

#Collecting hurricanes between categiries 3 and 5

atlantic_cat345<-ATLANTIC[which(ATLANTIC$Wind >=96),]

#Convert wind speed to km/h. Currently recorded in knots

atlantic_cat345$Wind<-(atlantic_cat345$Wind*1.852)

#Delete additional data that will not be used due to limited number of observations

atlantic_cat345[,10:21]<-NULL
atlantic_cat345<-as.data.table(atlantic_cat345)

#SPlit character variables: Year, Month, Day, Time


alsplit<-str_split_fixed(atlantic_cat345$DateTime, " ",2)

datefix<-alsplit[,1]
timefix<-alsplit[,2]
dsplit<- str_split_fixed(datefix, "-",3)


#Need to separate the time variable into a more maliable formal
timefix<-as.matrix(timefix)

colnames(timefix)<-c("time")

tsplit<-str_split_fixed(timefix,":",3)




atlantic_cat345$month<-dsplit[,2]
atlantic_cat345$day<-dsplit[,3]
atlantic_cat345$time<-alsplit[,2]
atlantic_cat345$year<-dsplit[,1]
atlantic_cat345$atlantic_cat345_id <- 1:nrow(atlantic_cat345)
atlantic_cat345$time<-tsplit[,1]

#Remove the DateTIME Variable that we just broke up
atlantic_cat345$DateTime<-NULL



#############################Interpolate the data###############################################
na.df <- data.frame(num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA, let = NA, let = NA, num = NA, let = NA, num = NA, let = NA, num = NA)


##################################Splitting by key###################
atlantic_cat345 <- as.data.table(atlantic_cat345)
atlantic_cat345_list<-list()
atlantic_cat345_list<-split(atlantic_cat345, atlantic_cat345$Key)
View(atlantic_cat345_list)


################################Creating spaces for the interpolation in the data
hur1852<-as.data.frame(atlantic_cat345_list$AL011852)

for (i in 1:length(atlantic_cat345_list)){
  #Create space for interpolation
  atlantic_cat345_list[[i]]<-do.call(rbind, apply(atlantic_cat345_list[[i]], 1, function(x) {rbind(x, na.df)}))
  
  #Rename variables
  colnames(atlantic_cat345_list[[i]])<-colnames(hur1852)
  
  #Drop last row of each dataset
  atlantic_cat345_list[[i]]<-atlantic_cat345_list[[i]][-nrow(atlantic_cat345_list[[i]]),] 
  
  
  #List replicating the first 3 columns so this will delet them 
  atlantic_cat345_list[[i]][,14:16]<-NULL
  
}





#################################Fill the spaces created with interpoated data


for(count in 1:length(atlantic_cat345_list)){
  
  if(length(atlantic_cat345_list[[count]]$year)>2) { 
    
    #Interpolate for Year
    for(row in 2:length(atlantic_cat345_list[[count]]$year)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$year[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$year[row]<-atlantic_cat345_list[[count]]$year[row+1]
        
      }
      
    } 
    
    
    #Interpolate for month
    for (row in 2:length(atlantic_cat345_list[[count]]$month)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$month[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$month[row] <- atlantic_cat345_list[[count]]$month[row-1]
      }
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>day<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$day)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$day[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$day[row] <-atlantic_cat345_list[[count]]$day[row-1]
      }
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Category<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # 
    # for (row in 2:length(atlantic_cat345_list[[count]]$category)){ # 2 so you don't affect column names
    #   if(is.na(atlantic_cat345_list[[count]]$category[row])) {    # if its empty...
    #     atlantic_cat345_list[[count]]$category[row] <- atlantic_cat345_list[[count]]$category[row-1]
    #   }
    # }
    # 
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>3h interval<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    # for (row in 2:length(atlantic_cat345_list[[count]]$'6h interval UTC')){ # 2 so you don't affect column names
    #   if(is.na(atlantic_cat345_list[[count]]$'6h interval UTC'[row])) {    # if its empty...
    #     atlantic_cat345_list[[count]]$'6h interval UTC'[row]<- 3 + as.numeric(atlantic_cat345_list[[count]]$'6h interval UTC'[row-1])
    #   }
    # }
    
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>name<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Name)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$Name[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Name[row] <- atlantic_cat345_list[[count]]$Name[row-1]
      }
    }
    
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Latitudes<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Lat)){ # 2 so you don't affect column names
      
      if(is.na(atlantic_cat345_list[[count]]$Lat[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Lat[row] <- as.numeric((as.numeric(atlantic_cat345_list[[count]]$Lat[row-1]) + as.numeric(atlantic_cat345_list[[count]]$Lat[row+1]))/2)
        
        
      }
      
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Longitudes<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Lon)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$Lon[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Lon[row]<- as.numeric((as.numeric(atlantic_cat345_list[[count]]$Lon[row-1]) + as.numeric(atlantic_cat345_list[[count]]$Lon[row+1]))/2)
      }
    }
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>time<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$time)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$time[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$time[row] <- (as.numeric(atlantic_cat345_list[[count]]$time[row-1]) + as.numeric(atlantic_cat345_list[[count]]$time[row+1]))/2
      }
    }
    
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>wind<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Wind)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$Wind[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Wind[row] <- (as.numeric(atlantic_cat345_list[[count]]$Wind[row-1]) + as.numeric(atlantic_cat345_list[[count]]$Wind[row+1]))/2
      }
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Pressure<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Pressure)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$Pressure[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Pressure[row] <- (as.numeric(atlantic_cat345_list[[count]]$Pressure[row-1]) + as.numeric(atlantic_cat345_list[[count]]$Pressure[row+1]))/2
      }
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Status<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Status)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$Status[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Status[row] <- atlantic_cat345_list[[count]]$Status[row-1]
      }
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>Record<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Record)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$Record[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Record[row] <- atlantic_cat345_list[[count]]$Record[row-1]
      }
    }
    
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Fix ID<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    
    
    for (row in 2:length(atlantic_cat345_list[[count]]$atlantic_cat345_id)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$atlantic_cat345_id[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$atlantic_cat345_id[row] <- 1+ as.numeric(atlantic_cat345_list[[count]]$atlantic_cat345_id[row-1] )
      }
      
      else{
        
        
        atlantic_cat345_list[[count]]$atlantic_cat345_id[row] <- 1+ as.numeric(atlantic_cat345_list[[count]]$atlantic_cat345_id[row-1] )}
    }
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>atlantic_cat345icane key<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    for (row in 2:length(atlantic_cat345_list[[count]]$Key)){ # 2 so you don't affect column names
      if(is.na(atlantic_cat345_list[[count]]$Key[row])) {    # if its empty...
        atlantic_cat345_list[[count]]$Key[row] <- atlantic_cat345_list[[count]]$Key[row-1]
      }
    }  
    
    
    
    
  }
}

#############################Rbind all the elements of our list to create one solid dataset#######################
atlantic_cat345_interpolated<-rbindlist(atlantic_cat345_list, use.names=TRUE)


#Fix interpolation to be in order because it was messing up
atlantic_cat345_interpolated<- atlantic_cat345_interpolated[order(year),] 


##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Create csv file for atlantic_cat345_in~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################


write.table(atlantic_cat345_interpolated, file="atlantic_cat345_interpolated.csv",sep=",",row.names=F)

atlantic_cat345_interpolated <-fread("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Hurricane data\\atlantic_cat345_interpolated.csv")
atlantic_cat345_interpolated <- atlantic_cat345_interpolated[, Lat := as.numeric(stringr::str_replace(Lat, '\\.$', ''))]
atlantic_cat345_interpolated <- atlantic_cat345_interpolated[, Lon := as.numeric(stringr::str_replace(Lon, '\\.$', ''))]
atlantic_cat345_interpolated$atlantic_cat345_id<-1:nrow(atlantic_cat345_interpolated )


##############Distance with cropped file for atlantic dataset############################


#Distance from huricanes in 1990 for  each centroid in that year's census 

at <- data.table()
adts_crop90 <- data.table()
for(iRow in 1:nrow(atlantic_cat345_interpolated)){
  if((iRow %% 20)==0){
    cat(iRow, 'of', nrow(atlantic_cat345_interpolated), '\n')
  }
  at_lat<-as.numeric(atlantic_cat345_interpolated[iRow]$Lat)
  at_long<-as.numeric(atlantic_cat345_interpolated[iRow]$Lon)
  at_id<-as.numeric(atlantic_cat345_interpolated[iRow]$atlantic_cat345_id)
  atddis_crop<-(distCosine(Cen90_crop[,2:3], c(-at_long,at_lat), r=6378137))/1000 #This finds the shortest distance between two points
  #In meters. Dividing by 1000 gives us our estimates in kilometers
  at<-data.table(at_id = at_id, cen90_id = Cen90_crop$ID, atddis_crop )
  at<-at[atddis_crop<=500]
  adts_crop90 <- rbindlist(list(adts_crop90, at), use.names = TRUE, fill=TRUE)
}
#colnames(ddis)<-c("distance(km)", "dat","ID")

atddis_crop<-data.table::data.table(atddis_crop)


#Rename Column ID for hurricanes so that I can merge them.
colnames(atlantic_cat345_interpolated)[13]<-"at_id"




cropset_at<-merge(adts_crop90, atlantic_cat345_interpolated, by = "at_id", all=T)

#Dropping pressure and records because tey will allow our data to disappear
cropset_at$Record<-NULL
cropset_at$Status<-NULL
cropset_at$Pressure<-NULL




#cropset_at<-cropset_at[-c(is.na(cropset_at$cen90_id)),]

complete.cases(cropset_at)
cropset_at_full<-na.omit(cropset_at)
#cropset_at_full<-as.data.table(cropset_at_full)




#merge by centroid ID to plot better. ID here is the centroid ID for the greographical data
colnames(cropset_at_full)[2]<- "ID"

#Need to rename the hurricane ID so that I can identify it better. The name currently is 
#at_id. I will now call it hurricane_id

colnames(cropset_at_full)[1]<- "hurricane_id"


cropset_at_full<-merge(cropset_at_full, Cen90_crop, by = "ID", all=T)


complete.cases(cropset_at_full)
cropset_at_full<-na.omit(cropset_at_full) #delete NA entries


########################################################
#Have to reduce the dateset to include only hurricanes 
#that occurred between 1990 and 2017. The previous datset 
#included hurricanes as far back as 1851 which resulted
#in the vector of names being too larget to place in one
#set on the computer. The resuction thus allowed the codes
#to run.
##########################################################



colnames(cropset_at_full)[3]<-"distance"
colnames(cropset_at_full)[6]<-"hlat"
colnames(cropset_at_full)[7]<-"hlon"


#Ordered by centroid characteristics: dat




#Reduce dataset size since the current size will not allow the space to be allocated for the information provided.
cropset_at_full<-cropset_at_full[which(cropset_at_full$year>"1969"),]


#We want to create the file on the computer so that we do not have to keep running this code.
write.table(cropset_at_full, file="cropset_at_full.csv",sep=",",row.names=F)





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
  
  pointsSP = SpatialPoints(points, proj4string=CRS("+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  #gBuffer(pointsSP, width=1000)
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
}


points=data.frame(-cropset_at_full$Long,cropset_at_full$Lat.y)




# plot them on a map
map("worldHires", xlim=c(-119, -40), ylim=c(0, 30))
points(points$X.cropset_at_full.Long, points$cropset_at_full.Lat, col="Red")


#Data too big so i need to use bigmomory

devtools::install_github("kaneplusplus/bigmemory")  #Download from master package in github

library(bigmemory.sri)

install.packages("bigmemory", dependencies = TRUE)
library(bigmemory)

#Get a list of country names

names<-coords2country(points)

nam<-as.data.table(names)


#Change Frnace to Guadalupe and Martinique
#nam$`coords2country(points)`<- gsub('France', 'Guadalupe', nam$names)
#Change Naval base to Cuba
#nam$`coords2country(points)`<- gsub('US Naval Base Guantanamo Bay', 'Cuba', nam$names)


cropset_at_full<-cbind(cropset_at_full,nam$names)
colnames(cropset_at_full)[15]<-"Country Name"


#We want to create the file on the computer so that we do not have to keep running this code.
write.table(cropset_at_full, file="cropset_at_full.csv",sep=",",row.names=F)




# options(geonamesUsername="rushaine")
# source(system.file("tests","testing.R",package="geonames"),echo=TRUE)
# GNcountryCode(51.5,0)$'countryName'
# 
# #names<-list()
# 
# names2 <-GNcountryCode(-cropset_full$Long[1],cropset_full$Lat[1])
# 
# GNcountryCode(10.2,47.03, maxRows=10)
# 

#########################Check those locations that produce NA entries############################


plot(getMap()) 
points(-66.47917,18.47917,col='green') #This are is being recorded as NA but is highly populated and is in 
#Puerto Rico. Same situation with a number of other countries. 


###################################################
#A number of locations that are populated on 
#the peripheries of countries are being recorded 
#as NA so I need to find a way to account for these
#lcations so that our estimates can be as precise
#as possible.
###################################################

######################################################
#Lets locate the areas that are labeled as NA by using
#shape files and the extents. 
######################################################

prico<-readOGR(dsn = "C:\\Users\\goulb\\Downloads\\tl_2016_72_cousub", layer = "tl_2016_72_cousub")

#Use @bbox to identify the extents of the island.
#Now to test it.


prico@bbox

prico_cropset<-cropset_at_full[which(cropset_at_full$Long>=65.1685 & cropset_at_full$Long<=67.99875 & cropset_at_full$Lat.y>=17.83151 & cropset_at_full$Lat.y <=18.5680)]



prico.expand<-spTransform(prico, CRS("+proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs"))

pbuf<-gBuffer(prico.expand,width = 100)

coastal_border<-spTransform(pbuf, CRS("+proj=longlat +ellpswgs84"))
plot(coastal_border,border="red")
plot(prico, add= TRUE)






########################################Collect data by country######################################################
mexico<-data.table()
mexico<-cropset_at_full[which(cropset_at_full$`Country Name`=='Mexico')]
mexico_list<-split(mexico, mexico$Key)

puerto<-data.table()
puerto<-cropset_at_full[which(cropset_at_full$'Country Name'=='Puerto Rico')]
puerto_list<-split(puerto, puerto$Key)

bahamas<-data.table()
bahamas<-cropset_at_full[which(cropset_at_full$'Country Name'=='The Bahamas')]
bahamas_list<-split(bahamas, bahamas$Key)

haiti<-data.table()
haiti<-cropset_at_full[which(cropset_at_full$'Country Name'=='Haiti')]
haiti_list<-split(haiti,haiti$Key)

cayman<-data.table()
cayman<-cropset_at_full[which(cropset_at_full$'Country Name' =='Cayman Islands')]
cayman_list<-split(cayman, cayman$Key)

anguilla<-data.table()
anguilla<-cropset_at_full[which(cropset_at_full$'Country Name'=='Anguilla')]
anguilla_list<-split(anguilla, anguilla$Key)

guatemala<-data.table()
guatemala<-cropset_at_full[which(cropset_at_full$'Country Name'=='Guatemala')]
guatemala_list<-split(guatemala, guatemala$Key)

montserrat<-data.table()
montserrat<-cropset_at_full[which(cropset_at_full$'Country Name'=='Montserrat')]
montserrat_list<-split(montserrat, montserrat$Key)

dominica<-data.table()
dominica<-cropset_at_full[which(cropset_at_full$'Country Name'=='Dominica')]
dominica_list<-split(dominica, dominica$Key)


stlucia<-data.table()
stlucia<-cropset_at_full[which(cropset_at_full$'Country Name'=='Saint Lucia')]
stlucia_list<-split(stlucia, stlucia$Key)

aruba<-data.table()
aruba<-cropset_at_full[which(cropset_at_full$'Country Name'=='Aruba')]
aruba_list<-split(aruba, aruba$Key)

tt<-data.table()
tt<-cropset_at_full[which(cropset_at_full$'Country Name'=='Trinidad and Tobago')]
tt_list<-split(tt, tt$Key)

guyana<-data.table()
guyana<-cropset_at_full[which(cropset_at_full$'Country Name'=='Guyana')]
guyana_list<-split(guyana, guyana$Key)

cuba<-data.table()
cuba<-cropset_at_full[which(cropset_at_full$'Country Name'=='Cuba')]
cuba_list<-split(cuba, cuba$Key)

bvi<-data.table()
bvi<-cropset_at_full[which(cropset_at_full$'Country Name'=='British Virgin Islands')]
bvi_list<-split(bvi, bvi$Key)

belize<-data.table()
belize<-cropset_at_full[which(cropset_at_full$'Country Name'=='Belize')]
belize_list<-split(belize, belize$Key)

stmartin<-data.table()
stmartin<-cropset_at_full[which(cropset_at_full$'Country Name'=='Saint Martin')]
stmartin_list<-split(stmartin, stmartin$Key)

AB<-data.table()
AB<-cropset_at_full[which(cropset_at_full$'Country Name'=='Antigua and Barbuda')]
AB_list<-split(AB, AB$Key)

honduras<-data.table()
honduras<-cropset_at_full[which(cropset_at_full$'Country Name'=='Honduras')]
honduras_list<-split(honduras, honduras$Key)

nicaragua<-data.table()
nicaragua<-cropset_at_full[which(cropset_at_full$'Country Name'=='Nicaragua')]
nicaragua_list<-split(nicaragua, nicaragua$Key)

svg<-data.table()
svg<-cropset_at_full[which(cropset_at_full$'Country Name'=='Saint Vincent and the Grenadines')]
svg_list<-split(svg, svg$Key)

grenada<-data.table()
grenada<-cropset_at_full[which(cropset_at_full$'Country Name'=='Grenada')]
grenada_list<-split(grenada, grenada$Key)

costa_rica<-data.table()
costa_rica<-cropset_at_full[which(cropset_at_full$'Country Name'=='Costa Rica')]
costa_rica_list<-split(costa_rica, costa_rica$Key)

turks<-data.table()
turks<-cropset_at_full[which(cropset_at_full$'Country Name'=='Turks and Caicos Islands')]
turks_list<-split(turks, turks$Key)

domrep<-data.table()
domrep<-cropset_at_full[which(cropset_at_full$'Country Name'=='Dominican Republic')]
domrep_list<-split(domrep, domrep$Key)

jamaica<-data.table()
jamaica<-cropset_at_full[which(cropset_at_full$'Country Name'=='Jamaica')]
jamaica_list<-split(jamaica, jamaica$Key)

virgin<-data.table()
virgin<-cropset_at_full[which(cropset_at_full$'Country Name'=='United States Virgin Islands')]
virgin_list<-split(virgin, virgin$Key)

skn<-data.table()
skn<-cropset_at_full[which(cropset_at_full$'Country Name'=='Saint Kitts and Nevis')]
skn_list<-split(skn, skn$Key)

elsalvador<-data.table()
elsalvador<-cropset_at_full[which(cropset_at_full$'Country Name'=='El Salvador')]
elsalvador_list<-split(elsalvador, elsalvador$Key)

barbados<-data.table()
barbados<-cropset_at_full[which(cropset_at_full$'Country Name'=='Barbados')]
barbados_list<-split(barbados, barbados$Key)

columbia<-data.table()
columbia<-cropset_at_full[which(cropset_at_full$'Country Name'=='Colombia')]
columbia_list<-split(columbia, columbia$Key)

panama<-data.table()
panama<-cropset_at_full[which(cropset_at_full$'Country Name'=='Panama')]
panama_list<-split(panama, panama$Key)



#########################################
#with the above, we now have a clear 
#collection of hurricanes and their
#respective distances from each centroid
#in each relevant country
#########################################





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



#Plotting hurricanes for 2005, one fo the most active hurricane seasons in the period of focus.

points_hur=data.frame(-hurr_interpolated$Long.W[1897:1915],hurr_interpolated$Lat.N[1897:1915])

# plot them on a map
map("worldHires", xlim=c(-119, -40), ylim=c(0, 30))
lines(points_hur$X.hurr_interpolated.Long.W.1897.1915., points_hur$hurr_interpolated.Lat.N.1897.1915., col="Red")
lines(-hurr_interpolated$Long.W[1916:1948], hurr_interpolated$Lat.N[1916:1948], col="blue")
lines(-hurr_interpolated$Long.W[2053:2069], hurr_interpolated$Lat.N[2053:2069], col="green") 
lines(-hurr_interpolated$Long.W[1967:1991], hurr_interpolated$Lat.N[1967:1991], col="purple")
lines(-hurr_interpolated$Long.W[1992:2028], hurr_interpolated$Lat.N[1992:2028], col="pink")




#############################################
#So this worked.
############################################


