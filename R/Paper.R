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
install.packages("C:/Users/goulb/Downloads/weathermetrics_1.2.2.tar.gz", repos = NULL, type = "source")
devtools::install_github("geanders/stormwindmodel", build_vignettes = TRUE)
install.packages("stormwindmodel")


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
library(weathermetrics)
library(stormwindmodel)


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




Cen90_crop <-data.table::data.table(Cen90_crop)
Cen90_crop[, ID := .GRP, by = .(Long, Lat)]

Cen90_crop<-Cen90_crop[dat != 0] #Remove locations where no population exist


write.table(Cen90_crop, file="Cen_RDG.csv",sep=",",row.names=F)

write.table(longlat90_crop, file="LongLat.csv",sep=",",row.names=F)







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


write.table(adts_crop90, file="atds_crop90.csv",sep=",",row.names=F)


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


###########>>>>>>>>>>>>>>>>Callin the file cropset_at_full from file for speed rathe than formulate by code each time<<<<<<<<<<<<<<<<<<<############

cropset_at_full<-read.csv("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\hurricane\\R\\hurricane\\R\\cropset_at_full.csv")
colnames(cropset_at_full)[15]<-"Lat.y"
colnames(cropset_at_full)[16]<-"Country Name"




cropset_at_full<-read.csv("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Paper Data\\cropset_at_full.csv")
colnames(cropset_at_full)[15]<-"Lat.y"
colnames(cropset_at_full)[16]<-"Country Name"

#Convert the name column from factor to character 
cropset_at_full$`Country Name`<-as.character(cropset_at_full$`Country Name`)

cropset_at_full_list<-list()
cropset_at_full_list<-(split(cropset_at_full, cropset_at_full$year , drop = TRUE))




# 
# ######################################################
# #Lets locate the areas that are labeled as NA by using
# #shape files and the extents. 
# ######################################################
# 
# prico<-readOGR(dsn = "C:\\Users\\goulb\\Downloads\\tl_2016_72_cousub", layer = "tl_2016_72_cousub")
# 
# #Use @bbox to identify the extents of the island.
# #Now to test it.
# prico@bbox
# 
# 
# #Puerto Rico shapefile
# 
# # pr<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_PRI_shp", layer = "gadm36_PRI_1")
# # 
# # #plot(pr)
# # prbox<-pr@bbox
# # 
# # 
# # 
# # #Create the cropped sets for  each country
# # prico_cropset<-cropset_at_full[which(cropset_at_full$Long>=-(prbox[1,2]) & cropset_at_full$Long<=-(prbox[1,1]) & cropset_at_full$Lat.y>=(prbox[2,1]) & cropset_at_full$Lat.y <=(prbox[2,2])),]
# # 
# # #Testing the if function with a simple matrix.
# # 
# # # for(p in 1: nrow(mn)){
# # #   if(mn$m[p]==2){                #If you get an error saying only first element will be 
# # #                                                              #used, it means you arent using the identifier; in this 
# # #                                                              #case p
# # #  mn$m[p]<-"water"
# # #     
# # #   }}
# # 
# # 
# # #TEST
# # ###################################################################################################################
# # # for(p in 1: nrow(prico_cropset)){
# # #   if(is.na(prico_cropset$`Country Name`[p])){                #If you get an error saying only first element will be 
# # #     #used, it means you arent using the identifier; in this 
# # #     #case p
# # #     prico_cropset$`Country Name`[p]<-"water"
# # #     
# # #   }}
# # ######################################################################################################################
# # 
# # 
# # 
# # 
# # #Fill locations in dataset that belong to  "Puerto Rico"
# # for(p in 1: nrow(cropset_at_full)){
# #   
# #   if(cropset_at_full$Long[p]>=(-prbox[1,2]) & cropset_at_full$Long[p]<=(-prbox[1,1]) & cropset_at_full$Lat.y[p]>=(prbox[2,1]) & cropset_at_full$Lat.y[p]<=(prbox[2,2]) & is.na(cropset_at_full$`Country Name`[p]))
# #   {cropset_at_full$`Country Name`[p]<-"Puerto Rico"}
# #   
# # }
# 
# 
# 
#Shape file Jamaica
# 
# jam<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_JAM_shp", layer = "gadm36_JAM_1")
# 
# jmbox<-jam@bbox
# 
# #Fill locations in dataset that belong to  "Jamaica"
# for(p in 1: nrow(cropset_at_full)){
# 
#   if(cropset_at_full$Long[p]>=-(jmbox[1,2]) & cropset_at_full$Long[p]<=-(jmbox[1,1]) & cropset_at_full$Lat.y[p]>=(jmbox[2,1]) & cropset_at_full$Lat.y[p]<=(jmbox[2,2]))
# 
#   {cropset_at_full$`Country Name`[p]<- "Jamaica"
#   }
# 
# }
# 
# 
# #Shapefile for Aruba
# 
# aruba<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_ABW_shp", layer = "gadm36_ABW_0")
# 
# abwbox<-aruba@bbox
# 
# #Fill locations in dataset that belong to "Aruba"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(abwbox[1,2]) & cropset_at_full$Long[p]<=-(abwbox[1,1]) & cropset_at_full$Lat.y[p]>=(abwbox[2,1]) & cropset_at_full$Lat.y[p]<=(abwbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Aruba"}
#   
# }
# 
# 
# #Shapefile for Anguilla
# 
# anguilla<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_AIA_shp", layer = "gadm36_AIA_0")
# 
# angbox<-anguilla@bbox
# 
# #Fill locations in dataset that belong to  "Anguilla"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(angbox[1,2]) & cropset_at_full$Long[p]<=-(angbox[1,1]) & cropset_at_full$Lat.y[p]>=(angbox[2,1]) & cropset_at_full$Lat.y[p]<=(angbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Anguilla"}
#   
# }
# 
# 
# #Shapefile for A&B
# 
# AB<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_ATG_shp", layer = "gadm36_ATG_1")
# 
# ABbox<-AB@bbox
# 
# #Fill locations in dataset that belong to "Antigua and Barbuda"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(ABbox[1,2]) & cropset_at_full$Long[p]<=-(ABbox[1,1]) & cropset_at_full$Lat.y[p]>=(ABbox[2,1]) & cropset_at_full$Lat.y[p]<=(ABbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Antigua and Barbuda"}
#   
# }
# 
# 
# 
# 
# bhs<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_BHS_shp", layer = "gadm36_BHS_1")
# 
# bhsbox<-bhs@bbox
# 
# #Fill locations in dataset that belong to Puerto Rico with the name "Bahamas"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(bhsbox[1,2]) & cropset_at_full$Long[p]<=-(bhsbox[1,1]) & cropset_at_full$Lat.y[p]>=(bhsbox[2,1]) & cropset_at_full$Lat.y[p]<=(bhsbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Bahamas"}
#   
# }
# 
# 
# blz<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_BLZ_shp", layer = "gadm36_BLZ_1")
# 
# blzbox<-blz@bbox
# 
# #Fill locations in dataset that belong to Puerto Rico with the name "Belize"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(blzbox[1,2]) & cropset_at_full$Long[p]<=-(blzbox[1,1]) & cropset_at_full$Lat.y[p]>=(blzbox[2,1]) & cropset_at_full$Lat.y[p]<=(blzbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Belize"}
#   
# }
# 
# 
# brb<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_BRB_shp", layer = "gadm36_BRB_1")
# 
# brbbox<-brb@bbox
# 
# #Fill locations in dataset that belong to Puerto Rico with the name "Barbados"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(brbbox[1,2]) & cropset_at_full$Long[p]<=-(brbbox[1,1]) & cropset_at_full$Lat.y[p]>=(brbbox[2,1]) & cropset_at_full$Lat.y[p]<=(brbbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Barbados"}
#   
# }
# 
# 
# col<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_COL_shp", layer = "gadm36_COL_1")
# 
# colbox<-col@bbox
# 
# #Fill locations in dataset that belong to "Columbia"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(colbox[1,2]) & cropset_at_full$Long[p]<=-(colbox[1,1]) & cropset_at_full$Lat.y[p]>=(colbox[2,1]) & cropset_at_full$Lat.y[p]<=(colbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Columbia"}
#   
# }
# 
# 
# cri<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_CRI_shp", layer = "gadm36_CRI_1")
# 
# cribox<-cri@bbox
# 
# #Fill locations in dataset that belong to "Costa Rica"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(cribox[1,2]) & cropset_at_full$Long[p]<=-(cribox[1,1]) & cropset_at_full$Lat.y[p]>=(cribox[2,1]) & cropset_at_full$Lat.y[p]<=(cribox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Costa Rica"}
#   
# }
# 
# 
# cub<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_CUB_shp", layer = "gadm36_CUB_1")
# 
# cubbox<-cub@bbox
# 
# #Fill locations in dataset that belong to "Cuba"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(cubbox[1,2]) & cropset_at_full$Long[p]<=-(cubbox[1,1]) & cropset_at_full$Lat.y[p]>=(cubbox[2,1]) & cropset_at_full$Lat.y[p]<=(cubbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Cuba"}
#   
# }
# 
# 
# cur<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_CUW_shp", layer = "gadm36_CUW_0")
# 
# curbox<-cur@bbox
# 
# #Fill locations in dataset that belong to "Curacao"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(curbox[1,2]) & cropset_at_full$Long[p]<=-(curbox[1,1]) & cropset_at_full$Lat.y[p]>=(curbox[2,1]) & cropset_at_full$Lat.y[p]<=(curbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Curacao"}
#   
# }
# 
# cym<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_CYM_shp", layer = "gadm36_CYM_1")
# 
# cymbox<-cym@bbox
# 
# #Fill locations in dataset that belong to "Cayman"
# for(p in 1: nrow(cropset_at_full)){
#   
#   
#   if(cropset_at_full$Long[p]>=-(cymbox[1,2]) & cropset_at_full$Long[p]<=-(cymbox[1,1]) & cropset_at_full$Lat.y[p]>=(cymbox[2,1]) & cropset_at_full$Lat.y[p]<=(cymbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Cayman"}
#   
#   
#   
# }
# 
# 
# #Need  extra steps to fully identify cayman: Fill cells 1945666:1946001
# cropset_at_full$`Country Name`[1945666:1946001]<-"Cayman"
# 
# 
# 
# 
# dma<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_DMA_shp", layer = "gadm36_DMA_1")
# 
# dmabox<-dma@bbox
# 
# #Fill locations in dataset that belong to  "Dominica"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(dmabox[1,2]) & cropset_at_full$Long[p]<=-(dmabox[1,1]) & cropset_at_full$Lat.y[p]>=(dmabox[2,1]) & cropset_at_full$Lat.y[p]<=(dmabox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Dominica"}
#   
# }
# 
# dom<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_DOM_shp", layer = "gadm36_DOM_1")
# 
# dombox<-dom@bbox
# 
# #Fill locations in dataset that belong to  "Dom Rep."
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(dombox[1,2]) & cropset_at_full$Long[p]<=-(dombox[1,1]) & cropset_at_full$Lat.y[p]>=(dombox[2,1]) & cropset_at_full$Lat.y[p]<=(dombox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Dominican Republic"}
#   
# }
# 
# 
# glp<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_GLP_shp", layer = "gadm36_GLP_1")
# 
# glpbox<-glp@bbox
# 
# #Fill locations in dataset that belong to  "Guadaloupe"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(glpbox[1,2]) & cropset_at_full$Long[p]<=-(glpbox[1,1]) & cropset_at_full$Lat.y[p]>=(glpbox[2,1]) & cropset_at_full$Lat.y[p]<=(glpbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Guadeloupe"}
#   
# }
# 
# grd<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_GRD_shp", layer = "gadm36_GRD_1")
# 
# grdbox<-grd@bbox
# 
# #Fill locations in dataset that belong to  "Grenada"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(grdbox[1,2]) & cropset_at_full$Long[p]<=-(grdbox[1,1]) & cropset_at_full$Lat.y[p]>=(grdbox[2,1]) & cropset_at_full$Lat.y[p]<=(grdbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Grenada"}
#   
# }
# 
# gtm<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_GTM_shp", layer = "gadm36_GTM_1")
# 
# gtmbox<-gtm@bbox
# 
# #Fill locations in dataset that belong to  "Guatemala"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(gtmbox[1,2]) & cropset_at_full$Long[p]<=-(gtmbox[1,1]) & cropset_at_full$Lat.y[p]>=(gtmbox[2,1]) & cropset_at_full$Lat.y[p]<=(gtmbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Guatemala"}
#   
# }
# 
# guy<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_GUY_shp", layer = "gadm36_GUY_1")
# 
# guybox<-guy@bbox
# 
# #Fill locations in dataset that belong to "Guyana"
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(guybox[1,2]) & cropset_at_full$Long[p]<=-(guybox[1,1]) & cropset_at_full$Lat.y[p]>=(guybox[2,1]) & cropset_at_full$Lat.y[p]<=(guybox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Guyana"}
#   
# }
# 
# hnd<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_HND_shp", layer = "gadm36_HND_1")
# 
# hndbox<-hnd@bbox
# 
# #Fill locations in dataset that belong to honduras
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(hndbox[1,2]) & cropset_at_full$Long[p]<=-(hndbox[1,1]) & cropset_at_full$Lat.y[p]>=(hndbox[2,1]) & cropset_at_full$Lat.y[p]<=(hndbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Honduras"}
#   
# }
# 
# hti<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_HTI_shp", layer = "gadm36_HTI_1")
# 
# htibox<-hti@bbox
# 
# #Fill locations in dataset that belong to Haiti
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(htibox[1,2]) & cropset_at_full$Long[p]<=-(htibox[1,1]) & cropset_at_full$Lat.y[p]>=(htibox[2,1]) & cropset_at_full$Lat.y[p]<=(htibox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Haiti"}
#   
#   
#  
# }
# 
# #Extra Help Filling cells:
# 
# cropset_at_full$`Country Name`[3058877:3059301]<-"Haiti"
# 
# 
# 
# 
# 
# kna<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_KNA_shp", layer = "gadm36_KNA_1")
# 
# knabox<-kna@bbox
# 
# #Fill locations in dataset that belong to SKN
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(knabox[1,2]) & cropset_at_full$Long[p]<=-(knabox[1,1]) & cropset_at_full$Lat.y[p]>=(knabox[2,1]) & cropset_at_full$Lat.y[p]<=(knabox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Saint Kitts and Nevis"}
#   
# }
# 
# lca<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_LCA_shp", layer = "gadm36_LCA_1")
# 
# lcabox<-lca@bbox
# 
# #Fill locations in dataset that belong to St. Lucia
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(lcabox[1,2]) & cropset_at_full$Long[p]<=-(lcabox[1,1]) & cropset_at_full$Lat.y[p]>=(lcabox[2,1]) & cropset_at_full$Lat.y[p]<=(lcabox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Saint Lucia"}
#   
# }
# 
# maf<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_MAF_shp", layer = "gadm36_MAF_0")
# 
# mafbox<-maf@bbox
# 
# #Fill locations in dataset that belong to Saint Martin
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(mafbox[1,2]) & cropset_at_full$Long[p]<=-(mafbox[1,1]) & cropset_at_full$Lat.y[p]>=(mafbox[2,1]) & cropset_at_full$Lat.y[p]<=(mafbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Saint Martin"}
#   
# }
# 
# cropset_at_full$`Country Name`[3067614:3068213]<-"Saint Martin"
# 
# 
# mex<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_MEX_shp", layer = "gadm36_MEX_1")
# 
# mexbox<-mex@bbox
# 
# #Fill locations in dataset that belong to Mexico
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(mexbox[1,2]) & cropset_at_full$Long[p]<=-(mexbox[1,1]) & cropset_at_full$Lat.y[p]>=(mexbox[2,1]) & cropset_at_full$Lat.y[p]<=(mexbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Mexico"}
#   
# }
# 
# mtq<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_MTQ_shp", layer = "gadm36_MTQ_1")
# 
# mtqbox<-mtq@bbox
# 
# #Fill locations in dataset that belong to Martinique
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(mtqbox[1,2]) & cropset_at_full$Long[p]<=-(mtqbox[1,1]) & cropset_at_full$Lat.y[p]>=(mtqbox[2,1]) & cropset_at_full$Lat.y[p]<=(mtqbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Martinique"}
# 
#   
# }
# 
# nic<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_NIC_shp", layer = "gadm36_NIC_1")
# 
# nicbox<-nic@bbox
# 
# #Fill locations in dataset that belong to Nicaragua
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(nicbox[1,2]) & cropset_at_full$Long[p]<=-(nicbox[1,1]) & cropset_at_full$Lat.y[p]>=(nicbox[2,1]) & cropset_at_full$Lat.y[p]<=(nicbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Nicaragua"}
#   
# }
# 
# pan<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_PAN_shp", layer = "gadm36_PAN_1")
# 
# panbox<-pan@bbox
# 
# #Fill locations in dataset that belong to Panama
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(panbox[1,2]) & cropset_at_full$Long[p]<=-(panbox[1,1]) & cropset_at_full$Lat.y[p]>=(panbox[2,1]) & cropset_at_full$Lat.y[p]<=(panbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Panama"}
#   
#   
# }
# 
# pri<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_PRI_shp", layer = "gadm36_PRI_1")
# 
# pribox<-pri@bbox
# 
# #Fill locations in dataset that belong to Puerto Rico
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(pribox[1,2]) & cropset_at_full$Long[p]<=-(pribox[1,1]) & cropset_at_full$Lat.y[p]>=(pribox[2,1]) & cropset_at_full$Lat.y[p]<=(pribox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Puerto Rico"}
#   
#   
#  
#   }
# 
# #Extra Help Needed 
# 
# 
# cropset_at_full$`Country Name`[2658418:2659166]<-"Puerto Rico"
# 
# 
# 
# slv<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_SLV_shp", layer = "gadm36_SLV_1")
# 
# slvbox<-slv@bbox
# 
# #Fill locations in dataset that belong to El Salvadore
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(slvbox[1,2]) & cropset_at_full$Long[p]<=-(slvbox[1,1]) & cropset_at_full$Lat.y[p]>=(slvbox[2,1]) & cropset_at_full$Lat.y[p]<=(slvbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"El Salvador"}
#   
#   
# }
# 
# tca<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_TCA_shp", layer = "gadm36_TCA_1")
# 
# tcabox<-tca@bbox
# 
# #Fill locations in dataset that belong to Turks and Caicos Islands
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(tcabox[1,2]) & cropset_at_full$Long[p]<=-(tcabox[1,1]) & cropset_at_full$Lat.y[p]>=(tcabox[2,1]) & cropset_at_full$Lat.y[p]<=(tcabox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Turks and Caicos Islands"}
#   
#   
# }
# 
# tto<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_TTO_shp", layer = "gadm36_TTO_1")
# 
# ttobox<-tto@bbox
# 
# #Fill locations in dataset that belong to Trinidad and Tobago
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(ttobox[1,2]) & cropset_at_full$Long[p]<=-(ttobox[1,1]) & cropset_at_full$Lat.y[p]>=(ttobox[2,1]) & cropset_at_full$Lat.y[p]<=(ttobox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Trinidad and Tobago"}
#   
#   
# }
# 
# vct<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_VCT_shp", layer = "gadm36_VCT_1")
# 
# vctbox<-vct@bbox
# 
# #Fill locations in dataset that belong to Saint Vincent and the Grenadines
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(vctbox[1,2]) & cropset_at_full$Long[p]<=-(vctbox[1,1]) & cropset_at_full$Lat.y[p]>=(vctbox[2,1]) & cropset_at_full$Lat.y[p]<=(vctbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"Saint Vincent and the Grenadines"}
#   
#   
# }
# 
# #########################################################################################################################################################
# #Venezuela's marine borders extend as far as Puerto Rico and hence was overriding the countries that lie between these two countries. As such this section 
# #of the algorithm had to be removed in order to get the names accurately.
# ##########################################################################################################################################################
# 
# # ven<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_VEN_shp", layer = "gadm36_VEN_2")
# # 
# # venbox<-ven@bbox
# # 
# # #Fill locations in dataset that belong to Venezuela
# # for(p in 1: nrow(cropset_at_full)){
# #   
# #   if(cropset_at_full$Long[p]>=-(venbox[1,2]) & cropset_at_full$Long[p]<=-(venbox[1,1]) & cropset_at_full$Lat.y[p]>=(venbox[2,1]) & cropset_at_full$Lat.y[p]<=(venbox[2,2]))
# #     
# #   {cropset_at_full$`Country Name`[p]<-"Venezuela"}
# #   
# #   
# # }
# # 
# # #Venezuela' smarine borders extend as far as Puerto Rico so we have to leave it out of the code since it overrides the countries between these two countries.
# # 
# #  ven1<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\capa-de-venezuela\\Venezuela Esequibo", layer= "Limites_Internacionales_Venezuela")
# # 
# 
#  vgb<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_VGB_shp", layer = "gadm36_VGB_1")
# 
#  vgbbox<-vgb@bbox
# 
#  #Fill locations in dataset that belong to British Virgin Islands
#  for(p in 1: nrow(cropset_at_full)){
# 
#    if(cropset_at_full$Long[p]>=-(vgbbox[1,2]) & cropset_at_full$Long[p]<=-(vgbbox[1,1]) & cropset_at_full$Lat.y[p]>=(vgbbox[2,1]) & cropset_at_full$Lat.y[p]<=(vgbbox[2,2]))
# 
#    {cropset_at_full$`Country Name`[p]<-"British Virgin Islands"}
# 
# 
# }
# 
# vir<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_VIR_shp", layer = "gadm36_VIR_1")
# 
# virbox<-vir@bbox
# 
# #Fill locations in dataset that belong to United States Virgin Islands
# for(p in 1: nrow(cropset_at_full)){
#   
#   if(cropset_at_full$Long[p]>=-(virbox[1,2]) & cropset_at_full$Long[p]<=-(virbox[1,1]) & cropset_at_full$Lat.y[p]>=(virbox[2,1]) & cropset_at_full$Lat.y[p]<=(virbox[2,2]))
#     
#   {cropset_at_full$`Country Name`[p]<-"United States Virgin Islands"}
#   
#   
# }

boxlist<-list(virbox, brbbox, colbox, ABbox, abwbox, bhsbox, blzbox, cribox, cubbox, curbox, cymbox, dmabox, glpbox, gtmbox, guybox, hndbox, htibox, jmbox, knabox, lcabox, mafbox, mexbox, mtqbox, nicbox, panbox, prbox, slvbox, tcabox, ttobox)


# #Countries where locations were not assigned- they were manually entered
# 
# cropset_at_full$`Country Name`[3107996:3108193]<-"Saint Barthelemy"
# cropset_at_full$`Country Name`[3122237:3122632]<-"Saint Barthelemy"
# cropset_at_full$`Country Name`[3151635:3151823]<-"St Croix"
# cropset_at_full$`Country Name`[3303999:3318849]<-"Montserrat" #:3308720]<-"Montserrat"
# cropset_at_full$`Country Name`[3746857:3746997]<-"Martinique"
# cropset_at_full$`Country Name`[3338388:3371927]<-"Guadeloupe"
# cropset_at_full$`Country Name`[3734213:3759981]<-"Martinique"
# cropset_at_full$`Country Name`[4124593:4124690]<-"Saint Vincent and the Grenadines"
# cropset_at_full$`Country Name`[4131243:4292136]<-"Saint Vincent and the Grenadines"
# cropset_at_full$`Country Name`[4131339:4131610]<-"Barbados"
# cropset_at_full$`Country Name`[4132000:4132030]<-"Honduras"
# cropset_at_full$`Country Name`[4132031:4132479]<-"Honduras"
# cropset_at_full$`Country Name`[4410773:4410818]<-"Venezuela"
# cropset_at_full$`Country Name`[4448486:4448690]<-"Venezuela"
# cropset_at_full$`Country Name`[4611625:4569958]<-"Venezuela"
# cropset_at_full$`Country Name`[4431756:4569906]<-"Venezuela"
# cropset_at_full$`Country Name`[4320298:4424477]<-"Venezuela"


#This method of assigning names is more efficient than using the loop
#Note that the shape files dont account for the rigidity in the shapes 
#of the countries so for countries in places like central america 
#close to borders the names may not be national correct.
#
#
#
#Must Assign Columbia before Aruba since the shape file will absorb Aruba due to proximity.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Get names<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cropset_at_full[(which(cropset_at_full$Long>=-(jmbox[1,2]) & cropset_at_full$Long<=-(jmbox[1,1]) & cropset_at_full$Lat.y>=(jmbox[2,1]) & cropset_at_full$Lat.y<=(jmbox[2,2]))),16]<-"Jamaica"
cropset_at_full[(which(cropset_at_full$Long>=-(colbox[1,2]) & cropset_at_full$Long<=-(colbox[1,1]) & cropset_at_full$Lat.y>=(colbox[2,1]) & cropset_at_full$Lat.y<=(colbox[2,2]))),16]<-"Columbia"
cropset_at_full[(which(cropset_at_full$Long>=-(abwbox[1,2]) & cropset_at_full$Long<=-(abwbox[1,1]) & cropset_at_full$Lat.y>=(abwbox[2,1]) & cropset_at_full$Lat.y<=(abwbox[2,2]))),16]<-"Aruba"
cropset_at_full[(which(cropset_at_full$Long>=-(angbox[1,2]) & cropset_at_full$Long<=-(angbox[1,1]) & cropset_at_full$Lat.y>=(angbox[2,1]) & cropset_at_full$Lat.y<=(angbox[2,2]))),16]<-"Anguilla"
cropset_at_full[(which(cropset_at_full$Long>=-(ABbox[1,2]) & cropset_at_full$Long<=-(ABbox[1,1]) & cropset_at_full$Lat.y>=(ABbox[2,1]) & cropset_at_full$Lat.y<=(ABbox[2,2]))),16]<-"Antigua and Barbuda"
cropset_at_full[(which(cropset_at_full$Long>=-(bhsbox[1,2]) & cropset_at_full$Long<=-(bhsbox[1,1]) & cropset_at_full$Lat.y>=(bhsbox[2,1]) & cropset_at_full$Lat.y<=(bhsbox[2,2]))),16]<-"Bahamas"
cropset_at_full[(which(cropset_at_full$Long>=-(blzbox[1,2]) & cropset_at_full$Long<=-(blzbox[1,1]) & cropset_at_full$Lat.y>=(blzbox[2,1]) & cropset_at_full$Lat.y<=(blzbox[2,2]))),16]<-"Belize"
cropset_at_full[(which(cropset_at_full$Long>=-(brbbox[1,2]) & cropset_at_full$Long<=-(brbbox[1,1]) & cropset_at_full$Lat.y>=(brbbox[2,1]) & cropset_at_full$Lat.y<=(brbbox[2,2]))),16]<-"Barbados"
cropset_at_full[(which(cropset_at_full$Long>=-(cribox[1,2]) & cropset_at_full$Long<=-(cribox[1,1]) & cropset_at_full$Lat.y>=(cribox[2,1]) & cropset_at_full$Lat.y<=(cribox[2,2]))),16]<-"Costa Rica"
cropset_at_full[(which(cropset_at_full$Long>=-(cubbox[1,2]) & cropset_at_full$Long<=-(cubbox[1,1]) & cropset_at_full$Lat.y>=(cubbox[2,1]) & cropset_at_full$Lat.y<=(cubbox[2,2]))),16]<-"Cuba"
cropset_at_full[(which(cropset_at_full$Long>=-(curbox[1,2]) & cropset_at_full$Long<=-(curbox[1,1]) & cropset_at_full$Lat.y>=(curbox[2,1]) & cropset_at_full$Lat.y<=(curbox[2,2]))),16]<-"Curacao"
cropset_at_full[(which(cropset_at_full$Long>=-(cymbox[1,2]) & cropset_at_full$Long<=-(cymbox[1,1]) & cropset_at_full$Lat.y>=(cymbox[2,1]) & cropset_at_full$Lat.y<=(cymbox[2,2]))),16]<-"Cayman"
cropset_at_full[(which(cropset_at_full$Long>=-(dmabox[1,2]) & cropset_at_full$Long<=-(dmabox[1,1]) & cropset_at_full$Lat.y>=(dmabox[2,1]) & cropset_at_full$Lat.y<=(dmabox[2,2]))),16]<-"Dominica"
cropset_at_full[(which(cropset_at_full$Long>=-(dombox[1,2]) & cropset_at_full$Long<=-(dombox[1,1]) & cropset_at_full$Lat.y>=(dombox[2,1]) & cropset_at_full$Lat.y<=(dombox[2,2]))),16]<-"Dominican Republic"
cropset_at_full[(which(cropset_at_full$Long>=-(glpbox[1,2]) & cropset_at_full$Long<=-(glpbox[1,1]) & cropset_at_full$Lat.y>=(glpbox[2,1]) & cropset_at_full$Lat.y<=(glpbox[2,2]))),16]<-"Guadeloupe"
cropset_at_full[(which(cropset_at_full$Long>=-(grdbox[1,2]) & cropset_at_full$Long<=-(grdbox[1,1]) & cropset_at_full$Lat.y>=(grdbox[2,1]) & cropset_at_full$Lat.y<=(grdbox[2,2]))),16]<-"Grenada"
cropset_at_full[(which(cropset_at_full$Long>=-(gtmbox[1,2]) & cropset_at_full$Long<=-(gtmbox[1,1]) & cropset_at_full$Lat.y>=(gtmbox[2,1]) & cropset_at_full$Lat.y<=(gtmbox[2,2]))),16]<-"Guatemala"
cropset_at_full[(which(cropset_at_full$Long>=-(guybox[1,2]) & cropset_at_full$Long<=-(guybox[1,1]) & cropset_at_full$Lat.y>=(guybox[2,1]) & cropset_at_full$Lat.y<=(guybox[2,2]))),16]<-"Guyana"
cropset_at_full[(which(cropset_at_full$Long>=-(hndbox[1,2]) & cropset_at_full$Long<=-(hndbox[1,1]) & cropset_at_full$Lat.y>=(hndbox[2,1]) & cropset_at_full$Lat.y<=(hndbox[2,2]))),16]<-"Honduras"
cropset_at_full[(which(cropset_at_full$Long>=-(htibox[1,2]) & cropset_at_full$Long<=-(htibox[1,1]) & cropset_at_full$Lat.y>=(htibox[2,1]) & cropset_at_full$Lat.y<=(htibox[2,2]))),16]<-"Haiti"
cropset_at_full[(which(cropset_at_full$Long>=-(knabox[1,2]) & cropset_at_full$Long<=-(knabox[1,1]) & cropset_at_full$Lat.y>=(knabox[2,1]) & cropset_at_full$Lat.y<=(knabox[2,2]))),16]<-"Saint Kitts and Nevis"
cropset_at_full[(which(cropset_at_full$Long>=-(lcabox[1,2]) & cropset_at_full$Long<=-(lcabox[1,1]) & cropset_at_full$Lat.y>=(lcabox[2,1]) & cropset_at_full$Lat.y<=(lcabox[2,2]))),16]<-"Saint Lucia"
cropset_at_full[(which(cropset_at_full$Long>=-(mafbox[1,2]) & cropset_at_full$Long<=-(mafbox[1,1]) & cropset_at_full$Lat.y>=(mafbox[2,1]) & cropset_at_full$Lat.y<=(mafbox[2,2]))),16]<-"Saint Martin"
cropset_at_full[(which(cropset_at_full$Long>=-(mexbox[1,2]) & cropset_at_full$Long<=-(mexbox[1,1]) & cropset_at_full$Lat.y>= (mexbox[2,1]) & cropset_at_full$Lat.y<= (mexbox[2,2]))),16]<-"Mexico"
cropset_at_full[(which(cropset_at_full$Long>=-(mtqbox[1,2]) & cropset_at_full$Long<=-(mtqbox[1,1]) & cropset_at_full$Lat.y>=(mtqbox[2,1]) & cropset_at_full$Lat.y<=(mtqbox[2,2]))),16]<-"Martinique"
cropset_at_full[(which(cropset_at_full$Long>=-(nicbox[1,2]) & cropset_at_full$Long<=-(nicbox[1,1]) & cropset_at_full$Lat.y>=(nicbox[2,1]) & cropset_at_full$Lat.y<=(nicbox[2,2]))),16]<-"Nicaragua"
cropset_at_full[(which(cropset_at_full$Long>=-(panbox[1,2]) & cropset_at_full$Long<=-(panbox[1,1]) & cropset_at_full$Lat.y>=(panbox[2,1]) & cropset_at_full$Lat.y<=(panbox[2,2]))),16]<-"Panama"
cropset_at_full[(which(cropset_at_full$Long>=-(pribox[1,2]) & cropset_at_full$Long<=-(pribox[1,1]) & cropset_at_full$Lat.y>=(pribox[2,1]) & cropset_at_full$Lat.y<=(pribox[2,2]))),16]<-"Puerto Rico"
cropset_at_full[(which(cropset_at_full$Long>=-(slvbox[1,2]) & cropset_at_full$Long<=-(slvbox[1,1]) & cropset_at_full$Lat.y>=(slvbox[2,1]) & cropset_at_full$Lat.y<=(slvbox[2,2]))),16]<-"El Salvador"
cropset_at_full[(which(cropset_at_full$Long>=-(tcabox[1,2]) & cropset_at_full$Long<=-(tcabox[1,1]) & cropset_at_full$Lat.y>=(tcabox[2,1]) & cropset_at_full$Lat.y<=(tcabox[2,2]))),16]<-"Turks and Caicos Islands"    
cropset_at_full[(which(cropset_at_full$Long>=-(ttobox[1,2]) & cropset_at_full$Long<=-(ttobox[1,1]) & cropset_at_full$Lat.y>=(ttobox[2,1]) & cropset_at_full$Lat.y<=(ttobox[2,2]))),16]<- "Trinidad and Tobago"  
cropset_at_full[(which(cropset_at_full$Long>=-(vctbox[1,2]) & cropset_at_full$Long<=-(vctbox[1,1]) & cropset_at_full$Lat.y>=(vctbox[2,1]) & cropset_at_full$Lat.y<=(vctbox[2,2]))),16]<- "Saint Vincent and the Grenadines"
cropset_at_full[(which(cropset_at_full$Long>=-(vgbbox[1,2]) & cropset_at_full$Long<=-(vgbbox[1,1]) & cropset_at_full$Lat.y>=(vgbbox[2,1]) & cropset_at_full$Lat.y<=(vgbbox[2,2]))),16]<-"British Virgin Islands"  
cropset_at_full[(which(cropset_at_full$Long>=-(virbox[1,2]) & cropset_at_full$Long<=-(virbox[1,1]) & cropset_at_full$Lat.y>=(virbox[2,1]) & cropset_at_full$Lat.y<=(virbox[2,2]))),16]<-"United States Virgin Islands"
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>_____________________________<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




#Check the numner of cell entries that are NA 
sum(length(which(is.na(cropset_at_full$`Country Name`))))

#After filling, the total amount of NA locations fell from 228050 to 0

write.table(cropset_at_full, file="cropset_at_full.csv",sep=",",row.names=F)







########################################Collect data by country######################################################
mexico<-data.table()
mexico<-subset(cropset_at_full,cropset_at_full$`Country Name`== 'Mexico')
mexico_list<-split(mexico, mexico$Key, drop = TRUE)
write.table(mexico, file="mexico.csv",sep=",",row.names=F)

puerto<-data.table()
puerto<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Puerto Rico')
puerto_list<-split(puerto, puerto$Key , drop = TRUE)
write.table(puerto, file="puerto.csv",sep=",",row.names=F)

bahamas<-data.table()
bahamas<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Bahamas')
bahamas_list<-split(bahamas, bahamas$Key, drop = TRUE)
write.table(bahamas, file="bahamas.csv",sep=",",row.names=F)

haiti<-data.table()
haiti<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Haiti')
haiti_list<-split(haiti,haiti$Key, drop = TRUE)
write.table(haiti, file="haiti.csv",sep=",",row.names=F)

cayman<-data.table()
cayman<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Cayman')
cayman_list<-split(cayman, cayman$Key, drop = TRUE)
write.table(cayman, file="cayman.csv",sep=",",row.names=F)

anguilla<-data.table()
anguilla<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Anguilla')
anguilla_list<-split(anguilla, anguilla$Key, drop = TRUE)
write.table(anguilla, file="anguilla.csv",sep=",",row.names=F)

guatemala<-data.table()
guatemala<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Guatemala')
guatemala_list<-split(guatemala, guatemala$Key, drop = TRUE)
write.table(guatemala, file="guatemala.csv",sep=",",row.names=F)

montserrat<-data.table()
montserrat<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Montserrat')
montserrat_list<-split(montserrat, montserrat$Key, drop = TRUE)
write.table(montserrat, file="montserrat.csv",sep=",",row.names=F)

dominica<-data.table()
dominica<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Dominica')
dominica_list<-split(dominica, dominica$Key, drop = TRUE)
write.table(dominica, file="dominica.csv",sep=",",row.names=F)


stlucia<-data.table()
stlucia<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Saint Lucia')
stlucia_list<-split(stlucia, stlucia$Key, drop = TRUE)
write.table(stlucia, file="stlucia.csv",sep=",",row.names=F)

aruba<-data.table()
aruba<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Aruba')
aruba_list<-split(aruba, aruba$Key, drop = TRUE)
write.table(aruba, file="aruba.csv",sep=",",row.names=F)

tt<-data.table()
tt<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Trinidad and Tobago')
tt_list<-split(tt, tt$Key, drop = TRUE)
write.table(tt, file="tt.csv",sep=",",row.names=F)

guyana<-data.table()
guyana<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Guyana')
guyana_list<-split(guyana, guyana$Key, drop = TRUE)
write.table(guyana, file="guyana.csv",sep=",",row.names=F)

cuba<-data.table()
cuba<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Cuba')
cuba_list<-split(cuba, cuba$Key, drop = TRUE)
write.table(cuba, file="cuba.csv",sep=",",row.names=F)

bvi<-data.table()
bvi<-subset(cropset_at_full,cropset_at_full$`Country Name`=='British Virgin Islands')
bvi_list<-split(bvi, bvi$Key, drop = TRUE)
write.table(bvi, file="bvi.csv",sep=",",row.names=F)

belize<-data.table()
belize<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Belize')
belize_list<-split(belize, belize$Key, drop = TRUE)
write.table(belize, file="belize.csv",sep=",",row.names=F)

stmartin<-data.table()
stmartin<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Saint Martin')
stmartin_list<-split(stmartin, stmartin$Key, drop = TRUE)
write.table(stmartin, file="stmartin.csv",sep=",",row.names=F)

AB<-data.table()
AB<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Antigua and Barbuda')
AB_list<-split(AB, AB$Key, drop = TRUE)
write.table(AB, file="AB.csv",sep=",",row.names=F)

honduras<-data.table()
honduras<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Honduras')
honduras_list<-split(honduras, honduras$Key)
write.table(honduras, file="honduras.csv",sep=",",row.names=F)

nicaragua<-data.table()
nicaragua<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Nicaragua')
nicaragua_list<-split(nicaragua, nicaragua$Key, drop = TRUE)
write.table(nicaragua, file="nicaragua.csv",sep=",",row.names=F)

svg<-data.table()
svg<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Saint Vincent and the Grenadines')
svg_list<-split(svg, svg$Key, drop = TRUE)
write.table(svg, file="svg.csv",sep=",",row.names=F)

grenada<-data.table()
grenada<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Grenada')
grenada_list<-split(grenada, grenada$Key, drop = TRUE)
write.table(grenada, file="grenada.csv",sep=",",row.names=F)

costa_rica<-data.table()
costa_rica<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Costa Rica')
costa_rica_list<-split(costa_rica, costa_rica$Key, drop = TRUE)
write.table(costa_rica, file="costa_rica.csv",sep=",",row.names=F)

turks<-data.table()
turks<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Turks and Caicos Islands')
turks_list<-split(turks, turks$Key, drop = TRUE)
write.table(turks, file="turks.csv",sep=",",row.names=F)

domrep<-data.table()
domrep<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Dominican Republic')
domrep_list<-split(domrep, domrep$Key, drop = TRUE)
write.table(domrep, file="domrep.csv",sep=",",row.names=F)

jamaica<-data.table()
jamaica<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Jamaica')
jamaica_list<-split(jamaica, jamaica$Key, drop= TRUE)
write.table(jamaica, file="jamaica.csv",sep=",",row.names=F)

virgin<-data.table()
virgin<-subset(cropset_at_full,cropset_at_full$`Country Name`=='United States Virgin Islands')
virgin_list<-split(virgin, virgin$Key, drop = TRUE)
write.table(virgin, file="virgin.csv",sep=",",row.names=F)

skn<-data.table()
skn<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Saint Kitts and Nevis')
skn_list<-split(skn, skn$Key, drop = TRUE)
write.table(skn, file="skn.csv",sep=",",row.names=F)

elsalvador<-data.table()
elsalvador<-subset(cropset_at_full,cropset_at_full$`Country Name`=='El Salvador')
elsalvador_list<-split(elsalvador, elsalvador$Key, drop = TRUE)
write.table(elsalvador, file="elsalvador.csv",sep=",",row.names=F)

barbados<-data.table()
barbados<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Barbados')
barbados_list<-split(barbados, barbados$Key, drop = TRUE)
write.table(barbados, file="barbados.csv",sep=",",row.names=F)

columbia<-data.table()
columbia<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Columbia')
columbia_list<-split(columbia, columbia$Key, drop = TRUE)
write.table(columbia, file="columbia.csv",sep=",",row.names=F)

panama<-data.table()
panama<-subset(cropset_at_full,cropset_at_full$`Country Name`=='Panama')
panama_list<-split(panama, panama$Key, drop = TRUE)
write.table(panama, file="panama.csv",sep=",",row.names=F)



#########################################
#with the above, we now have a clear 
#collection of hurricanes and their
#respective distances from each centroid
#in each relevant country
#########################################


###########################################################Formulate data in Stormwin model format########################################################
jamaica <-fread("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Paper Data\\jamaica.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#storm Jamaica
# stwind_jam<-jamaica[9:12]
# stwind_jam$lat<-jamaica[6]
# stwind_jam$long<-jamaica[7]
# stwind_jam$wind<-jamaica[8]


#tracking<-merge(al,jamaica, By= "Key",all=T)
unique(jamaica$Name)  #16 hurricanes
h1j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="ALLEN")
h1j<-as.data.table(h1j)

#Correct day
for(f in 1: nrow(h1j))
{
  if(nchar(h1j$day[f])<2){
    h1j$day[f]<-paste0("0",h1j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h1j))
{
  if(nchar(h1j$month[f])<2){
    h1j$month[f]<-paste0("0",h1j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h1j$time<-round(h1j$time, 0)

for(f in 1: nrow(h1j))
{
  if(nchar(h1j$time[f])<2){
    h1j$time[f]<-paste0("0",h1j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h1j))
{
  if(nchar(h1j[f])<2){
    h1j$time[f]<-paste0("0",h1j$time[f])
  }
  else{h1j$time[f]<-paste0(h1j$time[f],"00")}
}



h1j$date<-paste0(h1j$year,h1j$month,h1j$day, h1j$time)
h1j<-as.data.frame(h1j)
h1j[1:4]<-NULL
h1j[4:9]<-NULL
#Reorder the dataframe
h1j<-h1j[c(4,1,2,3)]
#corecting date format for hurricanes
h1j$date<-as.character(h1j$date)



#rename columns because the package is strict on the names inputted
colnames(h1j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h1j$wind<-convert_wind_speed(h1j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Jamaica

#16 hurricanes
h2j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="GILBERT")
h2j<-as.data.table(h2j)

#Correct day
for(f in 1: nrow(h2j))
{
  if(nchar(h2j$day[f])<2){
    h2j$day[f]<-paste0("0",h2j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h2j))
{
  if(nchar(h2j$month[f])<2){
    h2j$month[f]<-paste0("0",h2j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h2j$time<-round(h2j$time, 0)

for(f in 1: nrow(h2j))
{
  if(nchar(h2j$time[f])<2){
    h2j$time[f]<-paste0("0",h2j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h2j))
{
  if(nchar(h2j$time[f])<2){
    h2j$time[f]<-paste0("0",h2j$time[f])
  }
  else{h2j$time[f]<-paste0(h2j$time[f],"00")}
}



h2j$date<-paste0(h2j$year,h2j$month,h2j$day, h2j$time)
h2j<-as.data.frame(h2j)
h2j[1:4]<-NULL
h2j[4:9]<-NULL
#Reorder the dataframe
h2j<-h2j[c(4,1,2,3)]
#corecting date format for hurricanes
h2j$date<-as.character(h2j$date)



#rename columns because the package is strict on the names inputted
colnames(h2j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h2j$wind<-convert_wind_speed(h2j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 3<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Jamaica

#16 hurricanes
h3j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="MITCH")
h3j<-as.data.table(h3j)

#Correct day
for(f in 1: nrow(h3j))
{
  if(nchar(h3j$day[f])<2){
    h3j$day[f]<-paste0("0",h3j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h3j))
{
  if(nchar(h3j$month[f])<2){
    h3j$month[f]<-paste0("0",h3j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h3j$time<-round(h3j$time, 0)

for(f in 1: nrow(h3j))
{
  if(nchar(h3j$time[f])<2){
    h3j$time[f]<-paste0("0",h3j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h3j))
{
  if(nchar(h3j$time[f])<2){
    h3j$time[f]<-paste0("0",h3j$time[f])
  }
  else{h3j$time[f]<-paste0(h3j$time[f],"00")}
}



h3j$date<-paste0(h3j$year,h3j$month,h3j$day, h3j$time)
h3j<-as.data.frame(h3j)
h3j[1:4]<-NULL
h3j[4:9]<-NULL
#Reorder the dataframe
h3j<-h3j[c(4,1,2,3)]
#corecting date format for hurricanes
h3j$date<-as.character(h3j$date)



#rename columns because the package is strict on the names inputted
colnames(h3j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h3j$wind<-convert_wind_speed(h3j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 3<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 4<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Jamaica

#16 hurricanes
h4j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="IVAN")
h4j<-as.data.table(h4j)

#Correct day
for(f in 1: nrow(h4j))
{
  if(nchar(h4j$day[f])<2){
    h4j$day[f]<-paste0("0",h4j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h4j))
{
  if(nchar(h4j$month[f])<2){
    h4j$month[f]<-paste0("0",h4j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h4j$time<-round(h4j$time, 0)

for(f in 1: nrow(h4j))
{
  if(nchar(h4j$time[f])<2){
    h4j$time[f]<-paste0("0",h4j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h4j))
{
  if(nchar(h4j$time[f])<2){
    h4j$time[f]<-paste0("0",h4j$time[f])
  }
  else{h4j$time[f]<-paste0(h4j$time[f],"00")}
}



h4j$date<-paste0(h4j$year,h4j$month,h4j$day, h4j$time)
h4j<-as.data.frame(h4j)
h4j[1:4]<-NULL
h4j[4:9]<-NULL
#Reorder the dataframe
h4j<-h4j[c(4,1,2,3)]
#corecting date format for hurricanes
h4j$date<-as.character(h4j$date)



#rename columns because the package is strict on the names inputted
colnames(h4j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h4j$wind<-convert_wind_speed(h4j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 4<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 5<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h5j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="DENNIS")
h5j<-as.data.table(h5j)

#Correct day
for(f in 1: nrow(h5j))
{
  if(nchar(h5j$day[f])<2){
    h5j$day[f]<-paste0("0",h5j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h5j))
{
  if(nchar(h5j$month[f])<2){
    h5j$month[f]<-paste0("0",h5j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h5j$time<-round(h5j$time, 0)

for(f in 1: nrow(h5j))
{
  if(nchar(h5j$time[f])<2){
    h5j$time[f]<-paste0("0",h5j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h5j))
{
  if(nchar(h5j$time[f])<2){
    h5j$time[f]<-paste0("0",h5j$time[f])
  }
  else{h5j$time[f]<-paste0(h5j$time[f],"00")}
}



h5j$date<-paste0(h5j$year,h5j$month,h5j$day, h5j$time)
h5j<-as.data.frame(h5j)
h5j[1:4]<-NULL
h5j[4:9]<-NULL
#Reorder the dataframe
h5j<-h5j[c(4,1,2,3)]
#corecting date format for hurricanes
h5j$date<-as.character(h5j$date)



#rename columns because the package is strict on the names inputted
colnames(h5j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h5j$wind<-convert_wind_speed(h5j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 5<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 6-8<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
emily<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="EMILY")
emlist<-list()
emlist<-split(emily, emily$year, drop = TRUE)
h6j<-emlist[[1]]
h6j<-as.data.table(h6j)
h7j<-emlist[[2]]
h7j<-as.data.table(h7j)
h8j<-emlist[[3]]
h8j<-as.data.table(h8j)




#Correct day
for(f in 1: nrow(h6j))
{
  if(nchar(h6j$day[f])<2){
    h6j$day[f]<-paste0("0",h6j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h6j))
{
  if(nchar(h6j$month[f])<2){
    h6j$month[f]<-paste0("0",h6j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h6j$time<-round(h6j$time, 0)

for(f in 1: nrow(h6j))
{
  if(nchar(h6j$time[f])<2){
    h6j$time[f]<-paste0("0",h6j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h6j))
{
  if(nchar(h6j$time[f])<2){
    h6j$time[f]<-paste0("0",h6j$time[f])
  }
  else{h6j$time[f]<-paste0(h6j$time[f],"00")}
}



h6j$date<-paste0(h6j$year,h6j$month,h6j$day, h6j$time)
h6j<-as.data.frame(h6j)
h6j[1:4]<-NULL
h6j[4:9]<-NULL
#Reorder the dataframe
h6j<-h6j[c(4,1,2,3)]
#corecting date format for hurricanes
h6j$date<-as.character(h6j$date)



#rename columns because the package is strict on the names inputted
colnames(h6j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h6j$wind<-convert_wind_speed(h6j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 6<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 7<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# h7j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="WILMA")
# h7j<-as.data.table(h7j)

#Correct day
for(f in 1: nrow(h7j))
{
  if(nchar(h7j$day[f])<2){
    h7j$day[f]<-paste0("0",h7j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h7j))
{
  if(nchar(h7j$month[f])<2){
    h7j$month[f]<-paste0("0",h7j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h7j$time<-round(h7j$time, 0)

for(f in 1: nrow(h7j))
{
  if(nchar(h7j$time[f])<2){
    h7j$time[f]<-paste0("0",h7j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h7j))
{
  if(nchar(h7j$time[f])<2){
    h7j$time[f]<-paste0("0",h7j$time[f])
  }
  else{h7j$time[f]<-paste0(h7j$time[f],"00")}
}



h7j$date<-paste0(h7j$year,h7j$month,h7j$day, h7j$time)
h7j<-as.data.frame(h7j)
h7j[1:4]<-NULL
h7j[4:9]<-NULL
#Reorder the dataframe
h7j<-h7j[c(4,1,2,3)]
#corecting date format for hurricanes
h7j$date<-as.character(h7j$date)



#rename columns because the package is strict on the names inputted
colnames(h7j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h7j$wind<-convert_wind_speed(h7j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 7<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 8<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# h8j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="DEAN")
# h8j<-as.data.table(h8j)

#Correct day
for(f in 1: nrow(h8j))
{
  if(nchar(h8j$day[f])<2){
    h8j$day[f]<-paste0("0",h8j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h8j))
{
  if(nchar(h8j$month[f])<2){
    h8j$month[f]<-paste0("0",h8j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h8j$time<-round(h8j$time, 0)

for(f in 1: nrow(h8j))
{
  if(nchar(h8j$time[f])<2){
    h8j$time[f]<-paste0("0",h8j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h8j))
{
  if(nchar(h8j$time[f])<2){
    h8j$time[f]<-paste0("0",h8j$time[f])
  }
  else{h8j$time[f]<-paste0(h8j$time[f],"00")}
}



h8j$date<-paste0(h8j$year,h8j$month,h8j$day, h8j$time)
h8j<-as.data.frame(h8j)
h8j[1:4]<-NULL
h8j[4:9]<-NULL
#Reorder the dataframe
h8j<-h8j[c(4,1,2,3)]
#corecting date format for hurricanes
h8j$date<-as.character(h8j$date)



#rename columns because the package is strict on the names inputted
colnames(h8j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h8j$wind<-convert_wind_speed(h8j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 6-8<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 9<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h9j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="WILMA")
h9j<-as.data.table(h9j)

#Correct day
for(f in 1: nrow(h9j))
{
  if(nchar(h9j$day[f])<2){
    h9j$day[f]<-paste0("0",h9j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h9j))
{
  if(nchar(h9j$month[f])<2){
    h9j$month[f]<-paste0("0",h9j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h9j$time<-round(h9j$time, 0)

for(f in 1: nrow(h9j))
{
  if(nchar(h9j$time[f])<2){
    h9j$time[f]<-paste0("0",h9j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h9j))
{
  if(nchar(h9j$time[f])<2){
    h9j$time[f]<-paste0("0",h9j$time[f])
  }
  else{h9j$time[f]<-paste0(h9j$time[f],"00")}
}



h9j$date<-paste0(h9j$year,h9j$month,h9j$day, h9j$time)
h9j<-as.data.frame(h9j)
h9j[1:4]<-NULL
h9j[4:9]<-NULL
#Reorder the dataframe
h9j<-h9j[c(4,1,2,3)]
#corecting date format for hurricanes
h9j$date<-as.character(h9j$date)



#rename columns because the package is strict on the names inputted
colnames(h9j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h9j$wind<-convert_wind_speed(h9j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 9<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 10<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


h10j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="DEAN")
h10j<-as.data.table(h10j)

#Correct day
for(f in 1: nrow(h10j))
{
  if(nchar(h10j$day[f])<2){
    h10j$day[f]<-paste0("0",h10j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h10j))
{
  if(nchar(h10j$month[f])<2){
    h10j$month[f]<-paste0("0",h10j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h10j$time<-round(h10j$time, 0)

for(f in 1: nrow(h10j))
{
  if(nchar(h10j$time[f])<2){
    h10j$time[f]<-paste0("0",h10j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h10j))
{
  if(nchar(h10j$time[f])<2){
    h10j$time[f]<-paste0("0",h10j$time[f])
  }
  else{h10j$time[f]<-paste0(h10j$time[f],"00")}
}



h10j$date<-paste0(h10j$year,h10j$month,h10j$day, h10j$time)
h10j<-as.data.frame(h10j)
h10j[1:4]<-NULL
h10j[4:9]<-NULL
#Reorder the dataframe
h10j<-h10j[c(4,1,2,3)]
#corecting date format for hurricanes
h10j$date<-as.character(h10j$date)



#rename columns because the package is strict on the names inputted
colnames(h10j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h10j$wind<-convert_wind_speed(h10j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 10<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 11-13<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
felix<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="FELIX")
flist<-list()
flist<-split(felix, felix$year, drop = TRUE)
h11j<-flist[[1]]
h12j<-flist[[2]]
h13j<-flist[[3]]

h11j<-as.data.table(h11j)
h12j<as.data.table(h12j)
h13j<-as.data.table(h13j)


#Correct day
for(f in 1: nrow(h11j))
{
  if(nchar(h11j$day[f])<2){
    h11j$day[f]<-paste0("0",h11j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h11j))
{
  if(nchar(h11j$month[f])<2){
    h11j$month[f]<-paste0("0",h11j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h11j$time<-round(h11j$time, 0)

for(f in 1: nrow(h11j))
{
  if(nchar(h11j$time[f])<2){
    h11j$time[f]<-paste0("0",h11j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h11j))
{
  if(nchar(h11j$time[f])<2){
    h11j$time[f]<-paste0("0",h11j$time[f])
  }
  else{h11j$time[f]<-paste0(h11j$time[f],"00")}
}



h11j$date<-paste0(h11j$year,h11j$month,h11j$day, h11j$time)
h11j<-as.data.frame(h11j)
h11j[1:4]<-NULL
h11j[4:9]<-NULL
#Reorder the dataframe
h11j<-h11j[c(4,1,2,3)]
#corecting date format for hurricanes
h11j$date<-as.character(h11j$date)



#rename columns because the package is strict on the names inputted
colnames(h11j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h11j$wind<-convert_wind_speed(h11j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#################################################################STORM 12################################################################################
#Correct day
for(f in 1: nrow(h12j))
{
  if(nchar(h12j$day[f])<2){
    h12j$day[f]<-paste0("0",h12j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h12j))
{
  if(nchar(h12j$month[f])<2){
    h12j$month[f]<-paste0("0",h12j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h12j$time<-round(h12j$time, 0)

for(f in 1: nrow(h12j))
{
  if(nchar(h12j$time[f])<2){
    h12j$time[f]<-paste0("0",h12j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h12j))
{
  if(nchar(h12j$time[f])<2){
    h12j$time[f]<-paste0("0",h12j$time[f])
  }
  else{h12j$time[f]<-paste0(h12j$time[f],"00")}
}



h12j$date<-paste0(h12j$year,h12j$month,h12j$day, h12j$time)
h12j<-as.data.frame(h12j)
h12j[1:4]<-NULL
h12j[4:9]<-NULL
#Reorder the dataframe
h12j<-h12j[c(4,1,2,3)]
#corecting date format for hurricanes
h12j$date<-as.character(h12j$date)



#rename columns because the package is strict on the names inputted
colnames(h12j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h12j$wind<-convert_wind_speed(h12j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)
#########################################################################################################################################################

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 13<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Correct day
for(f in 1: nrow(h13j))
{
  if(nchar(h13j$day[f])<2){
    h13j$day[f]<-paste0("0",h13j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h13j))
{
  if(nchar(h13j$month[f])<2){
    h13j$month[f]<-paste0("0",h13j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h13j$time<-round(h13j$time, 0)

for(f in 1: nrow(h13j))
{
  if(nchar(h13j$time[f])<2){
    h13j$time[f]<-paste0("0",h13j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h13j))
{
  if(nchar(h13j$time[f])<2){
    h13j$time[f]<-paste0("0",h13j$time[f])
  }
  else{h13j$time[f]<-paste0(h13j$time[f],"00")}
}



h13j$date<-paste0(h13j$year,h13j$month,h13j$day, h13j$time)
h13j<-as.data.frame(h13j)
h13j[1:4]<-NULL
h13j[4:9]<-NULL
#Reorder the dataframe
h13j<-h13j[c(4,1,2,3)]
#corecting date format for hurricanes
h13j$date<-as.character(h13j$date)



#rename columns because the package is strict on the names inputted
colnames(h13j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h13j$wind<-convert_wind_speed(h13j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 11-13<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 14-15<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# h10j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="GUSTAV")
# h10j<-as.data.table(h10j)

gustav<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="GUSTAV")
glist<-list()
glist<-split(gustav, gustav$year, drop = TRUE)
h14j<-glist[[1]]
h15j<-glist[[2]]

h14j<-as.data.table(h14j)
h15j<-as.data.table(h15j)

#Correct day
for(f in 1: nrow(h14j))
{
  if(nchar(h14j$day[f])<2){
    h14j$day[f]<-paste0("0",h14j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h14j))
{
  if(nchar(h14j$month[f])<2){
    h14j$month[f]<-paste0("0",h14j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h14j$time<-round(h14j$time, 0)

for(f in 1: nrow(h14j))
{
  if(nchar(h14j$time[f])<2){
    h14j$time[f]<-paste0("0",h14j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h14j))
{
  if(nchar(h14j$time[f])<2){
    h14j$time[f]<-paste0("0",h14j$time[f])
  }
  else{h14j$time[f]<-paste0(h14j$time[f],"00")}
}



h14j$date<-paste0(h14j$year,h14j$month,h14j$day, h14j$time)
h14j<-as.data.frame(h14j)
h14j[1:4]<-NULL
h14j[4:9]<-NULL
#Reorder the dataframe
h14j<-h14j[c(4,1,2,3)]
#corecting date format for hurricanes
h14j$date<-as.character(h14j$date)



#rename columns because the package is strict on the names inputted
colnames(h14j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h14j$wind<-convert_wind_speed(h14j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


############################################################################STORM 15####################################################################

for(f in 1: nrow(h15j))
{
  if(nchar(h15j$day[f])<2){
    h15j$day[f]<-paste0("0",h15j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h15j))
{
  if(nchar(h15j$month[f])<2){
    h15j$month[f]<-paste0("0",h15j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h15j$time<-round(h15j$time, 0)

for(f in 1: nrow(h15j))
{
  if(nchar(h15j$time[f])<2){
    h15j$time[f]<-paste0("0",h15j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h15j))
{
  if(nchar(h15j$time[f])<2){
    h15j$time[f]<-paste0("0",h15j$time[f])
  }
  else{h15j$time[f]<-paste0(h15j$time[f],"00")}
}



h15j$date<-paste0(h15j$year,h15j$month,h15j$day, h15j$time)
h15j<-as.data.frame(h15j)
h15j[1:4]<-NULL
h15j[4:9]<-NULL
#Reorder the dataframe
h15j<-h15j[c(4,1,2,3)]
#corecting date format for hurricanes
h15j$date<-as.character(h15j$date)



#rename columns because the package is strict on the names inputted
colnames(h15j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h15j$wind<-convert_wind_speed(h15j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 14-15<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 16<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h16j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="IKE")
h16j<-as.data.table(h16j)

#Correct day
for(f in 1: nrow(h16j))
{
  if(nchar(h16j$day[f])<2){
    h16j$day[f]<-paste0("0",h16j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h16j))
{
  if(nchar(h16j$month[f])<2){
    h16j$month[f]<-paste0("0",h16j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h16j$time<-round(h16j$time, 0)

for(f in 1: nrow(h16j))
{
  if(nchar(h16j$time[f])<2){
    h16j$time[f]<-paste0("0",h16j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h16j))
{
  if(nchar(h16j$time[f])<2){
    h16j$time[f]<-paste0("0",h16j$time[f])
  }
  else{h16j$time[f]<-paste0(h16j$time[f],"00")}
}



h16j$date<-paste0(h16j$year,h16j$month,h16j$day, h16j$time)
h16j<-as.data.frame(h16j)
h16j[1:4]<-NULL
h16j[4:9]<-NULL
#Reorder the dataframe
h16j<-h16j[c(4,1,2,3)]
#corecting date format for hurricanes
h16j$date<-as.character(h16j$date)



#rename columns because the package is strict on the names inputted
colnames(h16j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h16j$wind<-convert_wind_speed(h16j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 16<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 17<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h17j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="PALOMA")
h17j<-as.data.table(h17j)

#Correct day
for(f in 1: nrow(h17j))
{
  if(nchar(h17j$day[f])<2){
    h17j$day[f]<-paste0("0",h17j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h17j))
{
  if(nchar(h17j$month[f])<2){
    h17j$month[f]<-paste0("0",h17j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h17j$time<-round(h17j$time, 0)

for(f in 1: nrow(h17j))
{
  if(nchar(h17j$time[f])<2){
    h17j$time[f]<-paste0("0",h17j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h17j))
{
  if(nchar(h17j$time[f])<2){
    h17j$time[f]<-paste0("0",h17j$time[f])
  }
  else{h17j$time[f]<-paste0(h17j$time[f],"00")}
}



h17j$date<-paste0(h17j$year,h17j$month,h17j$day, h17j$time)
h17j<-as.data.frame(h17j)
h17j[1:4]<-NULL
h17j[4:9]<-NULL
#Reorder the dataframe
h17j<-h17j[c(4,1,2,3)]
#corecting date format for hurricanes
h17j$date<-as.character(h17j$date)



#rename columns because the package is strict on the names inputted
colnames(h17j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h17j$wind<-convert_wind_speed(h17j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 17<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 18<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h18j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="SANDY")
h18j<-as.data.table(h18j)

#Correct day
for(f in 1: nrow(h18j))
{
  if(nchar(h18j$day[f])<2){
    h18j$day[f]<-paste0("0",h18j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h18j))
{
  if(nchar(h18j$month[f])<2){
    h18j$month[f]<-paste0("0",h18j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h18j$time<-round(h18j$time, 0)

for(f in 1: nrow(h18j))
{
  if(nchar(h18j$time[f])<2){
    h18j$time[f]<-paste0("0",h18j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h18j))
{
  if(nchar(h18j$time[f])<2){
    h18j$time[f]<-paste0("0",h18j$time[f])
  }
  else{h18j$time[f]<-paste0(h18j$time[f],"00")}
}



h18j$date<-paste0(h18j$year,h18j$month,h18j$day, h18j$time)
h18j<-as.data.frame(h18j)
h18j[1:4]<-NULL
h18j[4:9]<-NULL
#Reorder the dataframe
h18j<-h18j[c(4,1,2,3)]
#corecting date format for hurricanes
h18j$date<-as.character(h18j$date)



#rename columns because the package is strict on the names inputted
colnames(h18j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h18j$wind<-convert_wind_speed(h18j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 18<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 19<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h19j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="MATTHEW")
h19j<-as.data.table(h19j)

#Correct day
for(f in 1: nrow(h19j))
{
  if(nchar(h19j$day[f])<2){
    h19j$day[f]<-paste0("0",h19j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h19j))
{
  if(nchar(h19j$month[f])<2){
    h19j$month[f]<-paste0("0",h19j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h19j$time<-round(h19j$time, 0)

for(f in 1: nrow(h19j))
{
  if(nchar(h19j$time[f])<2){
    h19j$time[f]<-paste0("0",h19j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h19j))
{
  if(nchar(h19j$time[f])<2){
    h19j$time[f]<-paste0("0",h19j$time[f])
  }
  else{h19j$time[f]<-paste0(h19j$time[f],"00")}
}



h19j$date<-paste0(h19j$year,h19j$month,h19j$day, h19j$time)
h19j<-as.data.frame(h19j)
h19j[1:4]<-NULL
h19j[4:9]<-NULL
#Reorder the dataframe
h19j<-h19j[c(4,1,2,3)]
#corecting date format for hurricanes
h19j$date<-as.character(h19j$date)



#rename columns because the package is strict on the names inputted
colnames(h19j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h19j$wind<-convert_wind_speed(h19j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 19<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 20<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h20j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="IRMA")
h20j<-as.data.table(h20j)

#Correct day
for(f in 1: nrow(h20j))
{
  if(nchar(h20j$day[f])<2){
    h20j$day[f]<-paste0("0",h20j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h20j))
{
  if(nchar(h20j$month[f])<2){
    h20j$month[f]<-paste0("0",h20j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h20j$time<-round(h20j$time, 0)

for(f in 1: nrow(h20j))
{
  if(nchar(h20j$time[f])<2){
    h20j$time[f]<-paste0("0",h20j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h20j))
{
  if(nchar(h20j$time[f])<2){
    h20j$time[f]<-paste0("0",h20j$time[f])
  }
  else{h20j$time[f]<-paste0(h20j$time[f],"00")}
}



h20j$date<-paste0(h20j$year,h20j$month,h20j$day, h20j$time)
h20j<-as.data.frame(h20j)
h20j[1:4]<-NULL
h20j[4:9]<-NULL
#Reorder the dataframe
h20j<-h20j[c(4,1,2,3)]
#corecting date format for hurricanes
h20j$date<-as.character(h20j$date)



#rename columns because the package is strict on the names inputted
colnames(h20j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h20j$wind<-convert_wind_speed(h20j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 20<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 21<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h21j<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="DAVID")
h21j<-as.data.table(h21j)

#Correct day
for(f in 1: nrow(h21j))
{
  if(nchar(h21j$day[f])<2){
    h21j$day[f]<-paste0("0",h21j$day[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h21j))
{
  if(nchar(h21j$month[f])<2){
    h21j$month[f]<-paste0("0",h21j$month[f])
  }
  #else{ stwind_jam$day[f]<-paste0(stwind_jam$day[f],"00")}
}

#Correct time

h21j$time<-round(h21j$time, 0)

for(f in 1: nrow(h21j))
{
  if(nchar(h21j$time[f])<2){
    h21j$time[f]<-paste0("0",h21j$time[f])
  }
  #else{ stwind_jam$time[f]<-paste0(stwind_jam$time[f],"00")}
}


for(f in 1: nrow(h21j))
{
  if(nchar(h21j$time[f])<2){
    h21j$time[f]<-paste0("0",h21j$time[f])
  }
  else{h21j$time[f]<-paste0(h21j$time[f],"00")}
}



h21j$date<-paste0(h21j$year,h21j$month,h21j$day, h21j$time)
h21j<-as.data.frame(h21j)
h21j[1:4]<-NULL
h21j[4:9]<-NULL
#Reorder the dataframe
h21j<-h21j[c(4,1,2,3)]
#corecting date format for hurricanes
h21j$date<-as.character(h21j$date)



#rename columns because the package is strict on the names inputted
colnames(h21j)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h21j$wind<-convert_wind_speed(h21j$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 21<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Location<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
Cen90_crop<-fread("C://Users//goulb/OneDrive//Desktop//Research 2018//Census Data//lacd1990//Cen_RDG.csv")
jam<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_JAM_shp", layer = "gadm36_JAM_1")

jmbox<-jam@bbox
Cen90_crop$location<-"NA"
Cen90_crop[(which(Cen90_crop$Long>=-(jmbox[1,2]) & Cen90_crop$Long<=-(jmbox[1,1]) & Cen90_crop$Lat>=(jmbox[2,1]) & Cen90_crop$Lat<=(jmbox[2,2]))),5]<-"Jamaica"
try_ljam<-subset(Cen90_crop, Cen90_crop$location=="Jamaica")

try_ljam[,1]<-NULL
try_ljam[,4]<-NULL
try_ljam<-as.data.frame(try_ljam)
try_ljam<-try_ljam[c(3,2,1)]
try_ljam$glon<-(try_ljam$glon)
try_ljam$glon<-(-try_ljam$glon)
colnames(try_ljam)<-c("gridid","glat","glon")

small_jam<-head(try_ljam)





# stloc_jam<-jamaica[1]
# stloc_jam$lat<-jamaica[15]
# stloc_jam$long<-jamaica[14]
# #Correct the minus sign in the longitude
# stloc_jam[3]<--(stloc_jam[3])
# 




#Get wind speed estimates


#Testing using data provided by the package

data("floyd_tracks")
head(floyd_tracks)

floyd_winds <- get_grid_winds(hurr_track = floyd_tracks, grid_df = county_points)
#works just fine


#will shorten my dataset so that I can test my data
#my current concern is that I have the same location for multiple years in the sam data set.
#Im not sure if the package will calculate only fot the relavant years
# 
# try_wjam<-head(stwind_jam)
# colnames(try_wjam)<-c("date","latitude","longitude","wind")
# #This shortened dataset refusese to reame the columns so I have to transfer the columns ot another dataframe
# 
# test_tracks<-as.data.table(try_wjam$date)
# colnames(test_tracks)<-c("date")
# test_tracks$latitude<-try_wjam$latitude
# test_tracks$longitude<-try_wjam$longitude
# test_tracks$wind<-try_wjam$wind





wind_jam_try<-get_grid_winds(hurr_track= test_tracks, grid_df = small_jam)
#Giving an error so I need to check if the problem is due to the fact that I dont have all the locations separated by ID.
#Ill try using the original points obtained from the tif file without the separation on grounds of hurricane being 
#500km or less from location of interes.


#>>>>Using the original data from the tif file works. I will now run the code on the full set of locations for Jamaica
#So using the original hurricane track data we are able to calclate the wind speed experienced by each locality in the dataset. 



wind_jam1<-get_grid_winds(hurr_track= h1j, grid_df = try_ljam) #Hurricane 1 for Jamaica
wind_jam2<-get_grid_winds(hurr_track= h2j, grid_df = try_ljam) #Hurricane 2 for Jamaica
wind_jam3<-get_grid_winds(hurr_track= h3j, grid_df = try_ljam) #Hurricane 3 for Jamaica

#Four is not working. I will correct it by dropping the last two rows where the times were the same which should not have been in the first place.  
#cutting the dataset into two  
h4j<-h4j[-c(90:91), ]
wind_jam4<-get_grid_winds(hurr_track= h4j, grid_df = try_ljam) #Hurricane 4 for Jamaica
wind_jam5<-get_grid_winds(hurr_track= h5j, grid_df = try_ljam) #Hurricane 5 for Jamaica
#The maximum wind speed is measure in meters per second.
wind_jam6<-get_grid_winds(hurr_track= head(h6j), grid_df = try_ljam) #Hurricane 6 for Jamaica
wind_jam7<-get_grid_winds(hurr_track= h7j, grid_df = try_ljam) #Hurricane 7 for Jamaica
wind_jam8<-get_grid_winds(hurr_track= h8j, grid_df = try_ljam) #Hurricane 8 for Jamaica
wind_jam9<-get_grid_winds(hurr_track= h9j, grid_df = try_ljam) #Hurricane 9 for Jamaica
wind_jam10<-get_grid_winds(hurr_track= h10j, grid_df = try_ljam) #Hurricane 10 for Jamaica
wind_jam11<-get_grid_winds(hurr_track= h11j, grid_df = try_ljam) #Hurricane 11 for Jamaica
wind_jam12<-get_grid_winds(hurr_track= h12j, grid_df = try_ljam) #Hurricane 12 for Jamaica
wind_jam13<-get_grid_winds(hurr_track= h13j, grid_df = try_ljam) #Hurricane 13 for Jamaica
wind_jam14<-get_grid_winds(hurr_track= h14j, grid_df = try_ljam) #Hurricane 14 for Jamaica
wind_jam15<-get_grid_winds(hurr_track= h15j, grid_df = try_ljam) #Hurricane 15 for Jamaica
wind_jam16<-get_grid_winds(hurr_track= h16j, grid_df = try_ljam) #Hurricane 16 for Jamaica
wind_jam17<-get_grid_winds(hurr_track= h17j, grid_df = try_ljam) #Hurricane 17 for Jamaica
wind_jam18<-get_grid_winds(hurr_track= h18j, grid_df = try_ljam) #Hurricane 18 for Jamaica
wind_jam19<-get_grid_winds(hurr_track= h19j, grid_df = try_ljam) #Hurricane 19 for Jamaica
wind_jam20<-get_grid_winds(hurr_track= h20j, grid_df = try_ljam) #Hurricane 20 for Jamaica
wind_jam21<-get_grid_winds(hurr_track= h21j, grid_df = try_ljam) #Hurricane 21 for Jamaica

speed_list<-list(wind_jam1, wind_jam2, wind_jam3, wind_jam4, wind_jam5, wind_jam6, wind_jam6, wind_jam7, wind_jam8, wind_jam9, wind_jam10, wind_jam11, wind_jam12, wind_jam13, wind_jam14, wind_jam15, wind_jam16, wind_jam17, wind_jam18, wind_jam19, wind_jam20, wind_jam21)
save(speed_list, file="speed_list.RData")

load("speed_list.RData")

adts_crop90<-fread("C://Users//goulb//OneDrive//Desktop//Research 2018//Paper Data//adts_crop90.csv")


#Trouble with the track files for H-6, 9, 10,11 since when the hurricane names were reused. Hurricane Gustav in particular was used in a number of years,
#In particular, the name was used in 1984,1990,1996,2002, and 2008. We have to separate the storms in order for the wind speed experienced to be measured.



#The problems were resolved and the wind speeds were calculated accordingly. 
#Now we will proceed to gather the wind speeds that were recorded within 500km of the storm center. This has to be done since the windspeed model
#to this point has not provided a means by which to separate by distance from storm. Our initial distance calculations cannot be used directly but the 
#ID assignments thereof will prove beneficial in identiying the relevant tract data. 
###########################################################################################################################################################

#######################################################Using ID to capture locations affected within 500km###########################################################

colnames(wind_jam1)[1]<-"ID"
#Test merge
full_wind<-merge(cropset_at_full, wind_jam1, by= "ID", all= T) 




###################################################################Puerto Rico#######################################################################################

Puerto<-fread("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Paper Data\\Puerto.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#storm Puerto
# stwind_pram<-Puerto[9:12]
# stwind_pram$lat<-Puerto[6]
# stwind_pram$long<-Puerto[7]
# stwind_pram$wind<-Puerto[8]


#tracking<-merge(al,Puerto, By= "Key",all=T)
unique(Puerto$Name)  #16 hurricanes
h1pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="DAVID")
h1pr<-as.data.table(h1pr)

#Correct day
for(f in 1: nrow(h1pr))
{
  if(nchar(h1pr$day[f])<2){
    h1pr$day[f]<-paste0("0",h1pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h1pr))
{
  if(nchar(h1pr$month[f])<2){
    h1pr$month[f]<-paste0("0",h1pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h1pr$time<-round(h1pr$time, 0)

for(f in 1: nrow(h1pr))
{
  if(nchar(h1pr$time[f])<2){
    h1pr$time[f]<-paste0("0",h1pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h1pr))
{
  if(nchar(h1pr[f])<2){
    h1pr$time[f]<-paste0("0",h1pr$time[f])
  }
  else{h1pr$time[f]<-paste0(h1pr$time[f],"00")}
}



h1pr$date<-paste0(h1pr$year,h1pr$month,h1pr$day, h1pr$time)
h1pr<-as.data.frame(h1pr)
h1pr[1:4]<-NULL
h1pr[4:9]<-NULL
#Reorder the dataframe
h1pr<-h1pr[c(4,1,2,3)]
#corecting date format for hurricanes
h1pr$date<-as.character(h1pr$date)



#rename columns because the package is strict on the names inputted
colnames(h1pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h1pr$wind<-convert_wind_speed(h1pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 1<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Puerto

#16 hurricanes
h2pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="ALLEN")
h2pr<-as.data.table(h2pr)

#Correct day
for(f in 1: nrow(h2pr))
{
  if(nchar(h2pr$day[f])<2){
    h2pr$day[f]<-paste0("0",h2pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h2pr))
{
  if(nchar(h2pr$month[f])<2){
    h2pr$month[f]<-paste0("0",h2pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h2pr$time<-round(h2pr$time, 0)

for(f in 1: nrow(h2pr))
{
  if(nchar(h2pr$time[f])<2){
    h2pr$time[f]<-paste0("0",h2pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h2pr))
{
  if(nchar(h2pr$time[f])<2){
    h2pr$time[f]<-paste0("0",h2pr$time[f])
  }
  else{h2pr$time[f]<-paste0(h2pr$time[f],"00")}
}



h2pr$date<-paste0(h2pr$year,h2pr$month,h2pr$day, h2pr$time)
h2pr<-as.data.frame(h2pr)
h2pr[1:4]<-NULL
h2pr[4:9]<-NULL
#Reorder the dataframe
h2pr<-h2pr[c(4,1,2,3)]
#corecting date format for hurricanes
h2pr$date<-as.character(h2pr$date)



#rename columns because the package is strict on the names inputted
colnames(h2pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h2pr$wind<-convert_wind_speed(h2pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 3<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h3pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="GLORIA")
h3pr<-as.data.table(h3pr)

#Correct day
for(f in 1: nrow(h3pr))
{
  if(nchar(h3pr$day[f])<2){
    h3pr$day[f]<-paste0("0",h3pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h3pr))
{
  if(nchar(h3pr$month[f])<2){
    h3pr$month[f]<-paste0("0",h3pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h3pr$time<-round(h3pr$time, 0)

for(f in 1: nrow(h3pr))
{
  if(nchar(h3pr$time[f])<2){
    h3pr$time[f]<-paste0("0",h3pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h3pr))
{
  if(nchar(h3pr$time[f])<2){
    h3pr$time[f]<-paste0("0",h3pr$time[f])
  }
  else{h3pr$time[f]<-paste0(h3pr$time[f],"00")}
}



h3pr$date<-paste0(h3pr$year,h3pr$month,h3pr$day, h3pr$time)
h3pr<-as.data.frame(h3pr)
h3pr[1:4]<-NULL
h3pr[4:9]<-NULL
#Reorder the dataframe
h3pr<-h3pr[c(4,1,2,3)]
#corecting date format for hurricanes
h3pr$date<-as.character(h3pr$date)



#rename columns because the package is strict on the names inputted
colnames(h3pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h3pr$wind<-convert_wind_speed(h3pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 3<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 4-6<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Puerto

#3 hurricanes
h4pr<-emlist[[1]]
h4pr<-as.data.table(h4pr)
h5pr<-emlist[[2]]
h5pr<-as.data.table(h5pr)
h6pr<-emlist[[3]]
h6pr<-as.data.table(h6pr)




#Correct day
for(f in 1: nrow(h4pr))
{
  if(nchar(h4pr$day[f])<2){
    h4pr$day[f]<-paste0("0",h4pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h4pr))
{
  if(nchar(h4pr$month[f])<2){
    h4pr$month[f]<-paste0("0",h4pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h4pr$time<-round(h4pr$time, 0)

for(f in 1: nrow(h4pr))
{
  if(nchar(h4pr$time[f])<2){
    h4pr$time[f]<-paste0("0",h4pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h4pr))
{
  if(nchar(h4pr$time[f])<2){
    h4pr$time[f]<-paste0("0",h4pr$time[f])
  }
  else{h4pr$time[f]<-paste0(h4pr$time[f],"00")}
}



h4pr$date<-paste0(h4pr$year,h4pr$month,h4pr$day, h4pr$time)
h4pr<-as.data.frame(h4pr)
h4pr[1:4]<-NULL
h4pr[4:9]<-NULL
#Reorder the dataframe
h4pr<-h4pr[c(4,1,2,3)]
#corecting date format for hurricanes
h4pr$date<-as.character(h4pr$date)



#rename columns because the package is strict on the names inputted
colnames(h4pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h4pr$wind<-convert_wind_speed(h4pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 4<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 5<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Correct day
for(f in 1: nrow(h5pr))
{
  if(nchar(h5pr$day[f])<2){
    h5pr$day[f]<-paste0("0",h5pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h5pr))
{
  if(nchar(h5pr$month[f])<2){
    h5pr$month[f]<-paste0("0",h5pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h5pr$time<-round(h5pr$time, 0)

for(f in 1: nrow(h5pr))
{
  if(nchar(h5pr$time[f])<2){
    h5pr$time[f]<-paste0("0",h5pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h5pr))
{
  if(nchar(h5pr$time[f])<2){
    h5pr$time[f]<-paste0("0",h5pr$time[f])
  }
  else{h5pr$time[f]<-paste0(h5pr$time[f],"00")}
}



h5pr$date<-paste0(h5pr$year,h5pr$month,h5pr$day, h5pr$time)
h5pr<-as.data.frame(h5pr)
h5pr[1:4]<-NULL
h5pr[4:9]<-NULL
#Reorder the dataframe
h5pr<-h5pr[c(4,1,2,3)]
#corecting date format for hurricanes
h5pr$date<-as.character(h5pr$date)



#rename columns because the package is strict on the names inputted
colnames(h5pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h5pr$wind<-convert_wind_speed(h5pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 5<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 6<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Correct day
for(f in 1: nrow(h6pr))
{
  if(nchar(h6pr$day[f])<2){
    h6pr$day[f]<-paste0("0",h6pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h6pr))
{
  if(nchar(h6pr$month[f])<2){
    h6pr$month[f]<-paste0("0",h6pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h6pr$time<-round(h6pr$time, 0)

for(f in 1: nrow(h6pr))
{
  if(nchar(h6pr$time[f])<2){
    h6pr$time[f]<-paste0("0",h6pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h6pr))
{
  if(nchar(h6pr$time[f])<2){
    h6pr$time[f]<-paste0("0",h6pr$time[f])
  }
  else{h6pr$time[f]<-paste0(h6pr$time[f],"00")}
}



h6pr$date<-paste0(h6pr$year,h6pr$month,h6pr$day, h6pr$time)
h6pr<-as.data.frame(h6pr)
h6pr[1:4]<-NULL
h6pr[4:9]<-NULL
#Reorder the dataframe
h6pr<-h6pr[c(4,1,2,3)]
#corecting date format for hurricanes
h6pr$date<-as.character(h6pr$date)



#rename columns because the package is strict on the names inputted
colnames(h6pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h6pr$wind<-convert_wind_speed(h6pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 6<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 7<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h7pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="GILBERT")
h7pr<-as.data.table(h7pr)


#Correct day
for(f in 1: nrow(h7pr))
{
  if(nchar(h7pr$day[f])<2){
    h7pr$day[f]<-paste0("0",h7pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h7pr))
{
  if(nchar(h7pr$month[f])<2){
    h7pr$month[f]<-paste0("0",h7pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h7pr$time<-round(h7pr$time, 0)

for(f in 1: nrow(h7pr))
{
  if(nchar(h7pr$time[f])<2){
    h7pr$time[f]<-paste0("0",h7pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h7pr))
{
  if(nchar(h7pr$time[f])<2){
    h7pr$time[f]<-paste0("0",h7pr$time[f])
  }
  else{h7pr$time[f]<-paste0(h7pr$time[f],"00")}
}



h7pr$date<-paste0(h7pr$year,h7pr$month,h7pr$day, h7pr$time)
h7pr<-as.data.frame(h7pr)
h7pr[1:4]<-NULL
h7pr[4:9]<-NULL
#Reorder the dataframe
h7pr<-h7pr[c(4,1,2,3)]
#corecting date format for hurricanes
h7pr$date<-as.character(h7pr$date)



#rename columns because the package is strict on the names inputted
colnames(h7pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h7pr$wind<-convert_wind_speed(h7pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 7<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 8<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h8pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="HUGO")
h8pr<-as.data.table(h8pr)

#Correct day
for(f in 1: nrow(h8pr))
{
  if(nchar(h8pr$day[f])<2){
    h8pr$day[f]<-paste0("0",h8pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h8pr))
{
  if(nchar(h8pr$month[f])<2){
    h8pr$month[f]<-paste0("0",h8pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h8pr$time<-round(h8pr$time, 0)

for(f in 1: nrow(h8pr))
{
  if(nchar(h8pr$time[f])<2){
    h8pr$time[f]<-paste0("0",h8pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h8pr))
{
  if(nchar(h8pr$time[f])<2){
    h8pr$time[f]<-paste0("0",h8pr$time[f])
  }
  else{h8pr$time[f]<-paste0(h8pr$time[f],"00")}
}



h8pr$date<-paste0(h8pr$year,h8pr$month,h8pr$day, h8pr$time)
h8pr<-as.data.frame(h8pr)
h8pr[1:4]<-NULL
h8pr[4:9]<-NULL
#Reorder the dataframe
h8pr<-h8pr[c(4,1,2,3)]
#corecting date format for hurricanes
h8pr$date<-as.character(h8pr$date)



#rename columns because the package is strict on the names inputted
colnames(h8pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h8pr$wind<-convert_wind_speed(h8pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 8<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 9<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


h9pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="LUIS")
h9pr<-as.data.table(h9pr)

#Correct day
for(f in 1: nrow(h9pr))
{
  if(nchar(h9pr$day[f])<2){
    h9pr$day[f]<-paste0("0",h9pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h9pr))
{
  if(nchar(h9pr$month[f])<2){
    h9pr$month[f]<-paste0("0",h9pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h9pr$time<-round(h9pr$time, 0)

for(f in 1: nrow(h9pr))
{
  if(nchar(h9pr$time[f])<2){
    h9pr$time[f]<-paste0("0",h9pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h9pr))
{
  if(nchar(h9pr$time[f])<2){
    h9pr$time[f]<-paste0("0",h9pr$time[f])
  }
  else{h9pr$time[f]<-paste0(h9pr$time[f],"00")}
}



h9pr$date<-paste0(h9pr$year,h9pr$month,h9pr$day, h9pr$time)
h9pr<-as.data.frame(h9pr)
h9pr[1:4]<-NULL
h9pr[4:9]<-NULL
#Reorder the dataframe
h9pr<-h9pr[c(4,1,2,3)]
#corecting date format for hurricanes
h9pr$date<-as.character(h9pr$date)



#rename columns because the package is strict on the names inputted
colnames(h9pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h9pr$wind<-convert_wind_speed(h9pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 9<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 10<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h10pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="MARILYN")
h10pr<-as.data.table(h10pr)

#Correct day
for(f in 1: nrow(h10pr))
{
  if(nchar(h10pr$day[f])<2){
    h10pr$day[f]<-paste0("0",h10pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h10pr))
{
  if(nchar(h10pr$month[f])<2){
    h10pr$month[f]<-paste0("0",h10pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h10pr$time<-round(h10pr$time, 0)

for(f in 1: nrow(h10pr))
{
  if(nchar(h10pr$time[f])<2){
    h10pr$time[f]<-paste0("0",h10pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h10pr))
{
  if(nchar(h10pr$time[f])<2){
    h10pr$time[f]<-paste0("0",h10pr$time[f])
  }
  else{h10pr$time[f]<-paste0(h10pr$time[f],"00")}
}



h10pr$date<-paste0(h10pr$year,h10pr$month,h10pr$day, h10pr$time)
h10pr<-as.data.frame(h10pr)
h10pr[1:4]<-NULL
h10pr[4:9]<-NULL
#Reorder the dataframe
h10pr<-h10pr[c(4,1,2,3)]
#corecting date format for hurricanes
h10pr$date<-as.character(h10pr$date)



#rename columns because the package is strict on the names inputted
colnames(h10pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h10pr$wind<-convert_wind_speed(h10pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 10<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 11<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bertha<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="BERTHA")
blist<-list()
blist<-split(bertha, bertha$year, drop = TRUE)
h11pr<-blist[[1]]
h11pr<-as.data.table(h11pr)

#Correct day
for(f in 1: nrow(h11pr))
{
  if(nchar(h11pr$day[f])<2){
    h11pr$day[f]<-paste0("0",h11pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h11pr))
{
  if(nchar(h11pr$month[f])<2){
    h11pr$month[f]<-paste0("0",h11pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h11pr$time<-round(h11pr$time, 0)

for(f in 1: nrow(h11pr))
{
  if(nchar(h11pr$time[f])<2){
    h11pr$time[f]<-paste0("0",h11pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h11pr))
{
  if(nchar(h11pr$time[f])<2){
    h11pr$time[f]<-paste0("0",h11pr$time[f])
  }
  else{h11pr$time[f]<-paste0(h11pr$time[f],"00")}
}



h11pr$date<-paste0(h11pr$year,h11pr$month,h11pr$day, h11pr$time)
h11pr<-as.data.frame(h11pr)
h11pr[1:4]<-NULL
h11pr[4:9]<-NULL
#Reorder the dataframe
h11pr<-h11pr[c(4,1,2,3)]
#corecting date format for hurricanes
h11pr$date<-as.character(h11pr$date)



#rename columns because the package is strict on the names inputted
colnames(h11pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h11pr$wind<-convert_wind_speed(h11pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 11<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 12-13<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
eduardo<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="EDOUARD")
adlist<-list()
edlist<-split(eduardo, eduardo$year, drop = TRUE)
h12pr<-edlist[[1]]
h13pr<-edlist[[2]]


h12pr<-as.data.table(h12pr)
h13pr<-as.data.table(h13pr)


#Correct day


#################################################################STORM 12################################################################################
#Correct day
for(f in 1: nrow(h12pr))
{
  if(nchar(h12pr$day[f])<2){
    h12pr$day[f]<-paste0("0",h12pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h12pr))
{
  if(nchar(h12pr$month[f])<2){
    h12pr$month[f]<-paste0("0",h12pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h12pr$time<-round(h12pr$time, 0)

for(f in 1: nrow(h12pr))
{
  if(nchar(h12pr$time[f])<2){
    h12pr$time[f]<-paste0("0",h12pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h12pr))
{
  if(nchar(h12pr$time[f])<2){
    h12pr$time[f]<-paste0("0",h12pr$time[f])
  }
  else{h12pr$time[f]<-paste0(h12pr$time[f],"00")}
}



h12pr$date<-paste0(h12pr$year,h12pr$month,h12pr$day, h12pr$time)
h12pr<-as.data.frame(h12pr)
h12pr[1:4]<-NULL
h12pr[4:9]<-NULL
#Reorder the dataframe
h12pr<-h12pr[c(4,1,2,3)]
#corecting date format for hurricanes
h12pr$date<-as.character(h12pr$date)



#rename columns because the package is strict on the names inputted
colnames(h12pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h12pr$wind<-convert_wind_speed(h12pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)
#########################################################################################################################################################

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 13<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#Correct day
for(f in 1: nrow(h13pr))
{
  if(nchar(h13pr$day[f])<2){
    h13pr$day[f]<-paste0("0",h13pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h13pr))
{
  if(nchar(h13pr$month[f])<2){
    h13pr$month[f]<-paste0("0",h13pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h13pr$time<-round(h13pr$time, 0)

for(f in 1: nrow(h13pr))
{
  if(nchar(h13pr$time[f])<2){
    h13pr$time[f]<-paste0("0",h13pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h13pr))
{
  if(nchar(h13pr$time[f])<2){
    h13pr$time[f]<-paste0("0",h13pr$time[f])
  }
  else{h13pr$time[f]<-paste0(h13pr$time[f],"00")}
}



h13pr$date<-paste0(h13pr$year,h13pr$month,h13pr$day, h13pr$time)
h13pr<-as.data.frame(h13pr)
h13pr[1:4]<-NULL
h13pr[4:9]<-NULL
#Reorder the dataframe
h13pr<-h13pr[c(4,1,2,3)]
#corecting date format for hurricanes
h13pr$date<-as.character(h13pr$date)



#rename columns because the package is strict on the names inputted
colnames(h13pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h13pr$wind<-convert_wind_speed(h13pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 12-13<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 14-15<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h14pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="GEORGES")

h14pr<-as.data.table(h14pr)

#Correct day
for(f in 1: nrow(h14pr))
{
  if(nchar(h14pr$day[f])<2){
    h14pr$day[f]<-paste0("0",h14pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h14pr))
{
  if(nchar(h14pr$month[f])<2){
    h14pr$month[f]<-paste0("0",h14pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h14pr$time<-round(h14pr$time, 0)

for(f in 1: nrow(h14pr))
{
  if(nchar(h14pr$time[f])<2){
    h14pr$time[f]<-paste0("0",h14pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h14pr))
{
  if(nchar(h14pr$time[f])<2){
    h14pr$time[f]<-paste0("0",h14pr$time[f])
  }
  else{h14pr$time[f]<-paste0(h14pr$time[f],"00")}
}



h14pr$date<-paste0(h14pr$year,h14pr$month,h14pr$day, h14pr$time)
h14pr<-as.data.frame(h14pr)
h14pr[1:4]<-NULL
h14pr[4:9]<-NULL
#Reorder the dataframe
h14pr<-h14pr[c(4,1,2,3)]
#corecting date format for hurricanes
h14pr$date<-as.character(h14pr$date)



#rename columns because the package is strict on the names inputted
colnames(h14pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h14pr$wind<-convert_wind_speed(h14pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


############################################################################STORM 15####################################################################
h15pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="LENNY")
h15pr<-as.data.table(h15pr)

for(f in 1: nrow(h15pr))
{
  if(nchar(h15pr$day[f])<2){
    h15pr$day[f]<-paste0("0",h15pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h15pr))
{
  if(nchar(h15pr$month[f])<2){
    h15pr$month[f]<-paste0("0",h15pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h15pr$time<-round(h15pr$time, 0)

for(f in 1: nrow(h15pr))
{
  if(nchar(h15pr$time[f])<2){
    h15pr$time[f]<-paste0("0",h15pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h15pr))
{
  if(nchar(h15pr$time[f])<2){
    h15pr$time[f]<-paste0("0",h15pr$time[f])
  }
  else{h15pr$time[f]<-paste0(h15pr$time[f],"00")}
}



h15pr$date<-paste0(h15pr$year,h15pr$month,h15pr$day, h15pr$time)
h15pr<-as.data.frame(h15pr)
h15pr[1:4]<-NULL
h15pr[4:9]<-NULL
#Reorder the dataframe
h15pr<-h15pr[c(4,1,2,3)]
#corecting date format for hurricanes
h15pr$date<-as.character(h15pr$date)



#rename columns because the package is strict on the names inputted
colnames(h15pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h15pr$wind<-convert_wind_speed(h15pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 14-15<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 16-19<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
frances<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="FRANCES")
frlist<-list()
frlist<-split(frances, frances$year, drop = TRUE)
h16pr<-frlist[[1]]
h17pr<-frlist[[2]]
h18pr<-frlist[[3]]
h19pr<-frlist[[4]]
h16pr<-as.data.table(h16pr)
h17pr<-as.data.table(h17pr)
h18pr<-as.data.table(h18pr)
h19pr<-as.data.table(h19pr)

#Correct day
for(f in 1: nrow(h16pr))
{
  if(nchar(h16pr$day[f])<2){
    h16pr$day[f]<-paste0("0",h16pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h16pr))
{
  if(nchar(h16pr$month[f])<2){
    h16pr$month[f]<-paste0("0",h16pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h16pr$time<-round(h16pr$time, 0)

for(f in 1: nrow(h16pr))
{
  if(nchar(h16pr$time[f])<2){
    h16pr$time[f]<-paste0("0",h16pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h16pr))
{
  if(nchar(h16pr$time[f])<2){
    h16pr$time[f]<-paste0("0",h16pr$time[f])
  }
  else{h16pr$time[f]<-paste0(h16pr$time[f],"00")}
}



h16pr$date<-paste0(h16pr$year,h16pr$month,h16pr$day, h16pr$time)
h16pr<-as.data.frame(h16pr)
h16pr[1:4]<-NULL
h16pr[4:9]<-NULL
#Reorder the dataframe
h16pr<-h16pr[c(4,1,2,3)]
#corecting date format for hurricanes
h16pr$date<-as.character(h16pr$date)



#rename columns because the package is strict on the names inputted
colnames(h16pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h16pr$wind<-convert_wind_speed(h16pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 16<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 17<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# h17pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="PALOMA")
# h17pr<-as.data.table(h17pr)

#Correct day
for(f in 1: nrow(h17pr))
{
  if(nchar(h17pr$day[f])<2){
    h17pr$day[f]<-paste0("0",h17pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h17pr))
{
  if(nchar(h17pr$month[f])<2){
    h17pr$month[f]<-paste0("0",h17pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h17pr$time<-round(h17pr$time, 0)

for(f in 1: nrow(h17pr))
{
  if(nchar(h17pr$time[f])<2){
    h17pr$time[f]<-paste0("0",h17pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h17pr))
{
  if(nchar(h17pr$time[f])<2){
    h17pr$time[f]<-paste0("0",h17pr$time[f])
  }
  else{h17pr$time[f]<-paste0(h17pr$time[f],"00")}
}



h17pr$date<-paste0(h17pr$year,h17pr$month,h17pr$day, h17pr$time)
h17pr<-as.data.frame(h17pr)
h17pr[1:4]<-NULL
h17pr[4:9]<-NULL
#Reorder the dataframe
h17pr<-h17pr[c(4,1,2,3)]
#corecting date format for hurricanes
h17pr$date<-as.character(h17pr$date)



#rename columns because the package is strict on the names inputted
colnames(h17pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h17pr$wind<-convert_wind_speed(h17pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 17<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 18<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# h18pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="SANDY")
# h18pr<-as.data.table(h18pr)

#Correct day
for(f in 1: nrow(h18pr))
{
  if(nchar(h18pr$day[f])<2){
    h18pr$day[f]<-paste0("0",h18pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h18pr))
{
  if(nchar(h18pr$month[f])<2){
    h18pr$month[f]<-paste0("0",h18pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h18pr$time<-round(h18pr$time, 0)

for(f in 1: nrow(h18pr))
{
  if(nchar(h18pr$time[f])<2){
    h18pr$time[f]<-paste0("0",h18pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h18pr))
{
  if(nchar(h18pr$time[f])<2){
    h18pr$time[f]<-paste0("0",h18pr$time[f])
  }
  else{h18pr$time[f]<-paste0(h18pr$time[f],"00")}
}



h18pr$date<-paste0(h18pr$year,h18pr$month,h18pr$day, h18pr$time)
h18pr<-as.data.frame(h18pr)
h18pr[1:4]<-NULL
h18pr[4:9]<-NULL
#Reorder the dataframe
h18pr<-h18pr[c(4,1,2,3)]
#corecting date format for hurricanes
h18pr$date<-as.character(h18pr$date)



#rename columns because the package is strict on the names inputted
colnames(h18pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h18pr$wind<-convert_wind_speed(h18pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 18<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 19<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h19pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="DEAN")
h19pr<-as.data.table(h19pr)

#Correct day
for(f in 1: nrow(h19pr))
{
  if(nchar(h19pr$day[f])<2){
    h19pr$day[f]<-paste0("0",h19pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h19pr))
{
  if(nchar(h19pr$month[f])<2){
    h19pr$month[f]<-paste0("0",h19pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h19pr$time<-round(h19pr$time, 0)

for(f in 1: nrow(h19pr))
{
  if(nchar(h19pr$time[f])<2){
    h19pr$time[f]<-paste0("0",h19pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h19pr))
{
  if(nchar(h19pr$time[f])<2){
    h19pr$time[f]<-paste0("0",h19pr$time[f])
  }
  else{h19pr$time[f]<-paste0(h19pr$time[f],"00")}
}



h19pr$date<-paste0(h19pr$year,h19pr$month,h19pr$day, h19pr$time)
h19pr<-as.data.frame(h19pr)
h19pr[1:4]<-NULL
h19pr[4:9]<-NULL
#Reorder the dataframe
h19pr<-h19pr[c(4,1,2,3)]
#corecting date format for hurricanes
h19pr$date<-as.character(h19pr$date)



#rename columns because the package is strict on the names inputted
colnames(h19pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h19pr$wind<-convert_wind_speed(h19pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 19<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 20<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h20pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="IKE")
h20pr<-as.data.table(h20pr)

#Correct day
for(f in 1: nrow(h20pr))
{
  if(nchar(h20pr$day[f])<2){
    h20pr$day[f]<-paste0("0",h20pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h20pr))
{
  if(nchar(h20pr$month[f])<2){
    h20pr$month[f]<-paste0("0",h20pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h20pr$time<-round(h20pr$time, 0)

for(f in 1: nrow(h20pr))
{
  if(nchar(h20pr$time[f])<2){
    h20pr$time[f]<-paste0("0",h20pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h20pr))
{
  if(nchar(h20pr$time[f])<2){
    h20pr$time[f]<-paste0("0",h20pr$time[f])
  }
  else{h20pr$time[f]<-paste0(h20pr$time[f],"00")}
}



h20pr$date<-paste0(h20pr$year,h20pr$month,h20pr$day, h20pr$time)
h20pr<-as.data.frame(h20pr)
h20pr[1:4]<-NULL
h20pr[4:9]<-NULL
#Reorder the dataframe
h20pr<-h20pr[c(4,1,2,3)]
#corecting date format for hurricanes
h20pr$date<-as.character(h20pr$date)



#rename columns because the package is strict on the names inputted
colnames(h20pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h20pr$wind<-convert_wind_speed(h20pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 20<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 21<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h21pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="OMAR")
h21pr<-as.data.table(h21pr)

#Correct day
for(f in 1: nrow(h21pr))
{
  if(nchar(h21pr$day[f])<2){
    h21pr$day[f]<-paste0("0",h21pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h21pr))
{
  if(nchar(h21pr$month[f])<2){
    h21pr$month[f]<-paste0("0",h21pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h21pr$time<-round(h21pr$time, 0)

for(f in 1: nrow(h21pr))
{
  if(nchar(h21pr$time[f])<2){
    h21pr$time[f]<-paste0("0",h21pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h21pr))
{
  if(nchar(h21pr$time[f])<2){
    h21pr$time[f]<-paste0("0",h21pr$time[f])
  }
  else{h21pr$time[f]<-paste0(h21pr$time[f],"00")}
}



h21pr$date<-paste0(h21pr$year,h21pr$month,h21pr$day, h21pr$time)
h21pr<-as.data.frame(h21pr)
h21pr[1:4]<-NULL
h21pr[4:9]<-NULL
#Reorder the dataframe
h21pr<-h21pr[c(4,1,2,3)]
#corecting date format for hurricanes
h21pr$date<-as.character(h21pr$date)



#rename columns because the package is strict on the names inputted
colnames(h21pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h21pr$wind<-convert_wind_speed(h21pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 21<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 22<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h22pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="EARL")
h22pr<-as.data.table(h22pr)

#Correct day
for(f in 1: nrow(h22pr))
{
  if(nchar(h22pr$day[f])<2){
    h22pr$day[f]<-paste0("0",h22pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h22pr))
{
  if(nchar(h22pr$month[f])<2){
    h22pr$month[f]<-paste0("0",h22pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h22pr$time<-round(h22pr$time, 0)

for(f in 1: nrow(h22pr))
{
  if(nchar(h22pr$time[f])<2){
    h22pr$time[f]<-paste0("0",h22pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h22pr))
{
  if(nchar(h22pr$time[f])<2){
    h22pr$time[f]<-paste0("0",h22pr$time[f])
  }
  else{h22pr$time[f]<-paste0(h22pr$time[f],"00")}
}



h22pr$date<-paste0(h22pr$year,h22pr$month,h22pr$day, h22pr$time)
h22pr<-as.data.frame(h22pr)
h22pr[1:4]<-NULL
h22pr[4:9]<-NULL
#Reorder the dataframe
h22pr<-h22pr[c(4,1,2,3)]
#corecting date format for hurricanes
h22pr$date<-as.character(h22pr$date)



#rename columns because the package is strict on the names inputted
colnames(h22pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h22pr$wind<-convert_wind_speed(h22pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 22<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 23<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


h23pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="GONZALO")
h23pr<-as.data.table(h23pr)

#Correct day
for(f in 1: nrow(h23pr))
{
  if(nchar(h23pr$day[f])<2){
    h23pr$day[f]<-paste0("0",h23pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h23pr))
{
  if(nchar(h23pr$month[f])<2){
    h23pr$month[f]<-paste0("0",h23pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h23pr$time<-round(h23pr$time, 0)

for(f in 1: nrow(h23pr))
{
  if(nchar(h23pr$time[f])<2){
    h23pr$time[f]<-paste0("0",h23pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h23pr))
{
  if(nchar(h23pr$time[f])<2){
    h23pr$time[f]<-paste0("0",h23pr$time[f])
  }
  else{h23pr$time[f]<-paste0(h23pr$time[f],"00")}
}



h23pr$date<-paste0(h23pr$year,h23pr$month,h23pr$day, h23pr$time)
h23pr<-as.data.frame(h23pr)
h23pr[1:4]<-NULL
h23pr[4:9]<-NULL
#Reorder the dataframe
h23pr<-h23pr[c(4,1,2,3)]
#corecting date format for hurricanes
h23pr$date<-as.character(h23pr$date)



#rename columns because the package is strict on the names inputted
colnames(h23pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h23pr$wind<-convert_wind_speed(h23pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 23<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 24<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
h24pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="IRMA")
h24pr<-as.data.table(h24pr)

#Correct day
for(f in 1: nrow(h24pr))
{
  if(nchar(h24pr$day[f])<2){
    h24pr$day[f]<-paste0("0",h24pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h24pr))
{
  if(nchar(h24pr$month[f])<2){
    h24pr$month[f]<-paste0("0",h24pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h24pr$time<-round(h24pr$time, 0)

for(f in 1: nrow(h24pr))
{
  if(nchar(h24pr$time[f])<2){
    h24pr$time[f]<-paste0("0",h24pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h24pr))
{
  if(nchar(h24pr$time[f])<2){
    h24pr$time[f]<-paste0("0",h24pr$time[f])
  }
  else{h24pr$time[f]<-paste0(h24pr$time[f],"00")}
}



h24pr$date<-paste0(h24pr$year,h24pr$month,h24pr$day, h24pr$time)
h24pr<-as.data.frame(h24pr)
h24pr[1:4]<-NULL
h24pr[4:9]<-NULL
#Reorder the dataframe
h24pr<-h24pr[c(4,1,2,3)]
#corecting date format for hurricanes
h24pr$date<-as.character(h24pr$date)



#rename columns because the package is strict on the names inputted
colnames(h24pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h24pr$wind<-convert_wind_speed(h24pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 24<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 25<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


h25pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="JOSE")
h25pr<-as.data.table(h25pr)

#Correct day
for(f in 1: nrow(h25pr))
{
  if(nchar(h25pr$day[f])<2){
    h25pr$day[f]<-paste0("0",h25pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h25pr))
{
  if(nchar(h25pr$month[f])<2){
    h25pr$month[f]<-paste0("0",h25pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h25pr$time<-round(h25pr$time, 0)

for(f in 1: nrow(h25pr))
{
  if(nchar(h25pr$time[f])<2){
    h25pr$time[f]<-paste0("0",h25pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h25pr))
{
  if(nchar(h25pr$time[f])<2){
    h25pr$time[f]<-paste0("0",h25pr$time[f])
  }
  else{h25pr$time[f]<-paste0(h25pr$time[f],"00")}
}



h25pr$date<-paste0(h25pr$year,h25pr$month,h25pr$day, h25pr$time)
h25pr<-as.data.frame(h25pr)
h25pr[1:4]<-NULL
h25pr[4:9]<-NULL
#Reorder the dataframe
h25pr<-h25pr[c(4,1,2,3)]
#corecting date format for hurricanes
h25pr$date<-as.character(h25pr$date)



#rename columns because the package is strict on the names inputted
colnames(h25pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h25pr$wind<-convert_wind_speed(h25pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 25<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 26<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
maria<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="MARIA")
mlist<-list()
mlist<-split(maria, maria$year, drop= TRUE)
h26pr<-mlist[[1]]
h26pr<-as.data.table(h26pr)

#Correct day
for(f in 1: nrow(h26pr))
{
  if(nchar(h26pr$day[f])<2){
    h26pr$day[f]<-paste0("0",h26pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h26pr))
{
  if(nchar(h26pr$month[f])<2){
    h26pr$month[f]<-paste0("0",h26pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h26pr$time<-round(h26pr$time, 0)

for(f in 1: nrow(h26pr))
{
  if(nchar(h26pr$time[f])<2){
    h26pr$time[f]<-paste0("0",h26pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h26pr))
{
  if(nchar(h26pr$time[f])<2){
    h26pr$time[f]<-paste0("0",h26pr$time[f])
  }
  else{h26pr$time[f]<-paste0(h26pr$time[f],"00")}
}



h26pr$date<-paste0(h26pr$year,h26pr$month,h26pr$day, h26pr$time)
h26pr<-as.data.frame(h26pr)
h26pr[1:4]<-NULL
h26pr[4:9]<-NULL
#Reorder the dataframe
h26pr<-h26pr[c(4,1,2,3)]
#corecting date format for hurricanes
h26pr$date<-as.character(h26pr$date)



#rename columns because the package is strict on the names inputted
colnames(h26pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h26pr$wind<-convert_wind_speed(h26pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 26<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 27<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h27pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="ERIKA")
h27pr<-as.data.table(h27pr)

#Correct day
for(f in 1: nrow(h27pr))
{
  if(nchar(h27pr$day[f])<2){
    h27pr$day[f]<-paste0("0",h27pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h27pr))
{
  if(nchar(h27pr$month[f])<2){
    h27pr$month[f]<-paste0("0",h27pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h27pr$time<-round(h27pr$time, 0)

for(f in 1: nrow(h27pr))
{
  if(nchar(h27pr$time[f])<2){
    h27pr$time[f]<-paste0("0",h27pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h27pr))
{
  if(nchar(h27pr$time[f])<2){
    h27pr$time[f]<-paste0("0",h27pr$time[f])
  }
  else{h27pr$time[f]<-paste0(h27pr$time[f],"00")}
}



h27pr$date<-paste0(h27pr$year,h27pr$month,h27pr$day, h27pr$time)
h27pr<-as.data.frame(h27pr)
h27pr[1:4]<-NULL
h27pr[4:9]<-NULL
#Reorder the dataframe
h27pr<-h27pr[c(4,1,2,3)]
#corecting date format for hurricanes
h27pr$date<-as.character(h27pr$date)



#rename columns because the package is strict on the names inputted
colnames(h27pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h27pr$wind<-convert_wind_speed(h27pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 27<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 28<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h28pr<-subset(atlantic_cat345_interpolated, atlantic_cat345_interpolated$Name=="FABIAN")
h28pr<-as.data.table(h28pr)

#Correct day
for(f in 1: nrow(h28pr))
{
  if(nchar(h28pr$day[f])<2){
    h28pr$day[f]<-paste0("0",h28pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h28pr))
{
  if(nchar(h28pr$month[f])<2){
    h28pr$month[f]<-paste0("0",h28pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h28pr$time<-round(h28pr$time, 0)

for(f in 1: nrow(h28pr))
{
  if(nchar(h28pr$time[f])<2){
    h28pr$time[f]<-paste0("0",h28pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h28pr))
{
  if(nchar(h28pr$time[f])<2){
    h28pr$time[f]<-paste0("0",h28pr$time[f])
  }
  else{h28pr$time[f]<-paste0(h28pr$time[f],"00")}
}



h28pr$date<-paste0(h28pr$year,h28pr$month,h28pr$day, h28pr$time)
h28pr<-as.data.frame(h28pr)
h28pr[1:4]<-NULL
h28pr[4:9]<-NULL
#Reorder the dataframe
h28pr<-h28pr[c(4,1,2,3)]
#corecting date format for hurricanes
h28pr$date<-as.character(h28pr$date)



#rename columns because the package is strict on the names inputted
colnames(h28pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h28pr$wind<-convert_wind_speed(h28pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 28<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 29<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

h29pr<-mlist[[2]]                                                          #Hurricane Maria 2017
h29pr<-as.data.table(h29pr)

#Correct day
for(f in 1: nrow(h29pr))
{
  if(nchar(h29pr$day[f])<2){
    h29pr$day[f]<-paste0("0",h29pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h29pr))
{
  if(nchar(h29pr$month[f])<2){
    h29pr$month[f]<-paste0("0",h29pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h29pr$time<-round(h29pr$time, 0)

for(f in 1: nrow(h29pr))
{
  if(nchar(h29pr$time[f])<2){
    h29pr$time[f]<-paste0("0",h29pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h29pr))
{
  if(nchar(h29pr$time[f])<2){
    h29pr$time[f]<-paste0("0",h29pr$time[f])
  }
  else{h29pr$time[f]<-paste0(h29pr$time[f],"00")}
}



h29pr$date<-paste0(h29pr$year,h29pr$month,h29pr$day, h29pr$time)
h29pr<-as.data.frame(h29pr)
h29pr[1:4]<-NULL
h29pr[4:9]<-NULL
#Reorder the dataframe
h29pr<-h29pr[c(4,1,2,3)]
#corecting date format for hurricanes
h29pr$date<-as.character(h29pr$date)



#rename columns because the package is strict on the names inputted
colnames(h29pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h29pr$wind<-convert_wind_speed(h29pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 29<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 30<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#bertha 2008
h30pr<-blist[[2]]
h30pr<-as.data.table(h30pr)

#Correct day
for(f in 1: nrow(h30pr))
{
  if(nchar(h30pr$day[f])<2){
    h30pr$day[f]<-paste0("0",h30pr$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(h30pr))
{
  if(nchar(h30pr$month[f])<2){
    h30pr$month[f]<-paste0("0",h30pr$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

h30pr$time<-round(h30pr$time, 0)

for(f in 1: nrow(h30pr))
{
  if(nchar(h30pr$time[f])<2){
    h30pr$time[f]<-paste0("0",h30pr$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(h30pr))
{
  if(nchar(h30pr$time[f])<2){
    h30pr$time[f]<-paste0("0",h30pr$time[f])
  }
  else{h30pr$time[f]<-paste0(h30pr$time[f],"00")}
}



h30pr$date<-paste0(h30pr$year,h30pr$month,h30pr$day, h30pr$time)
h30pr<-as.data.frame(h30pr)
h30pr[1:4]<-NULL
h30pr[4:9]<-NULL
#Reorder the dataframe
h30pr<-h30pr[c(4,1,2,3)]
#corecting date format for hurricanes
h30pr$date<-as.character(h30pr$date)



#rename columns because the package is strict on the names inputted
colnames(h30pr)<-c("date","latitude","longitude","wind")



#convert the speed from kmph to knots
h30pr$wind<-convert_wind_speed(h30pr$wind, old_metric = "kmph", new_metric = "knots", round = NULL)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Storm 30<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Location<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
Cen90_crop<-fread("C://Users//goulb/OneDrive//Desktop//Research 2018//Census Data//lacd1990//Cen_RDG.csv")
pri<-readOGR(dsn = "C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\Shape files\\gadm36_PRI_shp", layer = "gadm36_PRI_1")

pribox<-pri@bbox
Cen90_crop$location<-"NA"
Cen90_crop[(which(Cen90_crop$Long>=-(pribox[1,2]) & Cen90_crop$Long<=-(pribox[1,1]) & Cen90_crop$Lat>=(pribox[2,1]) & Cen90_crop$Lat<=(pribox[2,2]))),5]<-"Puerto Rico"
try_lpri<-subset(Cen90_crop, Cen90_crop$location=="Puerto Rico")

try_lpri[,1]<-NULL
try_lpri[,4]<-NULL
try_lpri<-as.data.frame(try_lpri)
try_lpri<-try_lpri[c(3,2,1)]
try_lpri$glon<-(try_lpri$glon)
try_lpri$glon<- -(try_lpri$glon)
colnames(try_lpri)<-c("gridid","glat","glon")


wind_pri1<-get_grid_winds(hurr_track= h1pr, grid_df = try_lpri) #Hurricane 1 for Puerto
wind_pri2<-get_grid_winds(hurr_track= h2pr, grid_df = try_lpri) #Hurricane 2 for Puerto
wind_pri3<-get_grid_winds(hurr_track= h3pr, grid_df = try_lpri) #Hurricane 3 for Puerto

#Four is not working. I will correct it by dropping the last two rows where the times were the same which should not have been in the first place.  
#cutting the dataset into two  
#h4pr<-h4pr[-c(90:91), ]
wind_pri4<-get_grid_winds(hurr_track= h4pr, grid_df = try_lpri) #Hurricane 4 for Puerto
wind_pri5<-get_grid_winds(hurr_track= h5pr, grid_df = try_lpri) #Hurricane 5 for Puerto
#The maximum wind speed is measure in meters per second.
wind_pri6<-get_grid_winds(hurr_track= head(h6pr), grid_df = try_lpri) #Hurricane 6 for Puerto
wind_pri7<-get_grid_winds(hurr_track= h7pr, grid_df = try_lpri) #Hurricane 7 for Puerto
wind_pri8<-get_grid_winds(hurr_track= h8pr, grid_df = try_lpri) #Hurricane 8 for Puerto
wind_pri9<-get_grid_winds(hurr_track= h9pr, grid_df = try_lpri) #Hurricane 9 for Puerto
wind_pri10<-get_grid_winds(hurr_track= h10pr, grid_df = try_lpri) #Hurricane 10 for Puerto
wind_pri11<-get_grid_winds(hurr_track= h11pr, grid_df = try_lpri) #Hurricane 11 for Puerto
wind_pri12<-get_grid_winds(hurr_track= h12pr, grid_df = try_lpri) #Hurricane 12 for Puerto
wind_pri13<-get_grid_winds(hurr_track= h13pr, grid_df = try_lpri) #Hurricane 13 for Puerto
h14pr<-h4pr[-c(23:25), ]
wind_pri14<-get_grid_winds(hurr_track= h14pr, grid_df = try_lpri) #Hurricane 14 for Puerto    Had to remove last 3 rows because of repeated times
wind_pri15<-get_grid_winds(hurr_track= h15pr, grid_df = try_lpri) #Hurricane 15 for Puerto
wind_pri16<-get_grid_winds(hurr_track= h16pr, grid_df = try_lpri) #Hurricane 16 for Puerto
wind_pri17<-get_grid_winds(hurr_track= h17pr, grid_df = try_lpri) #Hurricane 17 for Puerto    h17pr has one observation
wind_pri18<-get_grid_winds(hurr_track= h18pr, grid_df = try_lpri) #Hurricane 18 for Puerto
wind_pri19<-get_grid_winds(hurr_track= h19pr, grid_df = try_lpri) #Hurricane 19 for Puerto
wind_pri20<-get_grid_winds(hurr_track= h20pr, grid_df = try_lpri) #Hurricane 20 for Puerto
wind_pri21<-get_grid_winds(hurr_track= h21pr, grid_df = try_lpri) #Hurricane 21 for Puerto    h21pr has one observation
wind_pri22<-get_grid_winds(hurr_track= h22pr, grid_df = try_lpri) #Hurricane 22 for Puerto
wind_pri23<-get_grid_winds(hurr_track= h23pr, grid_df = try_lpri) #Hurricane 23 for Puerto
wind_pri24<-get_grid_winds(hurr_track= h24pr, grid_df = try_lpri) #Hurricane 24 for Puerto
wind_pri25<-get_grid_winds(hurr_track= h25pr, grid_df = try_lpri) #Hurricane 25 for Puerto
wind_pri26<-get_grid_winds(hurr_track= h26pr, grid_df = try_lpri) #Hurricane 26 for Puerto    h26pr has one observation 
wind_pri27<-get_grid_winds(hurr_track= h27pr, grid_df = try_lpri) #Hurricane 27 for Puerto
wind_pri28<-get_grid_winds(hurr_track= h28pr, grid_df = try_lpri) #Hurricane 28 for Puerto
wind_pri29<-get_grid_winds(hurr_track= h29pr, grid_df = try_lpri) #Hurricane 29 for Puerto
wind_pri30<-get_grid_winds(hurr_track= h30pr, grid_df = try_lpri) #Hurricane 30 for Puerto


speed_list_puerto<-list(wind_pri1, wind_pri2, wind_pri3, wind_pri4, wind_pri5, wind_pri6, wind_pri7, wind_pri8, wind_pri9, wind_pri10, wind_pri11, wind_pri12, wind_pri13, wind_pri14, wind_pri15, wind_pri16, wind_pri17, wind_pri18, wind_pri19, wind_pri20, wind_pri21, wind_pri22, wind_pri23, wind_pri24, wind_pri25,wind_pri26, wind_pri27, wind_pri28, wind_pri29, wind_pri30)
save(speed_list_puerto, file="speed_list_puerto.RData")

load("speed_list_puerto.RData")

###################################################################Puerto Rico#######################################################################################

#Wind speeds for Puerto Rico and Jamaica have been calculated. I need to use these to further develope the destruction index. 


############INFORMATION ABOUT THE WIND SPEED CALCULATIONS###########
                                #NAAO#

#The maximum wind speed is measure in meters per second.

#vmax_gust: Maximum 10-m 1-minute gust wind experienced at the grid point during the storm

            #Gusts are a few seconds (3-5 s) wind peak. Typically in a hurricane environment, 
            #the value of the maximum 3 second gust over a 1 minute period is on the order of 1.3 times
            #(or 30% higher than) than the 1 min sustained wind.



#vmax_sust: Maximum 10-m 1-minute sustained wind experienced at the grid point during the storm

            #The maximum sustained wind mentioned in the advisories that NHC issues for tropical
            #storms and hurricanes are the highest 1 min surface winds occurring within the circulation 
            #of the system. These "surface" winds are those observed (or, more often, estimated) to occur
            #at the standard meteorological height of 10 m (33 ft) in an unobstructed exposure (i.e., not blocked by buildings or trees).


#gust_dur: Duration gust wind was at or above a specified speed (default is 20 m/s), in minutes
#sust_dur: Duration sustained wind was at or above a specified speed (default is 20 m/s), in minutes



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










