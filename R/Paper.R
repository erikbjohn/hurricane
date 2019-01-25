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
install.packages("dplyr", dependencies = TRUE )


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
library(dplyr)



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



###################################
#I found a more efficient wat to 
#input the hurricanes into a datase
##################################




##################################################
#The dataset for hurricanes is provided in R 
#by the NAAO so We do not have to collect the data
#manually. The codes below use the NAOO R data.
#################################################





###############################################HURDAT DATASET#######################################################

#Call HURRDAT2 data from package. This dataset replaces the old dataset from which the previous literature 
#conducted research. This dataset is updated to include tropical cyclones between 1851 and 2017.

al<- get_hurdat(basin = "AL")




#########################Check those locations that produce NA entries############################


# plot(getMap()) 
# points(-66.47917,18.47917,col='green') #This area is being recorded as NA but is highly populated and is in 
#Puerto Rico. Same situation with a number of other countries. 


###################################################
#A number of locations that are populated on 
#the peripheries of countries are being recorded 
#as NA so I need to find a way to account for these
#lcations so that our estimates can be as precise
#as possible.
######################################################################################################################################


###########>>>>>>>>>>>>>>>>Callin the file cropset_at_full from file for speed rathe than formulate by code each time<<<<<<<<<<<<<<<<<<<############
#
# cropset_at_full<-read.csv("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\hurricane\\R\\hurricane\\R\\cropset_at_full.csv")
# colnames(cropset_at_full)[15]<-"Lat.y"
# colnames(cropset_at_full)[16]<-"Country Name"
#
#
#
#
# cropset_at_full<-read.csv("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Paper Data\\cropset_at_full.csv")
# colnames(cropset_at_full)[15]<-"Lat.y"
# colnames(cropset_at_full)[16]<-"Country Name"
#
# #Convert the name column from factor to character
# cropset_at_full$`Country Name`<-as.character(cropset_at_full$`Country Name`)
#
# cropset_at_full_list<-list()
# cropset_at_full_list<-(split(cropset_at_full, cropset_at_full$year , drop = TRUE))



al_new<-al[which(al$Status=="HU"),]



#Collecting hurricanes between categiries 3 and 5

al_new<-subset(al_new , al_new$Wind >=96)


al_new[9:21]<-NULL  #drop these columns
al_new[4:5]<-NULL


#######fix date on al_new
wlsplit<-str_split_fixed(al_new$DateTime, " ",2)

wdatefix<-wlsplit[,1]
wtimefix<-wlsplit[,2]
wdsplit<- str_split_fixed(wdatefix, "-",3)


#Need to separate the time variable into a more maliable formal
wtimefix<-as.matrix(wtimefix)

colnames(wtimefix)<-c("time")

wtsplit<-str_split_fixed(wtimefix,":",3)




al_new$month<-wdsplit[,2]
al_new$day<-wdsplit[,3]
al_new$time<-wlsplit[,2]
al_new$year<-wdsplit[,1]
#al_new$atlantic_cat345_id <- 1:nrow(atlantic_cat345)
al_new$time<-wtsplit[,1]

al_new$DateTime<-NULL




#Correct day
for(f in 1: nrow(al_new))
{
  if(nchar(al_new$day[f])<2){
    al_new$day[f]<-paste0("0",al_new$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(al_new))
{
  if(nchar(al_new$month[f])<2){
    al_new$month[f]<-paste0("0",al_new$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

al_new$time<-round(al_new$time, 0)

for(f in 1: nrow(al_new))
{
  if(nchar(al_new$time[f])<2){
    al_new$time[f]<-paste0("0",al_new$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(al_new))
{
  if(nchar(al_new$time[f])<2){
    al_new$time[f]<-paste0("0",al_new$time[f])
  }
  else{al_new$time[f]<-paste0(al_new$time[f],"00")}
}



al_new$date<-paste0(al_new$year,al_new$month,al_new$day, al_new$time)
al_new<-as.data.frame(al_new)

al_new<-subset(al_new, al_new$year>1969) #drop storms before 1970


for(i in 1:nrow(al_new)){
  
  which(al_new$Name[i]== al_new$Name[i+1] )
  
}


##################################################Correct names########################################################################

bertha<-subset(al_new, al_new$Name=="BERTHA")
bertha_list<-list()
bertha_list<-split(bertha, bertha$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="BERTHA")),]
bertha_list[[1]]$Name<-"BERTHA1"
bertha_list[[2]]$Name<-"BERTHA2"
al_new<-bind_rows(al_new, bertha_list[1])
al_new<-bind_rows(al_new, bertha_list[2])


edouard<-subset(al_new, al_new$Name=="EDOUARD")
edouard_list<-list()
edouard_list<-split(edouard, edouard$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="EDOUARD")),]
edouard_list[[1]]$Name<-"EDOUARD1"
edouard_list[[2]]$Name<-"EDOUARD2"
al_new<-bind_rows(al_new, edouard_list[1])
al_new<-bind_rows(al_new, edouard_list[2])


ella<-subset(al_new, al_new$Name=="ELLA")
ella_list<-list()
ella_list<-split(ella, ella$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="ELLA")),]
ella_list[[1]]$Name<-"ELLA1"
ella_list[[2]]$Name<-"ELLA2"
al_new<-bind_rows(al_new, ella_list[1])
al_new<-bind_rows(al_new, ella_list[2])

emily<-subset(al_new, al_new$Name=="EMILY")
emily_list<-list()
emily_list<-split(emily, emily$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="EMILY")),]
emily_list[[1]]$Name<-"EMILY1"
emily_list[[2]]$Name<-"EMILY2"
emily_list[[3]]$Name<-"EMILY3"
al_new<-bind_rows(al_new, emily_list[1])
al_new<-bind_rows(al_new, emily_list[2])
al_new<-bind_rows(al_new, emily_list[3])


felix<-subset(al_new, al_new$Name=="FELIX")
felix_list<-list()
felix_list<-split(felix, felix$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="FELIX")),]
felix_list[[1]]$Name<-"FELIX1"
felix_list[[2]]$Name<-"FELIX2"
felix_list[[3]]$Name<-"FELIX3"
al_new<-bind_rows(al_new, felix_list[1])
al_new<-bind_rows(al_new, felix_list[2])
al_new<-bind_rows(al_new, felix_list[3])



floyd<-subset(al_new, al_new$Name=="FLOYD")
floyd_list<-list()
floyd_list<-split(floyd, floyd$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="FLOYD")),]
floyd_list[[1]]$Name<-"FLOYD1"
floyd_list[[2]]$Name<-"FLOYD2"
al_new<-bind_rows(al_new, floyd_list[1])
al_new<-bind_rows(al_new, floyd_list[2])


frances<-subset(al_new, al_new$Name=="FRANCES")
frances_list<-list()
frances_list<-split(frances, frances$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="FRANCES")),]
frances_list[[1]]$Name<-"FRANCES1"
frances_list[[2]]$Name<-"FRANCES2"
frances_list[[3]]$Name<-"FRANCES3"
al_new<-bind_rows(al_new, frances_list[1])
al_new<-bind_rows(al_new, frances_list[2])
al_new<-bind_rows(al_new, frances_list[3])

gustav<-subset(al_new, al_new$Name=="GUSTAV")
gustav_list<-list()
gustav_list<-split(gustav, gustav$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="GUSTAV")),]
gustav_list[[1]]$Name<-"GUSTAV1"
gustav_list[[2]]$Name<-"GUSTAV2"
al_new<-bind_rows(al_new, gustav_list[1])
al_new<-bind_rows(al_new, gustav_list[2])

harvey<-subset(al_new, al_new$Name=="HARVEY")
harvey_list<-list()
harvey_list<-split(harvey, harvey$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="HARVEY")),]
harvey_list[[1]]$Name<-"HARVEY1"
harvey_list[[2]]$Name<-"HARVEY2"
al_new<-bind_rows(al_new, harvey_list[1])
al_new<-bind_rows(al_new, harvey_list[2])

helene<-subset(al_new, al_new$Name=="HELENE")
helene_list<-list()
helene_list<-split(helene, helene$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="HELENE")),]
helene_list[[1]]$Name<-"HELENE1"
helene_list[[2]]$Name<-"HELENE2"
al_new<-bind_rows(al_new, helene_list[1])
al_new<-bind_rows(al_new, helene_list[2])


irene<-subset(al_new, al_new$Name=="IRENE")
irene_list<-list()
irene_list<-split(irene, irene$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="IRENE")),]
irene_list[[1]]$Name<-"IRENE1"
irene_list[[2]]$Name<-"IRENE2"
al_new<-bind_rows(al_new, irene_list[1])
al_new<-bind_rows(al_new, irene_list[2])

isidore<-subset(al_new, al_new$Name=="ISIDORE")
isidore_list<-list()
isidore_list<-split(isidore, isidore$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="ISIDORE")),]
isidore_list[[1]]$Name<-"ISIDORE1"
isidore_list[[2]]$Name<-"ISIDORE2"
al_new<-bind_rows(al_new, isidore_list[1])
al_new<-bind_rows(al_new, isidore_list[2])

karl<-subset(al_new, al_new$Name=="KARL")
karl_list<-list()
karl_list<-split(karl, karl$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="KARL")),]
karl_list[[1]]$Name<-"KARL1"
karl_list[[2]]$Name<-"KARL2"
al_new<-bind_rows(al_new, karl_list[1])
al_new<-bind_rows(al_new, karl_list[2])

kate<-subset(al_new, al_new$Name=="KATE")
kate_list<-list()
kate_list<-split(kate, kate$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="KATE")),]
kate_list[[1]]$Name<-"KATE1"
kate_list[[2]]$Name<-"KATE2"
al_new<-bind_rows(al_new, kate_list[1])
al_new<-bind_rows(al_new, kate_list[2])

lili<-subset(al_new, al_new$Name=="LILI")
lili_list<-list()
lili_list<-split(lili, lili$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="LILI")),]
lili_list[[1]]$Name<-"LILI1"
lili_list[[2]]$Name<-"LILI2"
al_new<-bind_rows(al_new, lili_list[1])
al_new<-bind_rows(al_new, lili_list[2])

maria<-subset(al_new, al_new$Name=="MARIA")
maria_list<-list()
maria_list<-split(maria, maria$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="MARIA")),]
maria_list[[1]]$Name<-"MARIA1"
maria_list[[2]]$Name<-"MARIA2"
al_new<-bind_rows(al_new, maria_list[1])
al_new<-bind_rows(al_new, maria_list[2])


ophelia<-subset(al_new, al_new$Name=="OPHELIA")
ophelia_list<-list()
ophelia_list<-split(ophelia, ophelia$year, drop = TRUE)
al_new<-al_new[-(which(al_new$Name=="OPHELIA")),]
ophelia_list[[1]]$Name<-"OPHELIA1"
ophelia_list[[2]]$Name<-"OPHELIA2"
al_new<-bind_rows(al_new, ophelia_list[1])
al_new<-bind_rows(al_new, ophelia_list[2])





######################################################################################################################################


#al_new[2]<-NULL
al_new[6:9]<-NULL
#Reorder the dataframe
#al_new<-al_new[c(5,2,3,4,1)]
al_new<-al_new[c(6,3,4,5,1,2)]
#corecting date format for hurricanes
al_new$date<-as.character(al_new$date)



#rename columns because the package is strict on the names inputted
colnames(al_new)<-c("date","latitude","longitude","wind", "Key", "name")

write.table(al_new, file="al_new.csv",sep=",",row.names=F)

#convert the speed from kmph to knots
#al_new$wind<-convert_wind_speed(al_new$wind, old_metric = "kmph", new_metric = "knots", round = NULL)

#al_new_list<-split(al_new, al_new$Key, drop= TRUE)

#split by name instead of key and see if we can keep the names.
al_new_list<-split(al_new, al_new$name, drop= TRUE)


intp_1<-lapply(al_new_list, function(x) {x["Key"]<-NULL;x}) #Drop key from the dataset
intp_1<-lapply(al_new_list, function(x) {x["name"]<-NULL;x}) #Drop name from the dataset   
intp_1<-lapply(intp_1, function(x) {colnames(x)<-c("date","latitude","longitude","wind","Key");x}) #rename columns
intp_1<-
  
  interpolate<-vector("list",length(intp_1))

for(i in 1:length(intp_1)){
  interpolate[[i]]<-create_full_track(hurr_track = intp_1[[i]] , tint = 3)
}

#add names to each of the list itemslapply(intp_1, function(x) {as.data.frame(x);x})
listnames<-names(intp_1)

for(i in 1: length(interpolate)){
  interpolate[[i]]$name<-listnames[i]
  
}




interpol_hur<-rbindlist(interpolate, use.names=TRUE)
#order
#interpol_hur<-interpol_hur[order(date),]


interpol_hur<-as.data.table(interpol_hur)
colnames(interpol_hur)<-c("date","latitude","longitude","wind","name")
interpol_hur <- interpol_hur[, latitude := as.numeric(stringr::str_replace(latitude, '\\.$', ''))]
interpol_hur <- interpol_hur[, longitude := as.numeric(stringr::str_replace(longitude, '\\.$', ''))]
interpol_hur$storm_id<-1:nrow(interpol_hur )


#Calculate distances such that we can separate out those points that were greater than 500 km

tat <- data.table()
tadts_crop90 <- data.table()
for(iRow in 1:nrow(interpol_hur)){
  if((iRow %% 20)==0){
    cat(iRow, 'of', nrow(interpol_hur), '\n')
  }
  tat_lat<-as.numeric(interpol_hur[iRow]$latitude)
  tat_long<-as.numeric(interpol_hur[iRow]$longitude)
  tat_id<-as.numeric(interpol_hur[iRow]$storm_id)
  tatddis_crop<-(distCosine(Cen90_crop[,2:3], c(tat_long,tat_lat), r=6378137))/1000 #This finds the shortest distance between two points
  #In meters. Dividing by 1000 gives us our estimates in kilometers
  tat<-data.table(tat_id = tat_id, cen90_id = Cen90_crop$ID, tatddis_crop )
  tat<-tat[tatddis_crop<=500]
  tadts_crop90 <- rbindlist(list(tadts_crop90, tat), use.names = TRUE, fill=TRUE)
}
#colnames(ddis)<-c("distance(km)", "dat","ID")

tatddis_crop<-data.table::data.table(tatddis_crop)


#Rename Column ID for hurricanes so that I can merge them.
colnames(interpol_hur)[6]<-"tat_id"



############################################Need to fix date in interpol_hur before merging############################################

wlsplit<-str_split_fixed(interpol_hur$date, " ",2)

wdatefix<-wlsplit[,1]
wtimefix<-wlsplit[,2]
wdsplit<- str_split_fixed(wdatefix, "-",3)


#Need to separate the time variable into a more maliable formal
wtimefix<-as.matrix(wtimefix)

colnames(wtimefix)<-c("time")

wtsplit<-str_split_fixed(wtimefix,":",3)




interpol_hur$month<-wdsplit[,2]
interpol_hur$day<-wdsplit[,3]
interpol_hur$time<-wlsplit[,2]
interpol_hur$year<-wdsplit[,1]
#interpol_hur$atlantic_cat345_id <- 1:nrow(atlantic_cat345)
interpol_hur$time<-wtsplit[,1]

#interpol_hur$DateTime<-NULL




#Correct day
for(f in 1: nrow(interpol_hur))
{
  if(nchar(interpol_hur$day[f])<2){
    interpol_hur$day[f]<-paste0("0",interpol_hur$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(interpol_hur))
{
  if(nchar(interpol_hur$month[f])<2){
    interpol_hur$month[f]<-paste0("0",interpol_hur$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time


for(f in 1: nrow(interpol_hur))
{
  if(nchar(interpol_hur$time[f])<2){
    interpol_hur$time[f]<-paste0("0",interpol_hur$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(interpol_hur))
{
  if(nchar(interpol_hur$time[f])<2){
    interpol_hur$time[f]<-paste0("0",interpol_hur$time[f])
  }
  else{interpol_hur$time[f]<-paste0(interpol_hur$time[f],"00")}
}



interpol_hur$date<-paste0(interpol_hur$year,interpol_hur$month,interpol_hur$day, interpol_hur$time)
interpol_hur<-as.data.frame(interpol_hur)

#interpol_hur<-subset(interpol_hur, interpol_hur$year>1969) #drop storms before 1970

# interpol_hur[2]<-NULL
# interpol_hur[5:8]<-NULL
# #Reorder the dataframe
# interpol_hur<-interpol_hur[c(5,2,3,4,1)]
# #corecting date format for hurricanes
# interpol_hur$date<-as.character(interpol_hur$date)
# 
# 
# 
# #rename columns because the package is strict on the names inputted
# colnames(interpol_hur)<-c("date","latitude","longitude","wind", "Key")


write.table(interpol_hur, file="interpol_hur.csv",sep=",",row.names=F)
write.table(tadts_crop90, file="modified distance cals.csv",sep=",",row.names=F)
#######################################################################################################################################

new_cropset_at<-merge(tadts_crop90, interpol_hur, by = "tat_id", all=T)
new_cropset_at<-new_cropset_at[order(name, month, day, time,year),]


complete.cases(new_cropset_at)
new_cropset_at_full<-na.omit(new_cropset_at)

colnames(new_cropset_at_full)[2]<- "ID"
colnames(new_cropset_at_full)[1]<- "hurricane_id"
colnames(new_cropset_at_full)[3]<- "distance"


new_cropset_at_full<-merge(new_cropset_at_full, Cen90_crop, by = "ID", all=T)

complete.cases(new_cropset_at_full)
new_cropset_at_full<-na.omit(new_cropset_at_full)

write.table(new_cropset_at_full, file="new_cropset_at_full.csv",sep=",",row.names=F)




#name locations







setwd("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material")
source('R/call_boxes.R')  ##Call the shape files using this function 
boxes<- call_boxes()


new_cropset_at_full$location<-"NA"
# 
# cropset_at_full<-read.csv("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Paper Data\\cropset_at_full.csv")
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(jmbox[1,2]) & new_cropset_at_full$Long<=-(jmbox[1,1]) & new_cropset_at_full$Lat>=(jmbox[2,1]) & new_cropset_at_full$Lat<=(jmbox[2,2]))),16]<-"Jamaica"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(venbox[1,2]) & new_cropset_at_full$Long<=-(venbox[1,1]) & new_cropset_at_full$Lat>=(venbox[2,1]) & new_cropset_at_full$Lat<=(venbox[2,2]))),16]<-"Venezuela"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(colbox[1,2]) & new_cropset_at_full$Long<=-(colbox[1,1]) & new_cropset_at_full$Lat>=(colbox[2,1]) & new_cropset_at_full$Lat<=(colbox[2,2]))),16]<-"Columbia"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(abwbox[1,2]) & new_cropset_at_full$Long<=-(abwbox[1,1]) & new_cropset_at_full$Lat>=(abwbox[2,1]) & new_cropset_at_full$Lat<=(abwbox[2,2]))),16]<-"Aruba"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(angbox[1,2]) & new_cropset_at_full$Long<=-(angbox[1,1]) & new_cropset_at_full$Lat>=(angbox[2,1]) & new_cropset_at_full$Lat<=(angbox[2,2]))),16]<-"Anguilla"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(ABbox[1,2]) & new_cropset_at_full$Long<=-(ABbox[1,1]) & new_cropset_at_full$Lat>=(ABbox[2,1]) & new_cropset_at_full$Lat<=(ABbox[2,2]))),16]<-"Antigua and Barbuda"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(bhsbox[1,2]) & new_cropset_at_full$Long<=-(bhsbox[1,1]) & new_cropset_at_full$Lat>=(bhsbox[2,1]) & new_cropset_at_full$Lat<=(bhsbox[2,2]))),16]<-"Bahamas"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(blzbox[1,2]) & new_cropset_at_full$Long<=-(blzbox[1,1]) & new_cropset_at_full$Lat>=(blzbox[2,1]) & new_cropset_at_full$Lat<=(blzbox[2,2]))),16]<-"Belize"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(brbbox[1,2]) & new_cropset_at_full$Long<=-(brbbox[1,1]) & new_cropset_at_full$Lat>=(brbbox[2,1]) & new_cropset_at_full$Lat<=(brbbox[2,2]))),16]<-"Barbados"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(cribox[1,2]) & new_cropset_at_full$Long<=-(cribox[1,1]) & new_cropset_at_full$Lat>=(cribox[2,1]) & new_cropset_at_full$Lat<=(cribox[2,2]))),16]<-"Costa Rica"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(cubbox[1,2]) & new_cropset_at_full$Long<=-(cubbox[1,1]) & new_cropset_at_full$Lat>=(cubbox[2,1]) & new_cropset_at_full$Lat<=(cubbox[2,2]))),16]<-"Cuba"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(curbox[1,2]) & new_cropset_at_full$Long<=-(curbox[1,1]) & new_cropset_at_full$Lat>=(curbox[2,1]) & new_cropset_at_full$Lat<=(curbox[2,2]))),16]<-"Curacao"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(cymbox[1,2]) & new_cropset_at_full$Long<=-(cymbox[1,1]) & new_cropset_at_full$Lat>=(cymbox[2,1]) & new_cropset_at_full$Lat<=(cymbox[2,2]))),16]<-"Cayman"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(dmabox[1,2]) & new_cropset_at_full$Long<=-(dmabox[1,1]) & new_cropset_at_full$Lat>=(dmabox[2,1]) & new_cropset_at_full$Lat<=(dmabox[2,2]))),16]<-"Dominica"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(dombox[1,2]) & new_cropset_at_full$Long<=-(dombox[1,1]) & new_cropset_at_full$Lat>=(dombox[2,1]) & new_cropset_at_full$Lat<=(dombox[2,2]))),16]<-"Dominican Republic"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(glpbox[1,2]) & new_cropset_at_full$Long<=-(glpbox[1,1]) & new_cropset_at_full$Lat>=(glpbox[2,1]) & new_cropset_at_full$Lat<=(glpbox[2,2]))),16]<-"Guadeloupe"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(grdbox[1,2]) & new_cropset_at_full$Long<=-(grdbox[1,1]) & new_cropset_at_full$Lat>=(grdbox[2,1]) & new_cropset_at_full$Lat<=(grdbox[2,2]))),16]<-"Grenada"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(gtmbox[1,2]) & new_cropset_at_full$Long<=-(gtmbox[1,1]) & new_cropset_at_full$Lat>=(gtmbox[2,1]) & new_cropset_at_full$Lat<=(gtmbox[2,2]))),16]<-"Guatemala"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(guybox[1,2]) & new_cropset_at_full$Long<=-(guybox[1,1]) & new_cropset_at_full$Lat>=(guybox[2,1]) & new_cropset_at_full$Lat<=(guybox[2,2]))),16]<-"Guyana"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(hndbox[1,2]) & new_cropset_at_full$Long<=-(hndbox[1,1]) & new_cropset_at_full$Lat>=(hndbox[2,1]) & new_cropset_at_full$Lat<=(hndbox[2,2]))),16]<-"Honduras"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(htibox[1,2]) & new_cropset_at_full$Long<=-(htibox[1,1]) & new_cropset_at_full$Lat>=(htibox[2,1]) & new_cropset_at_full$Lat<=(htibox[2,2]))),16]<-"Haiti"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(knabox[1,2]) & new_cropset_at_full$Long<=-(knabox[1,1]) & new_cropset_at_full$Lat>=(knabox[2,1]) & new_cropset_at_full$Lat<=(knabox[2,2]))),16]<-"Saint Kitts and Nevis"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(lcabox[1,2]) & new_cropset_at_full$Long<=-(lcabox[1,1]) & new_cropset_at_full$Lat>=(lcabox[2,1]) & new_cropset_at_full$Lat<=(lcabox[2,2]))),16]<-"Saint Lucia"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(mafbox[1,2]) & new_cropset_at_full$Long<=-(mafbox[1,1]) & new_cropset_at_full$Lat>=(mafbox[2,1]) & new_cropset_at_full$Lat<=(mafbox[2,2]))),16]<-"Saint Martin"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(mexbox[1,2]) & new_cropset_at_full$Long<=-(mexbox[1,1]) & new_cropset_at_full$Lat>= (mexbox[2,1]) & new_cropset_at_full$Lat<= (mexbox[2,2]))),16]<-"Mexico"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(mtqbox[1,2]) & new_cropset_at_full$Long<=-(mtqbox[1,1]) & new_cropset_at_full$Lat>=(mtqbox[2,1]) & new_cropset_at_full$Lat<=(mtqbox[2,2]))),16]<-"Martinique"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(nicbox[1,2]) & new_cropset_at_full$Long<=-(nicbox[1,1]) & new_cropset_at_full$Lat>=(nicbox[2,1]) & new_cropset_at_full$Lat<=(nicbox[2,2]))),16]<-"Nicaragua"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(panbox[1,2]) & new_cropset_at_full$Long<=-(panbox[1,1]) & new_cropset_at_full$Lat>=(panbox[2,1]) & new_cropset_at_full$Lat<=(panbox[2,2]))),16]<-"Panama"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(pribox[1,2]) & new_cropset_at_full$Long<=-(pribox[1,1]) & new_cropset_at_full$Lat>=(pribox[2,1]) & new_cropset_at_full$Lat<=(pribox[2,2]))),16]<-"Puerto Rico"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(slvbox[1,2]) & new_cropset_at_full$Long<=-(slvbox[1,1]) & new_cropset_at_full$Lat>=(slvbox[2,1]) & new_cropset_at_full$Lat<=(slvbox[2,2]))),16]<-"El Salvador"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(tcabox[1,2]) & new_cropset_at_full$Long<=-(tcabox[1,1]) & new_cropset_at_full$Lat>=(tcabox[2,1]) & new_cropset_at_full$Lat<=(tcabox[2,2]))),16]<-"Turks and Caicos Islands"    
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(ttobox[1,2]) & new_cropset_at_full$Long<=-(ttobox[1,1]) & new_cropset_at_full$Lat>=(ttobox[2,1]) & new_cropset_at_full$Lat<=(ttobox[2,2]))),16]<- "Trinidad and Tobago"  
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(vctbox[1,2]) & new_cropset_at_full$Long<=-(vctbox[1,1]) & new_cropset_at_full$Lat>=(vctbox[2,1]) & new_cropset_at_full$Lat<=(vctbox[2,2]))),16]<- "Saint Vincent and the Grenadines"
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(vgbbox[1,2]) & new_cropset_at_full$Long<=-(vgbbox[1,1]) & new_cropset_at_full$Lat>=(vgbbox[2,1]) & new_cropset_at_full$Lat<=(vgbbox[2,2]))),16]<-"British Virgin Islands"  
new_cropset_at_full[(which(new_cropset_at_full$Long>=-(virbox[1,2]) & new_cropset_at_full$Long<=-(virbox[1,1]) & new_cropset_at_full$Lat>=(virbox[2,1]) & new_cropset_at_full$Lat<=(virbox[2,2]))),16]<-"United States Virgin Islands"
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>_____________________________<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




#Check the numner of cell entries that are NA 
sum(length(which(is.na(new_cropset_at_full$`Location`))))

#After filling, the total amount of NA locations fell from 228050 to 0

write.table(new_cropset_at_full, file="new_cropset_at_full.csv",sep=",",row.names=F)

new_cropset_at_full<-read.csv("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Paper Data\\new_cropset_at_full.csv")




########################################Collect data by country######################################################

mexico<-data.table()
mexico<-subset(new_cropset_at_full,new_cropset_at_full$location == "Mexico")
mexico_list<-split(mexico, mexico$name, drop = TRUE)
write.table(mexico, file="mexico.csv",sep=",",row.names=F)

puerto<-data.table()
puerto<-subset(new_cropset_at_full,new_cropset_at_full$location =='Puerto Rico')
puerto_list<-split(puerto, puerto$name , drop = TRUE)
write.table(puerto, file="puerto.csv",sep=",",row.names=F)

bahamas<-data.table()
bahamas<-subset(new_cropset_at_full,new_cropset_at_full$location=='Bahamas')
bahamas_list<-split(bahamas, bahamas$name, drop = TRUE)
write.table(bahamas, file="bahamas.csv",sep=",",row.names=F)

haiti<-data.table()
haiti<-subset(new_cropset_at_full,new_cropset_at_full$location=='Haiti')
haiti_list<-split(haiti,haiti$name, drop = TRUE)
write.table(haiti, file="haiti.csv",sep=",",row.names=F)

cayman<-data.table()
cayman<-subset(new_cropset_at_full,new_cropset_at_full$location=='Cayman')
cayman_list<-split(cayman, cayman$name, drop = TRUE)
write.table(cayman, file="cayman.csv",sep=",",row.names=F)

anguilla<-data.table()
anguilla<-subset(new_cropset_at_full,new_cropset_at_full$location=='Anguilla')
anguilla_list<-split(anguilla, anguilla$name, drop = TRUE)
write.table(anguilla, file="anguilla.csv",sep=",",row.names=F)

guatemala<-data.table()
guatemala<-subset(new_cropset_at_full,new_cropset_at_full$location=='Guatemala')
guatemala_list<-split(guatemala, guatemala$name, drop = TRUE)
write.table(guatemala, file="guatemala.csv",sep=",",row.names=F)

montserrat<-data.table()
montserrat<-subset(new_cropset_at_full,new_cropset_at_full$location=='Montserrat')
montserrat_list<-split(montserrat, montserrat$name, drop = TRUE)
write.table(montserrat, file="montserrat.csv",sep=",",row.names=F)

dominica<-data.table()
dominica<-subset(new_cropset_at_full,new_cropset_at_full$location=='Dominica')
dominica_list<-split(dominica, dominica$name, drop = TRUE)
write.table(dominica, file="dominica.csv",sep=",",row.names=F)


stlucia<-data.table()
stlucia<-subset(new_cropset_at_full,new_cropset_at_full$location=='Saint Lucia')
stlucia_list<-split(stlucia, stlucia$name, drop = TRUE)
write.table(stlucia, file="stlucia.csv",sep=",",row.names=F)

aruba<-data.table()
aruba<-subset(new_cropset_at_full,new_cropset_at_full$location=='Aruba')
aruba_list<-split(aruba, aruba$name, drop = TRUE)
write.table(aruba, file="aruba.csv",sep=",",row.names=F)

tt<-data.table()
tt<-subset(new_cropset_at_full,new_cropset_at_full$location=='Trinidad and Tobago')
tt_list<-split(tt, tt$name, drop = TRUE)
write.table(tt, file="tt.csv",sep=",",row.names=F)

guyana<-data.table()
guyana<-subset(new_cropset_at_full,new_cropset_at_full$location=='Guyana')
guyana_list<-split(guyana, guyana$name, drop = TRUE)
write.table(guyana, file="guyana.csv",sep=",",row.names=F)

cuba<-data.table()
cuba<-subset(new_cropset_at_full,new_cropset_at_full$location=='Cuba')
cuba_list<-split(cuba, cuba$name, drop = TRUE)
write.table(cuba, file="cuba.csv",sep=",",row.names=F)

bvi<-data.table()
bvi<-subset(new_cropset_at_full,new_cropset_at_full$location=='British Virgin Islands')
bvi_list<-split(bvi, bvi$name, drop = TRUE)
write.table(bvi, file="bvi.csv",sep=",",row.names=F)

belize<-data.table()
belize<-subset(new_cropset_at_full,new_cropset_at_full$location=='Belize')
belize_list<-split(belize, belize$name, drop = TRUE)
write.table(belize, file="belize.csv",sep=",",row.names=F)

stmartin<-data.table()
stmartin<-subset(new_cropset_at_full,new_cropset_at_full$location=='Saint Martin')
stmartin_list<-split(stmartin, stmartin$name, drop = TRUE)
write.table(stmartin, file="stmartin.csv",sep=",",row.names=F)

AB<-data.table()
AB<-subset(new_cropset_at_full,new_cropset_at_full$location=='Antigua and Barbuda')
AB_list<-split(AB, AB$name, drop = TRUE)
write.table(AB, file="AB.csv",sep=",",row.names=F)

honduras<-data.table()
honduras<-subset(new_cropset_at_full,new_cropset_at_full$location=='Honduras')
honduras_list<-split(honduras, honduras$name, drop = TRUE)
write.table(honduras, file="honduras.csv",sep=",",row.names=F)

nicaragua<-data.table()
nicaragua<-subset(new_cropset_at_full,new_cropset_at_full$location=='Nicaragua')
nicaragua_list<-split(nicaragua, nicaragua$name, drop = TRUE)
write.table(nicaragua, file="nicaragua.csv",sep=",",row.names=F)

svg<-data.table()
svg<-subset(new_cropset_at_full,new_cropset_at_full$location=='Saint Vincent and the Grenadines')
svg_list<-split(svg, svg$name, drop = TRUE)
write.table(svg, file="svg.csv",sep=",",row.names=F)

grenada<-data.table()
grenada<-subset(new_cropset_at_full,new_cropset_at_full$location=='Grenada')
grenada_list<-split(grenada, grenada$name, drop = TRUE)
write.table(grenada, file="grenada.csv",sep=",",row.names=F)

costa_rica<-data.table()
costa_rica<-subset(new_cropset_at_full,new_cropset_at_full$location=='Costa Rica')
costa_rica_list<-split(costa_rica, costa_rica$name, drop = TRUE)
write.table(costa_rica, file="costa_rica.csv",sep=",",row.names=F)

turks<-data.table()
turks<-subset(new_cropset_at_full,new_cropset_at_full$location=='Turks and Caicos Islands')
turks_list<-split(turks, turks$name, drop = TRUE)
write.table(turks, file="turks.csv",sep=",",row.names=F)

domrep<-data.table()
domrep<-subset(new_cropset_at_full,new_cropset_at_full$location=='Dominican Republic')
domrep_list<-split(domrep, domrep$name, drop = TRUE)
write.table(domrep, file="domrep.csv",sep=",",row.names=F)

jamaica<-data.table()
jamaica<-subset(new_cropset_at_full,new_cropset_at_full$location=='Jamaica')
jamaica_list<-split(jamaica, jamaica$name, drop= TRUE)
write.table(jamaica, file="jamaica.csv",sep=",",row.names=F)

virgin<-data.table()
virgin<-subset(new_cropset_at_full,new_cropset_at_full$location=='United States Virgin Islands')
virgin_list<-split(virgin, virgin$name, drop = TRUE)
write.table(virgin, file="virgin.csv",sep=",",row.names=F)

skn<-data.table()
skn<-subset(new_cropset_at_full,new_cropset_at_full$location=='Saint Kitts and Nevis')
skn_list<-split(skn, skn$name, drop = TRUE)
write.table(skn, file="skn.csv",sep=",",row.names=F)

elsalvador<-data.table()
elsalvador<-subset(new_cropset_at_full,new_cropset_at_full$location=='El Salvador')
elsalvador_list<-split(elsalvador, elsalvador$name, drop = TRUE)
write.table(elsalvador, file="elsalvador.csv",sep=",",row.names=F)

barbados<-data.table()
barbados<-subset(new_cropset_at_full,new_cropset_at_full$location=='Barbados')
barbados_list<-split(barbados, barbados$name, drop = TRUE)
write.table(barbados, file="barbados.csv",sep=",",row.names=F)

columbia<-data.table()
columbia<-subset(new_cropset_at_full,new_cropset_at_full$location=='Columbia')
columbia_list<-split(columbia, columbia$name, drop = TRUE)
write.table(columbia, file="columbia.csv",sep=",",row.names=F)

panama<-data.table()
panama<-subset(new_cropset_at_full,new_cropset_at_full$location=='Panama')
panama_list<-split(panama, panama$name, drop = TRUE)
write.table(panama, file="panama.csv",sep=",",row.names=F)


#########################################
#with the above, we now have a clear 
#collection of hurricanes and their
#respective distances from each centroid
#in each relevant country
#########################################

####Collecting the information for gilbert from Jamaica####
#####################################################################################################################################

jamaica <-fread("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Paper Data\\jamaica.csv")
Cen90_crop$location<-"NA"
Cen90_crop[(which(Cen90_crop$Long>=-(jmbox[1,2]) & Cen90_crop$Long<=-(jmbox[1,1]) & Cen90_crop$Lat>=(jmbox[2,1]) & Cen90_crop$Lat<=(jmbox[2,2]))),5]<-"Jamaica"
al_new<-fread("C:\\Users\\goulb\\OneDrive\\Desktop\\Research 2018\\Spatial tutorial material\\hurricane\\R\\hurricane\\R\\al_new.csv")

gilbert_track<-subset(al_new, al_new$name=="GILBERT", drop= TRUE)
gilbert_track<-as.data.table(gilbert_track)



# gilbert_track[,1:3]<-NULL
# gilbert_track[,5:13]<-NULL
#gilbert_track$longitude<--(gilbert_track$longitude)


jam_loc<-subset(Cen90_crop, Cen90_crop$location=="Jamaica")
jam_loc[,1]<-NULL
jam_loc[,4]<-NULL
jam_loc<-as.data.frame(jam_loc)
jam_loc<-jam_loc[c(3,2,1)]
colnames(jam_loc)<-c("gridid","glat","glon")  
jam_loc$glon<- (jam_loc$glon)*(-1)




########################################################Get wind speeds at localities##################################################

full_track<-create_full_track(hurr_track = gilbert_track, tint = 3)    #Vmax here is the 10 meter sustained wind speed (m/s). 
with_wind_radii <- add_wind_radii(full_track = full_track)            #Converting from m/s to knots will give you the same numbers
head(with_wind_radii)                                                #as those in the original dataset.

jam_points_list <- split(jam_loc, f = jam_loc$gridid)
jam_winds <- lapply(jam_points_list, FUN = calc_grid_wind,
                    with_wind_radii = with_wind_radii)
names(jam_winds) <- jam_loc$gridid
jam_winds<- bind_rows(jam_winds, .id = "gridid")   #Calculated the wind speed experienced at each locality at each timepoint of the storm



############################################################Merge wind speed at localities with the main dataset######################

h2jam<- as.data.table(new_cropset_at_full)
colnames(h2jam)[1]<-"gridid"




#######fix date on jam_winds
wlsplit<-str_split_fixed(jam_winds$date, " ",2)

wdatefix<-wlsplit[,1]
wtimefix<-wlsplit[,2]
wdsplit<- str_split_fixed(wdatefix, "-",3)


#Need to separate the time variable into a more maliable formal
wtimefix<-as.matrix(wtimefix)

colnames(wtimefix)<-c("time")

wtsplit<-str_split_fixed(wtimefix,":",3)




jam_winds$month<-wdsplit[,2]
jam_winds$day<-wdsplit[,3]
jam_winds$time<-wlsplit[,2]
jam_winds$year<-wdsplit[,1]
#jam_winds$atlantic_cat345_id <- 1:nrow(atlantic_cat345)
jam_winds$time<-wtsplit[,1]

#jam_winds$DateTime<-NULL




#Correct day
for(f in 1: nrow(jam_winds))
{
  if(nchar(jam_winds$day[f])<2){
    jam_winds$day[f]<-paste0("0",jam_winds$day[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}


#Correct month
for(f in 1: nrow(jam_winds))
{
  if(nchar(jam_winds$month[f])<2){
    jam_winds$month[f]<-paste0("0",jam_winds$month[f])
  }
  #else{ stwind_pram$day[f]<-paste0(stwind_pram$day[f],"00")}
}

#Correct time

jam_winds$time<-round(jam_winds$time, 0)

for(f in 1: nrow(jam_winds))
{
  if(nchar(jam_winds$time[f])<2){
    jam_winds$time[f]<-paste0("0",jam_winds$time[f])
  }
  #else{ stwind_pram$time[f]<-paste0(stwind_pram$time[f],"00")}
}


for(f in 1: nrow(jam_winds))
{
  if(nchar(jam_winds$time[f])<2){
    jam_winds$time[f]<-paste0("0",jam_winds$time[f])
  }
  else{jam_winds$time[f]<-paste0(jam_winds$time[f],"00")}
}



jam_winds$date<-paste0(jam_winds$year,jam_winds$month,jam_winds$day, jam_winds$time)
jam_winds<-as.data.frame(jam_winds)





fix_distance<-merge(jam_winds, h2jam, by= c("gridid", "date"), all= TRUE)
jam_fin<-subset(fix_distance, fix_distance$location=="Jamaica" & fix_distance$name=="GILBERT")
jam_fin[4:7]<-NULL


#####################################################Jam_fin above with data on Gilbert in Jamaica#################################################


#wind speed as with the column called wind is measured in meters per second. 
#The speeds that the localities get from the looks of my data tends to be lower than hurricane 3-5 speed. I dont think this should be a problem. 






# 
# ############INFORMATION ABOUT THE WIND SPEED CALCULATIONS###########
#                             #NAAO#
# 
# #The maximum wind speed is measure in meters per second.
# 
# #vmax_gust: Maximum 10-m 1-minute gust wind experienced at the grid point during the storm
# 
#           #Gusts are a few seconds (3-5 s) wind peak. Typically in a hurricane environment, 
#           #the value of the maximum 3 second gust over a 1 minute period is on the order of 1.3 times
#           #(or 30% higher than) than the 1 min sustained wind.
# 
# 
# 
# #vmax_sust: Maximum 10-m 1-minute sustained wind experienced at the grid point during the storm
# 
#           #The maximum sustained wind mentioned in the advisories that NHC issues for tropical
#           #storms and hurricanes are the highest 1 min surface winds occurring within the circulation 
#           #of the system. These "surface" winds are those observed (or, more often, estimated) to occur
#           #at the standard meteorological height of 10 m (33 ft) in an unobstructed exposure (i.e., not blocked by buildings or trees).
# 
# 
# #gust_dur: Duration gust wind was at or above a specified speed (default is 20 m/s), in minutes
# #sust_dur: Duration sustained wind was at or above a specified speed (default is 20 m/s), in minutes
# 

#####################################################################################################################################






# coords2country = function(points_hur)
# {
#   # prepare a SpatialPolygons object with one poly per country
#   countries = map('worldHires', fill=TRUE, col="transparent", plot=FALSE)
#   names = sapply(strsplit(countries$names, ":"), function(x) x[1])
#   
#   
#   #clean up polygons that are out of bounds
#   filter = countries$x < -180 & !is.na(countries$x)
#   countries$x[filter] = -180
#   
#   filter = countries$x > 180 & !is.na(countries$x)
#   countries$x[filter] = 180
#   
#   countriesSP <- getMap(resolution='high')
#   #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
#   
#   # convert our list of points to a SpatialPoints object
#   
#   pointsSP = SpatialPoints(points_hur, proj4string=CRS(" +proj=utm +zone=31 +ellps=clrk66 +units=m +no_defs "))
#   
#   #setting CRS directly to that from rworldmap
#   pointsSP = SpatialPoints(points_hur, proj4string=CRS(proj4string(countriesSP)))
#   
#   #gBuffer(pointsSP, width=1000)
#   # use 'over' to get indices of the Polygons object containing each point
#   indices = over(pointsSP, countriesSP)
#   
#   # return the ADMIN names of each country
#   indices$ADMIN
# }






##################################################Calculate the destruction index for Gilbert in Jamaica###########################################################
#Create the variable to be integrated

x<-jam_fin_list[[1]]$windspeed

d<-function(x){(x^3.8)*jam_fin_list[[1]]$dat[1]}

res<-integrate(d, lower = 1, upper = 13)

#So we got a value from it. Always remember to use the x




#Will have to split the dataset in to lists and lapply over the list



jam_fin_list<-split(jam_fin, jam_fin$gridid, drop = TRUE)
jam_fin_list<-lapply(jam_fin_list, function(x) {as.data.frame(x);x})

#change the date from character to numeric
for(i in 1:length(jam_fin_list)){
  jam_fin_list[[i]]$date<-(as.numeric(jam_fin_list[[i]]$date))
  
}


lister<-data.frame(matrix(ncol = 1, nrow = 596))

for(i in 1:length(jam_fin_list)){
  
  x<-jam_fin_list[[i]]$windspeed
  
  d<-function(x){(x^3.8)*jam_fin_list[[i]]$dat[1]}
  
  res<-integrate(d, lower = jam_fin_list[[i]]$date[1], upper = tail(jam_fin_list[[i]]$date, n=1))#nrow(jam_fin_list[[i]]))
  
  
  lister[i,1]<-res$value
  
}

#Lister holds the proposed integrated results

#Combind the list elements so that we can do the summations

jam1980<-sum(lister) #DISTRUCTION INDEX






####################################################Plotting hurricane tracks for Jamaica###############################

# plot them on a map\
par(bg = 'grey') #Change background of plots
map("worldHires", xlim=c(-119, -40), ylim=c(0, 30))
lines(al_new$Lon[al_new$Name=="ALLEN"], al_new$Lat[al_new$Name=="ALLEN"], col="Red")
lines(al_new$Lon[al_new$Name=="IVAN"], al_new$Lat[al_new$Name=="IVAN"], col="YELLOW")
lines(al_new$Lon[al_new$Name=="IKE"], al_new$Lat[al_new$Name=="IKE"], col="ORANGE")
lines(al_new$Lon[al_new$Name=="DEAN"], al_new$Lat[al_new$Name=="DEAN"],col="purple")
lines(al_new$longitude[al_new$name=="GILBERT"], al_new$latitude[al_new$name=="GILBERT"], col="green")
lines(al_new$longitude[al_new$Name=="DAVID"], al_new$latitude[al_new$Name=="DAVID"], col="aquamarine")

# 


####################################################Plotting hurricane tracks for Puerto Rico###############################

# plot them on a map\
par(bg = 'grey') #Change background of plots
map("worldHires", xlim=c(-119, -40), ylim=c(0, 30))
lines(al_new$Lon[al_new$Name=="ALLEN"], al_new$Lat[al_new$Name=="ALLEN"], col="Red")
lines(al_new$Lon[al_new$Name=="DAVID"], al_new$Lat[al_new$Name=="DAVID"], col="YELLOW")
lines(al_new$Lon[al_new$Name=="IKE"], al_new$Lat[al_new$Name=="IKE"], col="ORANGE")
lines(al_new$Lon[al_new$Name=="DEAN"], al_new$Lat[al_new$Name=="DEAN"],col="purple")
lines(al_new$Lon[al_new$Name=="GILBERT"], al_new$Lat[al_new$Name=="GILBERT"], col="green")



