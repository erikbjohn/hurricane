
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

rm(list=ls())


#setwd("C:\\Users\\slstewart2\\Dropbox\\Jamaica")
setwd("C:/Users/slstewart2/Downloads/Rushaine")

al_new<-fread("al_new.csv")

#al_new<-fread("al_new_1950.csv")

al_new_list<-split(al_new, al_new$name, drop= TRUE)


new_cropset_at_full<-read.csv("new_cropset_at_full.csv")
#new_cropset_at_full<-read.csv("new_cropset_at_full_1950.csv")


Cen90_crop<-fread("Cen90_crop.csv")

full_pop<-fread("share_pop.csv")


#extents<-readRDS("extents_1950")

#list2env(extents,globalenv())

countrynam<-as.vector(sort(unique(new_cropset_at_full$location))) #C

#Cen90_crop[(which(Cen90_crop$Long>=-(blzbox[1,2]) & Cen90_crop$Long<=-(blzbox[1,1]) & Cen90_crop$Lat>=(blzbox[2,1]) & Cen90_crop$Lat<=(blzbox[2,2]))),5]<-"Belize"

#countrynam<-countrynam[c(-2,-8,-25)] #for long set
countrynam<-countrynam[c(-1, -3,-6,-7,-8,-9,-11,-14,-16,-17,-18,-20,-22,-23,-24,-25,-29,-32,-33,-34)] #Shortsset
#countrynam

for(c in 1:length(countrynam)){
  
  if((c %% 1)==0){
    cat(c, 'of', length(countrynam), '\n')
  }
  
  
  country<-subset(new_cropset_at_full,new_cropset_at_full$location==countrynam[c])
  
  #jamaica<-fread("C:\\Users\\goulb\\Dropbox\\Paper Data\\1950 country data\\jamaica.csv")
  country_list<-split(country, country$name, drop = TRUE)
  
  
  ######Fubction to get tracks
  #setwd("C:\\Users\\rdgoulbourne\\New folder\\Dropbox\\Spatial tutorial material")
  # setwd("C:\\Users\\goulb\\Dropbox\\Spatial tutorial material")
  # source('R/country_tracks.R')  ##Call the shape files using this function 
  country_tracks<-al_new_list[names(country_list)]
  
  
  
  country_loc<-subset(Cen90_crop, Cen90_crop$location==countrynam[c])
  country_loc[,1]<-NULL
  country_loc[,4]<-NULL
  country_loc<-as.data.frame(country_loc)
  country_loc<-country_loc[c(3,2,1)]
  colnames(country_loc)<-c("gridid","glat","glon")  
  country_loc$glon<- (country_loc$glon)*(-1)
  
  ###################################################################################################################################################
  
  V_country<-vector("list", length(country_tracks)) 
  
  
  for(i in 1:length(country_tracks)){
    
    
    full_track<-create_full_track(hurr_track =country_tracks[[i]], tint = 3)    #Vmax here is the 10 meter sustained wind speed (m/s). 
    if(nrow(full_track)<2){cat("not enough \n")}else{  
      with_wind_radii <- add_wind_radii(full_track = full_track)            #Converting from m/s to knots will give you the same numbers
      head(with_wind_radii)                                                #as those in the original dataset.
      
      jam_points_list <- split(country_loc, f = country_loc$gridid)
      jam_winds <- lapply(jam_points_list, FUN = calc_grid_wind,
                          with_wind_radii = with_wind_radii)
      names(jam_winds) <- country_loc$gridid
      jam_winds<- bind_rows(jam_winds, .id = "gridid")   #Calculated the wind speed experienced at each locality at each timepoint of the storm
      
      jam_winds<-cbind(jam_winds,jam_winds$date)
      
      # data("floyd_tracks")
      # data("county_points")
      # full_track <- create_full_track(hurr_track = country_tracks[[3]], tint = 3)
      # with_wind_radii <- add_wind_radii(full_track = full_track)
      # wind_grid <- calc_grid_wind(grid_point = country_loc$gridid[5565],
      #                             with_wind_radii = with_wind_radii)
      # head(wind_grid)
      
      
      
      
      
      
      ############################################################Merge wind speed at localities with the main dataset######################
      h1jam<-new_cropset_at_full
      names(h1jam)[1]<-paste("gridid")
      
      
      #correct date
      #setwd("C:\\Users\\goulb\\Dropbox\\Spatial tutorial material")
      source('R/fixdate.R')  
      jam_winds<-datecorrect()
      
      jam_winds<-as.data.table(jam_winds)
      jam_winds$gridid<-as.numeric(jam_winds$gridid)
      jam_winds$date<-as.numeric(jam_winds$date)
      h1jam<-as.data.table(h1jam)
      h1jam$gridid<-as.numeric(h1jam$gridid)
      h1jam$date<-as.numeric(h1jam$date)
      
      setkeyv(jam_winds, c("gridid","date"))
      setkeyv(h1jam,c("gridid","date"))
      
      fix_distance<-jam_winds[h1jam]
      jam_fin<-subset(fix_distance, fix_distance$location==countrynam[c] & fix_distance$name==country_tracks[[i]]$name[1])
      jam_fin<-as.data.frame(jam_fin)
      jam_fin[5:8]<-NULL
      
      colnames(jam_fin)[4]<-"d2"
      
      
      full_track$tclon<--full_track$tclon
      for(j in 1:nrow(full_track)){
        
        full_track$direction<-bearing(full_track[,c(3,2)], a=6378137, f=1/298.257223563)
        
        for(f in 1: nrow(full_track)){
          if(is.na(full_track$direction[f])){full_track$direction[f]<-full_track$direction[f-1]}
        } 
      }
      
      #setwd("C:\\Users\\goulb\\Dropbox\\Spatial tutorial material")
      source('R/fix_full_track_date.R')
      full_track<-datecorrect()
      
      
      
      
      for(f in 1: nrow(jam_fin)){
        
        for(j in 1:nrow(full_track)){
          
          if(jam_fin$date[f]==full_track$date[j]){jam_fin$fpath[f]<-full_track$direction[j]}
        }
      }
      
      #####Calculate the size of the angle between two points#### -AZIMUTH
      for(f in 1: nrow(jam_fin)){
        jam_fin$azimuth[f]<-gzAzimuth(c(-jam_fin[f,c(8)],jam_fin[f,7]),c(-jam_fin[f,c(16)],jam_fin[f,17]), type = "snyder_sphere")
        
      }
      
      
      
      #setwd("C:\\Users\\goulb\\Dropbox\\Spatial tutorial material")
      source('R/get_v_mex.R')  
      RMW<-rmw_use()
      
      
      RMW<-as.data.table(RMW)
      full_pop<-as.data.table(full_pop)
      setkey(RMW, "gridid")
      setkey(full_pop, "ID")
      RMW <-RMW[full_pop]
      RMW<-subset(RMW, !is.na(RMW$date))
      
      
      #Create final value
      if(nrow(RMW)>1){
        
        
        h<-subset(jam_fin, jam_fin$gridid %in% RMW$gridid)
        
        
        # h<-as.data.table(h)
        # h[, ID := .GRP, by = .(gridid, date)]
        # h<-as.data.frame(h)
        h_list<-split(h, h$gridid, drop = TRUE)
        
        for(p in 1:length(h_list)){                                 #dates must be changed from characters to numeric to integrate
          h_list[[p]]$date<-(as.numeric(h_list[[p]]$date))}
        
        
        
        #pop<-subset(pop_ja, pop_ja$ID %in% RMW$gridid) #Had to reduce the population size to the that in the 177
        
        RMW<-as.data.table(RMW)
        RMW[, ID := .GRP, by = .(gridid, date)]
        RMW<-as.data.frame(RMW)
        RMW_list<-split(RMW, RMW$ID, drop = TRUE)
        RMW_list<-lapply(RMW_list, function(x) {as.data.frame(x);x})
        
        #change the date from character to numeric
        for(k in 1:length(RMW_list)){
          RMW_list[[k]]$date<-(as.numeric(RMW_list[[k]]$date))
          
        }
        
        RMW_int<-split(RMW, RMW$gridid, drop = TRUE)
        #Dont need to Create a list to get the integrals since h already has the same stuff
        #RMW_int[names(RMW_int)==RMW_list[[1]]$gridid]
        
        
        lister<-data.frame(matrix(ncol = 1, nrow = length(RMW_list)))
        
        for(l in 1:length(RMW_list)){
          
          w<-paste0("dat",RMW_list[[l]]$i.year-1)
          
          x<-RMW_list[[l]]$V
          if(is.null(RMW_list[[l]][1,w]) && RMW_list[[l]]$i.year==1950){w<-"dat1950"}
          if(is.null(RMW_list[[l]][1,w]) && RMW_list[[l]]$i.year>2010){w<-"dat2010"}
          d<-function(x){(x^3.8)*RMW_list[[l]][1,w]}
          
          u<-RMW_int[names(RMW_int)==RMW_list[[l]]$gridid]
          p<-nrow(u[[1]])
          
          res<-integrate(d, lower = 0, upper = p)
          
          #res<-integrate(d, lower =RMW_list[[i]]$date[1], upper = tail(RMW_list[[i]]$date, n=1))#nrow(RMW_list[[i]]))
          
          
          lister[l,1]<-res$value
          
        }
        
        
        
        #Lister holds the proposed integrated results
        
        #Combind the list elements so that we can do the summations
        
        V_country[[i]]<-sum(lister,na.rm = TRUE) #DISTRUCTION INDEX
        #V_jam=V_jam[-(which(sapply(V_jam,is.null),arr.ind=TRUE))]
      }
      
      #if(nrow(RMW)>0){
      #  strong_jam[[i]]<-RMW
      #}
    }
    
  }
  
  nam<-matrix(NA, nrow=length(country_tracks),ncol=2)
  
  for(m in 1:length(country_tracks)){nam[m,1]<-(country_list[[m]]$year[1])}
  for(m in 1:length(country_tracks)){nam[m,2]<-as.character(country_list[[m]]$name[[1]])} 
  
  
  V_country<-cbind(V_country, nam)
  
  
  V_country<-V_country[which(V_country[1:nrow(V_country),1]!="NULL"),]
  
  #setwd("C:\\Users\\slstewart2\\Dropbox\\Jamaica\\Results")
  saveRDS(V_country, file = paste(countrynam[c]))
  write.csv(V_country, file = paste0(countrynam[c],".csv"))
  #V_jam<-readRDS("V_jam")
  
}






