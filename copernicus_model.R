library(ncdf4)
library(chron)
library(ncdf4.helpers)
library(R.utils)
library(data.table)
library(tidyr)
library(MASS)
library(ggplot2)
library(ggmap)
library(maptools)
library(marelac)
library(rgeos)
library(raster)
library(geosphere)
library(RColorBrewer)
library(plyr)
library(rgdal)
library(stringr)




#in lab computer
setwd("C:/Users/dvora/Google Drive/second degree/thesis/data/copernicus")

#in my computer
setwd("C:/Users/Admin/Google Drive/second degree/thesis/data/copernicus")


folders=list.files()
ncFile_ex <- nc_open("2011.nc" )
analysed_sst_range=ncvar_get(ncFile_ex, "analysed_sst")









#get one file for example for the depth vector

axes.map_in=nc.get.dim.axes(ncFile_ex, "votemper")#this will be used later as an input for a function (for some reason needed in files later than 2003)
depth <- ncvar_get(ncFile_ex, "depth")
time_range=ncvar_get(ncFile_ex, "time")


lon_range=ncvar_get(ncFile_ex, "lon")
lat_range=ncvar_get(ncFile_ex, "lat")
nc_close(ncFile_ex )
unlink("yr2014/20140101_dm-INGV--TEMP-MFSe1r1-MED-b20160115_re-fv06.00.nc")


###Function: Get_close_location
##Description: 
##Inputs:The inputs are Lon and Lat (degrees) of the desired location
##Output: The closest location available in copernicus data

##Input:
#lat_in-latitude (e.g 32.22)
#lon_in- lontitude (e.g 34.22)
#depth_in- sample depth (e.g 30)
Get_close_location=function(lat_in,lon_in,depth_in){
  #Get the closest point from the available values in 'lat_range' vector
  cop_lat=lat_range[which.min(abs(lat_in-lat_range))]
  #Get the closest point from the available values in 'lon_range' vector
  cop_lon=lon_range[which.min(abs(lon_in-lon_range))]
  #Get the closest depth
  cop_depth=depth[which.min(abs(depth_in-depth))]
  #return vector
  return(list(lat_out=cop_lat,lon_out=cop_lon,depth_out=cop_depth))
}

##Example
Get_close_location_test=Get_close_location(lat_in=32.5,lon_in=34.2,depth_in=30)
Get_close_location_test[["lat_out"]]

###Function: Identify_NC_file
##Description: 
##Inputs:The date of the sample
##Output: Which file to select

##Input:
#year_in-sample year as.character(e.g "2008")
#month_in-sample year as.character (e.g "08")
#day_in-sample year as.character(e.g "01")

Identify_NC_file=function(year_in,month_in,day_in){
  folder_name=paste("yr",year_in,sep="")
  #list the files in the folder
  year_files=list.files(folder_name)
  #get the date fraction of the file name
  get_date_fraction=word(year_files,1,sep = "\\_")
  #create date string from the inputs
  date_in=paste(year_in,month_in,day_in,sep="")
  #identify the relevant file
  file_path_out=year_files[which(get_date_fraction == date_in)]
  return(file_path_out)
}

##Example
Identify_NC_file(year_in="2008",month_in="01",day_in="01")




#first function for single file will be used insid second function
get.tempF=function(NC,Lon,Lat,depth_v){
  temp_dt=data.frame()
  for (d in depth_v){
    LonStartIdx <- which( NC$dim$lon$vals %in% Lon)
    #print(NC$dim$lon$vals)
    #print(Lon_min)
    #print(Lon_max)
    #print(LonStartIdx)
    LatStartIdx <- which( NC$dim$lat$vals %in% Lat)
    #print(str(LatStartIdx))
    #print("ok")
    nc_sub=data.frame(nc.get.var.subset.by.axes(NC, "votemper", list(Z=which(depth_v %in% d),X=LonStartIdx, Y=LatStartIdx,T=1),axes.map=axes.map_in))
    
    #print("ok1")
    rownames(nc_sub)=as.character(NC$dim$lon$vals[LonStartIdx])
    colnames(nc_sub)=as.character(NC$dim$lat$vals[LatStartIdx])
    grid_cord=data.frame(expand.grid(as.character(NC$dim$lon$vals[LonStartIdx]),as.character(NC$dim$lat$vals[LatStartIdx])))
    colnames(grid_cord)=c("lon","lat")
    grid_cord$temp=rep(NA,nrow(grid_cord))
    for(i in 1:nrow(grid_cord)){
      grid_cord[i,"temp"]=nc_sub[as.character(grid_cord[i,1]),as.character(grid_cord[i,2])]
    }
    grid_cord$depth=rep(d,nrow(grid_cord))
    temp_dt=rbind(temp_dt,grid_cord)
    temp_dt$lon=as.numeric(as.character( temp_dt$lon))
    temp_dt$lat=as.numeric(as.character( temp_dt$lat))
  }
  return(temp_dt)
}

###Function: get_location_date_data
##Description: 
##Inputs:Date and location and depth of the sample
##Output: Copernicus data

##Input:
#lat_in-latitude (e.g 32.22)
#lon_in- lontitude (e.g 34.22)
#depth_in- sample depth (e.g 30)
#year_in-sample year as.character(e.g "2008")
#month_in-sample year as.character (e.g "08")
#day_in-sample year as.character(e.g "01")

get_location_date_data=function(lat_in,lon_in,depth_in,year_in,month_in,day_in){
  ##1. get the location and file details
  #Identify location and depth. calling function 'Get_close_location'
  location_depth_out=Get_close_location(lat_in,lon_in,depth_in)
  #Identify the folder anf file. calling function 'Identify_NC_file'
  folder_file_out=Identify_NC_file(year_in,month_in,day_in)
  
  ##2.locate and unzip the file
  #add folder
  folder_name=paste("yr",year_in,sep="")
  folder_file_out_full=paste(folder_name,folder_file_out,sep="/")
  #Unzip
  t=gunzip(folder_file_out_full,remove=F)
  ncFile <- nc_open(t)
  #set location and depth using  output above 'location_depth_out'
  Lon_min_in=location_depth_out[["lon_out"]]
  Lat_min_in=location_depth_out[["lat_out"]]
  depth_v_in=location_depth_out[["depth_out"]]
  file_info=get.tempF(ncFile,Lon_min_in,Lat_min_in,depth_v_in)
  #remove unziped file and close nc file
  unzip_file=substr(folder_file_out,1,nchar(folder_file_out)-3)
  nc_close(ncFile)
  unlink(paste(folder_name,unzip_file,sep="/"))
  #Add date 
  file_info$year=year_in
  file_info$month=month_in
  file_info$day=day_in
  file_info$lat_original=lat_in
  file_info$lon_original=lon_in
  file_info$depth_original=depth_in
  #return 
  return(file_info)
}

#Example
get_location_date_data(lat_in=32.33,lon_in=34.21,depth_in=22,year_in="2009",month_in="08",day_in="02")
get_location_date_data(lat_in=31.91443,lon_in = 34.46506,depth_in =146.2195,year_in = "2002",month_in = "01",day_in = "01" )

#----Dvora code---
#upload the nets data
all_data=read.csv(file = "C:/Users/JonathanB12/Google Drive/second degree/thesis/data/R/prosseced data/all_data_fishes_known_origin.csv")

#convert the date columns to character
all_data$Day=as.character(all_data$Day)
all_data$Month=as.character(all_data$Month)



#convert NA to 0
all_data$Day[is.na(all_data$Day)]=0
all_data$Month[is.na(all_data$Month)]=0
all_data$Lon_final[is.na(all_data$Lon_final)]=0
all_data$Lat_final[is.na(all_data$Lat_final)]=0
all_data$loc_DEPTH[is.na(all_data$loc_DEPTH)]=0


#change the form of the dates (from "1" to "01" inc.)
for (i in c(1:9)) {
  for (j in 1:nrow(all_data))
    {
      if(all_data[j,"Day"]==i)
      {all_data[j,"Day"]=paste("0",i,sep = "")}
    
      if(all_data[j,"Month"]==i)
      {all_data[j,"Month"]=paste("0",i,sep = "")}
    }
}


#--loop to recieve the temperature from every net
  #create empty data frames for the loop
  net_temp=data.frame("lon"=NA,"lat"=NA,"temp"=NA,"depth"=NA,"year"=NA,"month"=NA,"day"=NA,
                    "lat_original"=NA,"lon_original"=NA,"depth_original"=NA)
  net_temp=net_temp[0,]
  temp_vec=data.frame("lon"=NA,"lat"=NA,"temp"=NA,"depth"=NA,"year"=NA,"month"=NA,"day"=NA,
                    "lat_original"=NA,"lon_original"=NA,"depth_original"=NA)
  j=0
  
  
for (i in 10800:nrow(all_data)) {
  
  #ignore the 0 cells [it's was NA before]
    if((all_data[i,"Month"]==0) || (all_data[i,"Day"]==0) || (all_data[i,"Lon_final"]==0) || (all_data[i,"Lat_final"]==0)
       || (all_data [i,"loc_DEPTH"]==0) || (all_data[i,"Year"]<1987)||((all_data[i,"Day"]==13)&(all_data[i,"Month"]==10)&(all_data[i,"Year"]==1987) ))
     {
        temp_vec[1,]=0
        net_temp=rbind(net_temp,temp_vec)
      }
  else{
        temp_vec=get_location_date_data(lat_in = all_data[i,"Lat_final"],lon_in = all_data[i,"Lon_final"],
                                       depth_in =all_data[i,"loc_DEPTH"],year_in =as.character( all_data[i,"Year"]),
                                       month_in = all_data[i,"Month"],day_in = all_data[i,"Day"] )
        net_temp=rbind(net_temp,temp_vec)
        }
  if(j==100)
    {
    write.csv(net_temp,file ="C:/Users/JonathanB12/Google Drive/second degree/thesis/data/R/prosseced data/net_temperatures3.csv" )
    j=0
  }
  j=j+1
  }
#trycatch - dont stop if eror
write.csv(net_temp,file = "C:/Users/JonathanB12/Google Drive/second degree/thesis/data/R/prosseced data/net_temperatures3.csv")


