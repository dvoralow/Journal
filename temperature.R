#Temperatures
#download the layer from "Bio oracle"

library(dismo)
library(jsonlite)


#----------functions data-------------


sstmean <- raster("bio oracle/Present.Surface.Temperature.Mean.asc")
sstmin <- raster("bio oracle/Present.Surface.Temperature.Min.asc")
sstmax <- raster("bio oracle/Present.Surface.Temperature.Max.asc")
sstrange <- raster("bio oracle/Present.Surface.Temperature.Range.asc")

sp.data<-read.csv(file = "prosseced data/siganus_occur.csv", row.names=1, stringsAsFactors=FALSE)


# Extracting the temperatures 


lon<-sp.data$lon
lat<-sp.data$lat

sp.occr.latlon<-as.data.frame(cbind(lon,lat))

sp.mean.tmp <- as.data.frame(extract(sstmean, sp.occr.latlon))
sp.min.tmp <- as.data.frame(extract(sstmin, sp.occr.latlon))
sp.max.tmp <- as.data.frame(extract(sstmax, sp.occr.latlon))
sp.range.tmp <- as.data.frame(extract(sstrange, sp.occr.latlon))

colnames(sp.mean.tmp)<-"mean.tmp"
colnames(sp.min.tmp)<-"min.tmp"
colnames(sp.max.tmp)<-"max.tmp"
colnames(sp.range.tmp)<-"range.tmp"

sp.data<-cbind(sp.data,sp.mean.tmp,sp.min.tmp,sp.max.tmp,sp.range.tmp)

write.csv(sp.data,file = "prosseced data/occr_tmp.csv")

#----- t prefference---------
#calculate the preffernce temperature of the speceis with a simple avarage of all the
#obsarvations were taken from GBIF, and their temperature taken from BioOracle


#clean the outlayers
t.pref.data=sp.data
t.pref.data=t.pref.data[!is.na(t.pref.data$mean.tmp),]

#loop for calculating the mean temperature for every species
species_n<-unique(t.pref.data$species)
mean_temp<-c()
for (i in 1:length(species_n)
) {
  
  calc_data=subset(t.pref.data,species==species_n[i])
  mean_data=mean(calc_data$mean.tmp)
  mean_temp=rbind(mean_temp,mean_data)
  
}
row.names(mean_temp)=species_n
colnames(mean_temp)=c("Tpref")

#bind the sp_code to the T_pref data
#!the WHT code isn't apear in the t_pre data because its duplicate - the DIP species with Lmo species
scientific_names_seperate=read.csv(file= "prosseced data/species_names_seperate_fish_known_origin.csv")
sp_code=scientific_names_seperate$Sp_code
sp_code=sp_code[1:42]
mean_temp=cbind(mean_temp,sp_code)
mean_temp=cbind(mean_temp,sp_code=rep("BAT",4))

write.csv(mean_temp,file ="prosseced data/mean_temp_prefference.csv")

