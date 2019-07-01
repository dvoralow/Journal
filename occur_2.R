# Species occurrences and temperatures
#take the abundess data from the site "GBIF"

library(dismo)
library(jsonlite)
library(memisc)

library(ggplot2)
library(ggmap)
library(maptools)
library(maps)


#----------functions data-------------




new.data <- read.csv("data/species name_seperate.csv")

new.data = new.data[new.data$Sp_code=="Sig",] 

#names(new.data)[6]="species"

new.data=new.data[!is.na(new.data$Origin),]

new.data$scintific_name = as.character(new.data$scintific_name)

# #remove shrimps, crabs, octopus, squids.
# new.data=new.data[new.data$species!="Loligo vulgaris" & new.data$species!= "Octopus vulgaris" & new.data$species!= "Parapeneaus longirostris"
#                   & new.data$species!= "Sepia officinalis" & new.data$species!= "Metapenaeopsis aegyptia" & new.data$species!="Penaeus japonicus"
#                   & new.data$species!="Portunus segnis",]
# synonym.name=c(55,"Eha",1,"Daba","Hyporthodus haifensis")
# new.data=rbind(new.data,synonym.name)

##################################################################################################################


species<-unique(new.data$scintific_name)
sp.data<-c()


for(i in 1:length(species)){
  
  s<-species[i]
  
  print(s)
  print(i)
  
  #isolating the required species
  gn.sp<-strsplit(s, " ")
  
  #?? I need both of the names, how to do that??
  if(s == "Epinephelus haifensis"){
    gn.sp<-strsplit("Hyporthodus haifensis", " ")
  }
  
  
  if(s == "Alectis alexandrinus "){
    gn.sp<-strsplit("Alectis alexandrina", " ")
  }
 
  if(s == "Diplodus saragus"){
    gn.sp<-strsplit("Diplodus sargus", " ")
  }
  
  if(s == "Diplodus puntazoo"){
    gn.sp<-strsplit("Diplodus puntazzo", " ")
  }
  
  
  #?? lobster - get out from the data??
  if(s == "Parapeneaus longirostris"){
    gn.sp<-strsplit("Parapenaeus longirostris", " ")
  }
  
  if(s == "Spicara manena"){
    gn.sp<-strsplit("Spicara maena", " ")
  }
  
  gn<-gn.sp[[1]][1]
  sp<-gn.sp[[1]][2]
  
  
  
    sp.occr<-gbif(genus=gn ,species=sp)
    
    # select the columns: "lat","lon"
    
    sp.occr<-subset(sp.occr,select=c("lat","lon"))
    sp.occr$species<-s
    
    #cleaning the data from NA's
    sp.occr<-subset(sp.occr,!is.na(lon) & !is.na(lat))
    sp.occr<-sp.occr[complete.cases(sp.occr),]
    
    # remove duplicates
    dups <- duplicated(cbind(sp.occr$lon,sp.occr$lat))
    sp.occr <- sp.occr[!dups, ]
    
    # nrow=0
    
    if(nrow(sp.occr)==0){
      
    }
    
    # add species occurences to data
    sp.data<-rbind(sp.data,sp.occr)
  }

#to join Epinephelus haifensis and Hyporthodus haifensis

write.csv(sp.data, file= "prosseced data/sp.data.csv")
write.csv(sp.data, file= "prosseced data/siganus_occur.csv")




mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

for(i in 1:length(species)){
 
  sp.occr<-sp.data[sp.data$species==species[i],]
  
  p <- ggplot(sp.occr,aes(x=lon, y=lat)) +
    mapWorld +
    geom_point(color="blue", size=3) + #layer the occurrences on top
   ggtitle(species[i])
  print(p)
}



#----try without the loop-----

s<-species[10]

print(s)

#isolating the required species
gn.sp<-strsplit(s, " ")


#---change names
if(s == "Epinephelus haifensis"){
  gn.sp<-strsplit("Hyporthodus haifensis", " ")
}

if(s == "Alectis alexandrinus"){
  gn.sp<-strsplit("Alectis alexandrina", " ")
}

if(s == "Diplodus saragus"){
  gn.sp<-strsplit("Diplodus sargus", " ")
}

if(s == "Diplodus puntazoo"){
  gn.sp<-strsplit("Diplodus puntazzo", " ")
}

#?? lobster - get out from the data??
if(s == "Parapeneaus longirostris"){
  gn.sp<-strsplit("Parapenaeus longirostris", " ")
}

if(s == "Spicara manena"){
  gn.sp<-strsplit("Spicara maena", " ")
}

gn<-gn.sp[[1]][1]
sp<-gn.sp[[1]][2]

#if( s != "Arnoglossus sp."){
  #import occurrence data
  sp.occr<-gbif(genus=gn ,species=sp)
  
  # select the columns: "lat","lon"
  
  sp.occr<-subset(sp.occr,select=c("lat","lon"))
  sp.occr$species<-s
  
  #cleaning the data from NA's
  sp.occr<-subset(sp.occr,!is.na(lon) & !is.na(lat))
  sp.occr<-sp.occr[complete.cases(sp.occr),]
  
  # remove duplicates
  dups <- duplicated(cbind(sp.occr$lon,sp.occr$lat))
  sp.occr <- sp.occr[!dups, ]
  
  # nrow=0
  
  if(nrow(sp.occr)==0){
    
  }
  
  # add species occurences to data
  sp.data<-rbind(sp.data,sp.occr)
#}
  
  
  
 #on map 
  sp.occr<-sp.data[sp.data$species==species[1],]
 
  
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() +   mapWorld
  
  p <- ggplot(sp.occr,aes(x=lon, y=lat)) +
   mapWorld +
    geom_point(color="blue", size=3) + #layer the occurrences on top
    ggtitle(species[1])
  print(p)
  