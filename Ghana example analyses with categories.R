#Ghana plants
rm(list=ls())#Clean workspace
#Install packages
require(data.table)
require(raster)
require(dismo)
require(rasterVis)
require(ENMeval)
require(MASS)
require(sp)

#Get data
data<-read.delim(unz('GBIFdownload_Oct2018.zip','occurrence.txt'),sep='\t',quote="",dec='.',header=T)
data<-data[!is.na(data$decimalLatitude),]
coords <- cbind(data$decimalLongitude, data$decimalLatitude)
ghana_species_pts <- SpatialPointsDataFrame(coords, data, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
crs(dataSpatial)

#Read in eco services table
GhanaUses<-fread('FinalDataSpeciesNames.csv',header=T)
#Number of species
length(levels(as.factor(GhanaUses$Species)))

#Drop records missing species
Ghana_Species<-GhanaUses[GhanaUses$Species!="",]
#Ghana_Species<-GhanaUses[!is.na(GhanaUses$Species),]
View(Ghana_Species)

# #GBIF records
# #Read directly from zip directory
# gbifrecs<-read.delim(unz('GBIFdownload_Oct2018.zip','occurrence.txt'),sep='\t',quote="",dec='.',header=T)
# head(gbifrecs)
# summary(gbifrecs)
# dim(gbifrecs)

#Make spatial point data frame
Ghana_Species_pts<-SpatialPointsDataFrame(cbind(Ghana_Species$DecimalLongitude,Ghana_Species$DecimalLatitude),Ghana_Species,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

#Plot map of Ghana regions 
ghanamap<-getData('GADM',country='GHA',level=1)
plot(ghanamap)
plot(dataSpatial, add=T)
crs(ghanamap)
points(ghana_species_pts,pch=16,cex=0.1,col='red') #Species occurence points

#Extract records outside of Ghana land (i.e. sea, other countries)
ghana_species_pts_insidemap<-ghana_species_pts[ghanamap,]
points(ghana_species_pts_insidemap,cex=0.1,pch=16,col='blue')
legend('r',pch=16,col=c('red','blue'),c('Discarded records','Retained records'))
title( main = "Species Occurrence in Ghana")

#Dataset size
dim(ghana_species_pts_insidemap@data)

#Number of observations per species
species_records<-rev(sort(with(ghana_species_pts_insidemap@data,tapply(species,species,length))))
length(species_records)
length(levels(as.factor(ghana_species_pts_insidemap@data$species)))

#Very skewed number of observations per species
species_records[1:20]
hist(species_records,breaks='Scott')

#663 species with >20 records

length(species_records[species_records>20])

#So we have 48478 records from  4201 plant species

with(ghana_species_pts_insidemap@data,tapply(basisOfRecord,basisOfRecord,length))

#KML(ghana_species_pts_insidemap,'KMLGBIFrecords')




# Example MaxEnt model ----------------------------------------------------

#Elevation data
ghanaalt<-getData('alt',country='GHA')

levelplot(ghanaalt,margin=F,main='Elevation (m)')+
  layer(sp.polygons(ghanamap))

#WorldClim data
#2.5minute resolution is ca.4.5km at equator
gc1<-getData('worldclim',var='bio',res=2.5)
#Crop and mask to elevation data
ghanaC<-crop(gc1,ghanamap)
ghana1<-mask(ghanaC,ghanamap)
levelplot(ghana1$bio12,margin=F,main='Annual precipitation (mm)',par.settings='RdBuTheme')+
  layer(sp.polygons(ghanamap))

#Make a pairs plot to check for correlated variables
pairs(ghana1)#Many highly correlated varaibles as expected

#Reduce dimensionality
climdat<-values(ghana1)
dim(climdat)
#Remove NAs

climdat1<-climdat[!is.na(climdat[,1]),]


pca1<-princomp(climdat1)
plot(pca1)
biplot(pca1) #4 and 16 not correlated
biplot(pca1,choices=c(2,3))

pairs(ghana1[[c(4,7,12,13,16,17,19)]])
#Use Bio4 and BIo16 (Temp seasonality and precipitation of wettest quarter)

#Landcover data
#install.packages("rgdal")
lc1975<-raster('Landcover maps/west_africa_land-use_land-cover_1975_2km/swa_1975lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc1975))
ghanalc1975<-mask(crop(lc1975,ghanat),ghanat)
plot(ghanalc1975)
#NB legend is in RAT table
levels(ghanalc1975)
title( main = "Land cover 1975")
legend('r',pch=16,col=c('green','pink','gray'),c('forest','degraded forest','agriculture'))

#Do the same with other years too
lc2000<-raster('Landcover maps/west_africa_land-use_land-cover_2000_2km/swa_2000lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc2000))
ghanalc2000<-mask(crop(lc2000,ghanat),ghanat)
plot(ghanalc2000)
levels(ghanalc2000)
title( main = "Land cover 2000")
legend('r',pch=16,col=c('green','pink','gray'),c('forest','degraded forest','agriculture'))

lc2013<-raster('Landcover maps/west_africa_land-use_land-cover_2013_2km/swa_2013lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc2013))
ghanalc2013<-mask(crop(lc2013,ghanat),ghanat)
plot(ghanalc2013)
levels(ghanalc2013)
title( main = "Land cover 2013")
legend('r',pch=16,col=c('green','pink','gray'),c('forest','degraded forest','agriculture'))

#Cropping Ghana from Global population density data
pd2000<-raster('human population data/gpw2000_30_sec.tif')
pd2000ghana1<-crop(pd2000,ghanamap)
pd2000ghana<-mask(pd2000ghana1,ghanamap)
plot(pd2000ghana)
title( main = "Population density 2000")

pd2005<-raster('human population data/gpw2005_30_sec.tif')
pd2005ghana1<-crop(pd2005,ghanamap)
pd2005ghana<-mask(pd2005ghana1,ghanamap)
plot(pd2005ghana)
title( main = "Population density 2005")

pd2010<-raster('human population data/gpw2010_30_sec.tif')
pd2010ghana1<-crop(pd2010,ghanamap)
pd2010ghana<-mask(pd2010ghana1,ghanamap)
plot(pd2010ghana)
title( main = "Population density 2010")

pd2015<-raster('human population data/gpw2015_30_sec.tif')
pd2015ghana1<-crop(pd2015,ghanamap)
pd2015ghana<-mask(pd2015ghana1,ghanamap)
plot(pd2015ghana)
title( main = "Population density 2015")

pd2020<-raster('human population data/gpw2020_30_sec.tif')
pd2020ghana1<-crop(pd2020,ghanamap)
pd2020ghana<-mask(pd2020ghana1,ghanamap)
plot(pd2020ghana)
title( main = "Population density 2020")

#Stack up popdata
popdat<-stack(pd2000ghana,pd2005ghana,pd2010ghana,pd2015ghana,pd2020ghana)

#Resample to same grid as climate data
popdatrs<-resample(popdat,ghana1,method="bilinear")
writeRaster(popdatrs,'GhanaPopData')
ghanapopdat<-stack('GhanaPopData')

#Stack up lcdata
lcdat<-stack(lc1975,lc2000,lc2013)

#Project to same crs as climate data
lcdatp<-projectRaster(lcdat,ghana1,method="ngb")

#Resample to same grid as climate data
lcdatrs<-resample(lcdatp,ghana1,method="ngb")#Nearest neighbour as categorical

ghana_envvars<-stack(ghana1,mask(ghanapopdat,ghana1[[1]]),mask(lcdatrs,ghana1[[1]]))

#Merging health care with gbif recs to a data frame 
healthcare_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Health care']),]
dim(healthcare_gbif)
levels(droplevels(healthcare_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"]))))
ecosyshcsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"])))
gbifhcsp<-levels(droplevels(healthcare_gbif$species)) 
ecosyshcsp[which(ecosyshcsp%in%gbifhcsp==F)]

#Basic MaxEnt with two predictors for Health care
library(rJava)
dat=healthcare_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the species
p1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Health care',scales=list(draw=F),xlab=NULL,ylab=NULL)+layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)

#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters<-ENMevaluate(healthcare_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

 tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Health care',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(healthcare_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')

#Doing same for other categories

#Merging agriculture with gbif recs to a data frame
agricult_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Agriculture']),]
dim(agricult_gbif)
levels(droplevels(agricult_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Agriculture"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Agriculture"]))))
ecosysagrsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Agriculture"])))
gbifagrsp<-levels(droplevels(agricult_gbif$species)) 
ecosysagrsp[which(ecosysagrsp%in%gbifagrsp==F)]

#Basic MaxEnt with two predictors for Agriculture
library(rJava)
dat=agricult_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],agricult_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the speciesp1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Agriculture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)

#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters<-ENMevaluate(agricult_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],agricult_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Agriculture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(agricult_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],agricult_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')



#Merging water purification with gbif recs to a data frame 
purif_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Water purification']),]
dim(purif_gbif)
levels(droplevels(purif_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purification"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purificaion"]))))
ecosyspursp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purification"])))
gbifpursp<-levels(droplevels(purif_gbif$species)) 
ecosyspursp[which(ecosyspursp%in%gbifpursp==F)]

#Basic MaxEnt with two predictors for Water purifiction
library(rJava)
dat=purif_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],purif_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the speciesp1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Water purification',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)

#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters<-ENMevaluate(purif_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],purif_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Water purification',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(purif_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],purif_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')


#Merging construction with gbif recs to a data frame
construct_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Construction']),]
dim(construct_gbif)
levels(droplevels(construct_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"]))))
ecosysconssp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"])))
gbifconssp<-levels(droplevels(construct_gbif$species)) 
ecosysconssp[which(ecosysconssp%in%gbifconssp==F)]

#Basic MaxEnt with two predictors for Construction
library(rJava)
dat=construct_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],construct_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the speciesp1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Construction',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)

#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters<-ENMevaluate(construct_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],construct_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Construction',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(construct_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],construct_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')


#Merging social with gbif recs to a data frame
social_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Social']),]
dim(social_gbif)
levels(droplevels(social_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"]))))
ecosyssocsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"])))
gbifsocsp<-levels(droplevels(social_gbif$species)) 
ecosyssocsp[which(ecosyssocsp%in%gbifsocsp==F)]

#Basic MaxEnt with two predictors for social
library(rJava)
dat=social_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],social_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the speciesp1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Social',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)

#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters<-ENMevaluate(social_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],social_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Social',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(social_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],social_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')


#Merging energy with gbif recs to a data frame 
energy_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Energy']),]
dim(energy_gbif)
levels(droplevels(energy_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"]))))
ecosysensp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"])))
gbifensp<-levels(droplevels(energy_gbif$species)) 
ecosysensp[which(ecosysensp%in%gbifensp==F)]

#Basic MaxEnt with two predictors for energy
library(rJava)
dat=energy_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],energy_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the species
p1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Energy',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)
#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters<-ENMevaluate(energy_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],energy_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Energy',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(energy_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],energy_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')


#Merging food and nutrition with gbif recs to a data frame
foodnutr_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Food and nutrition']),]
dim(foodnutr_gbif)
levels(droplevels(foodnutr_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"]))))
ecosysfnsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"])))
gbiffnsp<-levels(droplevels(foodnutr_gbif$species)) 
ecosysfnsp[which(ecosysfnsp%in%gbiffnsp==F)]

#Basic MaxEnt with two predictors for Food and nutrition
library(rJava)
dat=foodnutr_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],foodnutr_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the speciesp1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Food and nutrition',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)

#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters<-ENMevaluate(foodnutr_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],foodnutr_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Food and nutrition',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(foodnutr_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],foodnutr_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')



#Merging Culture with gbif recs to a data frame 
cultur_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Culture']),]
dim(cultur_gbif)
levels(droplevels(cultur_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"]))))
ecosysculsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"])))
gbifculsp<-levels(droplevels(cultur_gbif$species)) 
ecosysculsp[which(ecosysculsp%in%gbifculsp==F)]

#Basic MaxEnt with two predictors for culture
library(rJava)
dat=cultur_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],cultur_gbif,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html

#Predict habitat suitability for the species
p1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Culture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras1 <- raster(densgbifrecs)
#Make sure density layer has same resolution and extent as env. data
densgbifrecs_ras<-mask(resample(densgbifrecs_ras1,ghana_envvars),ghana_envvars[[1]],maskvalue=NA)

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile

levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(SpatialPoints(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters<-ENMevaluate(cultur_gbif@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)


tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],cultur_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))


maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Culture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(energy_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],cultur_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='C:/Users/saray/Sarah/MASTERS PROJECT/GhanaEcoServices1/ExampleMaxEnt')
