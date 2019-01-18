#Ghana plants
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
crs(ghanamap)

plot(ghanamap)
plot(dataSpatial, add=T)
head(GhanaUses)
summary(GhanaUses)

#Number of species
length(levels(as.factor(GhanaUses$Species)))

#Drop records missing species
Ghana_Species<-GhanaUses[GhanaUses$Species!="",]
#Ghana_Species<-GhanaUses[!is.na(GhanaUses$Species),]
View(Ghana_Species)

#Make spatial point data frame
Ghana_Species_pts<-SpatialPointsDataFrame(cbind(Ghana_Species$DecimalLongitude,Ghana_Species$DecimalLatitude),Ghana_Species,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

#Plot map of Ghana regions 
ghanamap<-getData('GADM',country='GHA',level=1)
plot(ghanamap)
plot(dataSpatial, add=T)
crs(ghanamap)
points(Ghana_Species_pts,pch=16,cex=0.1,col='red') #Species occurence points

#Extract records outside of Ghana land (i.e. sea, other countries)
ghana_species_pts_insidemap<-ghana_species_pts[ghanamap,]
points(ghana_species_pts_insidemap,cex=0.1,pch=16,col='blue')
legend('r',pch=16,col=c('red','blue'),c('Discarded records','Retained records'))

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

ghanalc2000p<-projectRaster(ghanalc2000,crs=crs(ghana1))

#Do the same with other years too
lc2000<-raster('Landcover maps/west_africa_land-use_land-cover_2000_2km/swa_2000lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc2000))
ghanalc2000<-mask(crop(lc2000,ghanat),ghanat)
plot(ghanalc2000)
levels(ghanalc2000)

lc2013<-raster('Landcover maps/west_africa_land-use_land-cover_2013_2km/swa_2013lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc2013))
ghanalc2013<-mask(crop(lc2013,ghanat),ghanat)
plot(ghanalc2013)
levels(ghanalc2013)

#Cropping Ghana from Global population density data
pd2000<-raster('human population data/gpw2000_30_sec.tif')
pd2000ghana1<-crop(pd2000,ghanamap)
pd2000ghana<-mask(pd2000ghana1,ghanamap)
plot(pd2000ghana)

pd2005<-raster('human population data/gpw2005_30_sec.tif')
pd2005ghana1<-crop(pd2005,ghanamap)
pd2005ghana<-mask(pd2005ghana1,ghanamap)
plot(pd2005ghana)

pd2010<-raster('human population data/gpw2010_30_sec.tif')
pd2010ghana1<-crop(pd2010,ghanamap)
pd2010ghana<-mask(pd2010ghana1,ghanamap)
plot(pd2010ghana)

pd2015<-raster('human population data/gpw2015_30_sec.tif')
pd2015ghana1<-crop(pd2015,ghanamap)
pd2015ghana<-mask(pd2015ghana1,ghanamap)
plot(pd2015ghana)

pd2020<-raster('human population data/gpw2020_30_sec.tif')
pd2020ghana1<-crop(pd2020,ghanamap)
pd2020ghana<-mask(pd2020ghana1,ghanamap)
plot(pd2020ghana)

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

#Choose a species #Pterocarpus erinaceus
pteeri<-ghana_species_pts_insidemap[ghana_species_pts_insidemap$species=='Pterocarpus erinaceus',]

#Using GhanaUses for maxent
levels(GhanaUses$Species)
levels(GhanaUses$Category)
healthcare<-(GhanaUses$Category=='Health care')
healthcare#Not a dataframe.

#Basic MaxEnt with two predictors
library(rJava)
dat=pteeri@data
m1<-maxent(ghana1[[c(4,16)]],pteeri)

m1<-maxent(ghana_envvars[[c(4,16,20,26)]],pteeri,factors=c("swa_2000lulc_2km"),args=c("-P"))#Selecting 2000 for pop and lc 
m1#View output as html
#Predict habitat suitability for the species
p1<-predict(m1,ghana1)

p1<-predict(m1,ghana_envvars)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Pterocarpus erinaceus',scales=list(draw=F),xlab=NULL,ylab=NULL)+
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
tuneparameters<-ENMevaluate(pteeri@coords,ghana1[[c(4,16)]],method="block",bg.coords=bg_BC)

tuneparameters<-ENMevaluate(pteeri@coords,ghana_envvars[[c(4,16,20,26)]],categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],pteeri,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Pterocarpus erinaceus',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(pteeri,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana_envvars[[c(4,16,20,26)]],pteeri,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=1.0','threshold=TRUE','product=TRUE',"-P","-J"),
                    path='ExampleMaxEnt')


