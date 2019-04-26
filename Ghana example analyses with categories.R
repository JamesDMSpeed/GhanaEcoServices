<<<<<<< HEAD
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
require(rgdal)
require(rgeos)
require(maptools)

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
class(ghanamap)
plot(ghanamap)
plot(dataSpatial, add=T)
crs(ghanamap)
points(ghana_species_pts,pch=16,cex=0.1,col='red') #Species occurence points

#Extract records outside of Ghana land (i.e. sea, other countries)
ghana_species_pts_insidemap<-ghana_species_pts[ghanamap,]
points(ghana_species_pts_insidemap,cex=0.1,pch=16,col='blue')
legend('r',pch=16,col=c('red','blue'),c('Discarded records','Retained records'))
title( main = "Species Occurrences in Ghana")

# Need a background raster for levelplot
r <- raster(ncol=500, nrow=500)
extent(r) <- extent(ghanamap)
ghanamap_ras<-rasterize(ghanamap, r)

# Levelplot for species points
myTheme <- BTCTheme()
my.padding <- list(myTheme, layout.heights = list(
  top.padding = 1, 
  main.key.padding = 1, 
  key.axis.padding = 1, 
  axis.xlab.padding = 1, 
  xlab.key.padding = 1, 
  key.sub.padding = 1), 
  layout.widths = list( 
    left.padding = 1, 
    key.ylab.padding = 1, 
    ylab.axis.padding = 1, 
    axis.key.padding = 1, 
    right.padding = 1) 
) 

myTheme <- BTCTheme()
myTheme$regions$col = c('white')
my.padding$regions$col = c('white')
GhanaSpp<-levelplot(ghanamap_ras,main='Species records',margin=F, colorkey=F,par.settings=myTheme) #scales = list(draw = FALSE)
GhanaSpp<-GhanaSpp+layer(sp.polygons(ghanamap))
GhanaSpp<-GhanaSpp+layer(sp.points(ghana_species_pts_insidemap, pch =3, cex =.25, fill="green",col="green"))
GhanaSpp

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
class(ghanaalt)
levelplot(ghanaalt,margin=F,main='Elevation (m)')+
  layer(sp.polygons(ghanamap))

#WorldClim data
#2.5minute resolution is ca.4.5km at equator
gc1<-getData('worldclim',var='bio',res=2.5)
#Crop and mask to elevation data
myTheme2 <- RdBuTheme()
my.padding2 <- list(myTheme2, layout.heights = list(
  top.padding = 1, 
  main.key.padding = 1, 
  key.axis.padding = 1, 
  axis.xlab.padding = 1, 
  xlab.key.padding = 1, 
  key.sub.padding = 1), 
  layout.widths = list( 
    left.padding = 1, 
    key.ylab.padding = 1, 
    ylab.axis.padding = 1, 
    axis.key.padding = 1, 
    right.padding = 1) 
) 

ghanaC<-crop(gc1,ghanamap)
ghana1<-mask(ghanaC,ghanamap)
GhanaRain<-levelplot(ghana1$bio12,margin=F,main='Annual precipitation (mm)',par.settings="RdBuTheme")
GhanaRain<-GhanaRain+layer(sp.polygons(ghanamap))
GhanaRain

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

ghanalc1975@data@attributes[[1]][,1:5]

#make a new raster, leaving out the attribute table for simplicity
ghanalc1975_simple <- setValues(raster(ghanalc1975),ghanalc1975[])

# 1 represents forest
ghanalc1975_simple[ghanalc1975_simple==25]<-1
ghanalc1975_simple[ghanalc1975_simple==15]<-1
ghanalc1975_simple[ghanalc1975_simple==21]<-1
ghanalc1975_simple[ghanalc1975_simple==23]<-1
ghanalc1975_simple[ghanalc1975_simple==28]<-1

#2 represents savanna
ghanalc1975_simple[ghanalc1975_simple==4]<-2
ghanalc1975_simple[ghanalc1975_simple==22]<-2
ghanalc1975_simple[ghanalc1975_simple==29]<-2
ghanalc1975_simple[ghanalc1975_simple==31]<-2
ghanalc1975_simple[ghanalc1975_simple==32]<-2

#3 represents wetlands
ghanalc1975_simple[ghanalc1975_simple==7]<-3
ghanalc1975_simple[ghanalc1975_simple==9]<-3

#4 represents agriculture
ghanalc1975_simple[ghanalc1975_simple==6]<-4
ghanalc1975_simple[ghanalc1975_simple==8]<-4
ghanalc1975_simple[ghanalc1975_simple==14]<-4
ghanalc1975_simple[ghanalc1975_simple==24]<-4
ghanalc1975_simple[ghanalc1975_simple==27]<-4

#5 represents landscape area
ghanalc1975_simple[ghanalc1975_simple==10]<-5
ghanalc1975_simple[ghanalc1975_simple==11]<-5
ghanalc1975_simple[ghanalc1975_simple==12]<-5
ghanalc1975_simple[ghanalc1975_simple==13]<-5
ghanalc1975_simple[ghanalc1975_simple==78]<-5

#6 represents clouds
ghanalc1975_simple[ghanalc1975_simple==99]<-6

#Checking summary of simplified habitat areas
ghanalc1975_simple@data@attributes
ghanalc1975_simple
summary(as.factor(ghanalc1975_simple))
summary(as.factor(getValues(ghanalc1975_simple)))
#NB legend is in RAT table
levels(ghanalc1975_simple)

#Do the same with other years too
#Using lc2000 for maxent modelling
lc2000<-raster('Landcover maps/west_africa_land-use_land-cover_2000_2km/swa_2000lulc_2km.tif')
utmprojGhana<-"+proj=utm +north +zone=30 +ellps=WGS84"
crs(lc2000) # This is not WGS84 formatted! # '+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs'
lc2000<-projectRaster(lc2000, crs = "+proj=longlat +datum=WGS84")
ghanat<-spTransform(ghanamap,crs(lc2000))
ghanalc2000<-mask(crop(lc2000,ghanat),ghanat)

#checking table that describes the different values associated with the values
ghanalc2000@data@attributes[[1]][,1:5]

#make a new raster, leaving out the attribute table for simplicity
ghanalc2000_simple <- setValues(raster(ghanalc2000),ghanalc2000[])

#Simplifying habitat areas
#1 represents forest
ghanalc2000_simple[ghanalc2000_simple==25]<-1
ghanalc2000_simple[ghanalc2000_simple==15]<-1
ghanalc2000_simple[ghanalc2000_simple==21]<-1
ghanalc2000_simple[ghanalc2000_simple==23]<-1
ghanalc2000_simple[ghanalc2000_simple==28]<-1

#2 represents savanna
ghanalc2000_simple[ghanalc2000_simple==4]<-2
ghanalc2000_simple[ghanalc2000_simple==22]<-2
ghanalc2000_simple[ghanalc2000_simple==29]<-2
ghanalc2000_simple[ghanalc2000_simple==31]<-2
ghanalc2000_simple[ghanalc2000_simple==32]<-2

#3 represents wetlands
ghanalc2000_simple[ghanalc2000_simple==7]<-3
ghanalc2000_simple[ghanalc2000_simple==9]<-3

#4 represents agriculture
ghanalc2000_simple[ghanalc2000_simple==6]<-4
ghanalc2000_simple[ghanalc2000_simple==8]<-4
ghanalc2000_simple[ghanalc2000_simple==14]<-4
ghanalc2000_simple[ghanalc2000_simple==24]<-4
ghanalc2000_simple[ghanalc2000_simple==27]<-4

#5 represents landscape area
ghanalc2000_simple[ghanalc2000_simple==10]<-5
ghanalc2000_simple[ghanalc2000_simple==11]<-5
ghanalc2000_simple[ghanalc2000_simple==12]<-5
ghanalc2000_simple[ghanalc2000_simple==13]<-5
ghanalc2000_simple[ghanalc2000_simple==78]<-5

#6 represents clouds
ghanalc2000_simple[ghanalc2000_simple==99]<-6

#Checking summary of simplified habitat areas
ghanalc2000_simple@data@attributes
ghanalc2000_simple # This is not WGS84 format
summary(as.factor(ghanalc2000_simple))
summary(as.factor(getValues(ghanalc2000_simple)))

myTheme3 <- BTCTheme()
my.padding3 <- list(myTheme3, layout.heights = list(
  top.padding = 1, 
  main.key.padding = 1, 
  key.axis.padding = 1, 
  axis.xlab.padding = 1, 
  xlab.key.padding = 1, 
  key.sub.padding = 1), 
  layout.widths = list( 
    left.padding = 1, 
    key.ylab.padding = 1, 
    ylab.axis.padding = 1, 
    axis.key.padding = 1, 
    right.padding = 1) 
) 

GhanaLand2000<-levelplot(ghanalc2000_simple,margin=F,main='Land cover 2000',par.settings="BTCTheme")
GhanaLand2000<-GhanaLand2000+layer(sp.polygons(ghanamap))
GhanaLand2000

plot(ghanalc2000_simple)
levels(ghanalc2000_simple)
title( main = "Land cover 2000")
legend('r',pch=16,col=c('1','2','3','4','5','6'),c('forest','savanna','wetlands','agriculture','landcape area','cloud'))
levels(ghanalc2000_simple)

lc2013<-raster('Landcover maps/west_africa_land-use_land-cover_2013_2km/swa_2013lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc2013))
ghanalc2013<-mask(crop(lc2013,ghanat),ghanat)
plot(ghanalc2013)
levels(ghanalc2013)
crs(ghanalc2013)<-"+proj=utm +north +zone=30 +ellps=WGS84"
ghanalc2013

# Project Raster
ghanalc2013 <- projectRaster(ghanalc2013, crs = "+proj=longlat +datum=WGS84")

#checking table that describes the different values associated with the values
ghanalc2013@data@attributes[[1]][,1:5]

#make a new raster, leaving out the attribute table for simplicity
ghanalc2013_simple <- setValues(raster(ghanalc2013),ghanalc2013[])

#Simplifying habitat areas
#1 represents forest
ghanalc2013_simple[ghanalc2013_simple==25]<-1
ghanalc2013_simple[ghanalc2013_simple==15]<-1
ghanalc2013_simple[ghanalc2013_simple==21]<-1
ghanalc2013_simple[ghanalc2013_simple==23]<-1
ghanalc2013_simple[ghanalc2013_simple==28]<-1

#2 represents savanna
ghanalc2013_simple[ghanalc2013_simple==4]<-2
ghanalc2013_simple[ghanalc2013_simple==22]<-2
ghanalc2013_simple[ghanalc2013_simple==29]<-2
ghanalc2013_simple[ghanalc2013_simple==31]<-2
ghanalc2013_simple[ghanalc2013_simple==32]<-2

#3 represents wetlands
ghanalc2013_simple[ghanalc2013_simple==7]<-3
ghanalc2013_simple[ghanalc2013_simple==9]<-3

#4 represents agriculture
ghanalc2013_simple[ghanalc2013_simple==6]<-4
ghanalc2013_simple[ghanalc2013_simple==8]<-4
ghanalc2013_simple[ghanalc2013_simple==14]<-4
ghanalc2013_simple[ghanalc2013_simple==24]<-4
ghanalc2013_simple[ghanalc2013_simple==27]<-4

#5 represents landscape area
ghanalc2013_simple[ghanalc2013_simple==10]<-5
ghanalc2013_simple[ghanalc2013_simple==11]<-5
ghanalc2013_simple[ghanalc2013_simple==12]<-5
ghanalc2013_simple[ghanalc2013_simple==13]<-5
ghanalc2013_simple[ghanalc2013_simple==78]<-5

#6 represents clouds
ghanalc2013_simple[ghanalc2013_simple==99]<-6

#Checking summary of simplified habitat areas
ghanalc2013_simple@data@attributes
ghanalc2013_simple
summary(as.factor(ghanalc2013_simple))
summary(as.factor(getValues(ghanalc2013_simple)))

GhanaLand2013<-levelplot(ghanalc2013_simple,margin=F,main='Land cover 2013',par.settings='BTCTheme')
GhanaLand2013<-GhanaLand2013+layer(sp.polygons(ghanamap))
GhanaLand2013

#Cropping Ghana from Global population density data
pd2000<-raster('GhanaPopData.grd')
pd2000ghana1<-crop(pd2000,ghanamap)
pd2000ghana<-mask(pd2000ghana1,ghanamap)
str(pd2000ghana)
pd2000ghana@data@values<-log10(pd2000ghana@data@values)

myTheme4 <- YlOrRdTheme()
my.padding4 <- list(myTheme4, layout.heights = list(
  top.padding = 1, 
  main.key.padding = 1, 
  key.axis.padding = 1, 
  axis.xlab.padding = 1, 
  xlab.key.padding = 1, 
  key.sub.padding = 1), 
  layout.widths = list( 
    left.padding = 1, 
    key.ylab.padding = 1, 
    ylab.axis.padding = 1, 
    axis.key.padding = 1, 
    right.padding = 1) 
) 

pd2000ghana2<-levelplot(pd2000ghana,margin=F,main='Log Population density',par.settings="YlOrRdTheme")
pd2000ghana2<-pd2000ghana2+layer(sp.polygons(ghanamap))
pd2000ghana2

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

#Using population data 2000 for maxent modelling
#Stack up popdata
popdat<-stack(pd2000ghana,pd2005ghana,pd2010ghana,pd2015ghana,pd2020ghana)

#Resample to same grid as climate data
popdatrs<-resample(popdat,ghana1,method="bilinear")
writeRaster(popdatrs,'GhanaPopData')
ghanapopdat<-stack('GhanaPopData')

#### Combining levelplots in a single figure ####

# Combine: GhanaSpp, GhanaRain, GhanaLand2013
# Packages
library(grid)
require(gridExtra)

#### Ghana maps ####
GhanaMAPS <- paste0("Ghana.maps", "_",Sys.Date(), ".jpeg" )
jpeg (GhanaMAPS, width=28, height=10, res=400, unit="cm")
grid.arrange(GhanaRain, GhanaLand2000, pd2000ghana2, ncol=3, nrow=1, widths=c(1.5,1.5,1.5), heights=c(2),layout_matrix = cbind(c(1), c(2),c(3)))
dev.off()
#vp = grid::viewport(width=1.5,height=2)


#Stack up lcdata
lcdat<-stack(ghanalc1975, ghanalc2000, ghanalc2013, ghanalc1975_simple,ghanalc2000_simple,ghanalc2013_simple)

#Check the names
names(lcdat)
names(lcdat)[4:6]<-c("simplelc1975","simplelc2000","simplelc2013")

#Project to same crs as climate data
lcdatp<-projectRaster(lcdat,ghana1,method="ngb")

#Resample to same grid as climate data
lcdatrs<-resample(lcdatp,ghana1,method="ngb")#Nearest neighbour as categorical

ghana_envvars<-stack(ghana1,mask(ghanapopdat,ghana1[[1]]),mask(lcdatrs,ghana1[[1]]))
names(ghana_envvars)

#Merging health care with gbif recs to a data frame 
healthcare_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Health care']),]
dim(healthcare_gbif)
levels(droplevels(healthcare_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"]))))
ecosyshcsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"])))
gbifhcsp<-levels(droplevels(healthcare_gbif$species)) 
ecosyshcsp[which(ecosyshcsp%in%gbifhcsp==F)]

#Health care convex hull area
#First convert to utm to work in m
healthcare_gbif_utm<-project(healthcare_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
health_hull<-chull(healthcare_gbif_utm)
health_hul1 <- c(health_hull, health_hull[1])
health_hull.coords <- healthcare_gbif_utm[health_hull,]
health_hull_poly<-Polygon(health_hull.coords)
health_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Health care
library(rJava)
dat=healthcare_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,factors=c("ghana"),args=c("-P"))#Selecting 2000 for pop and lc 
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
tuneparameters_healthcare<-ENMevaluate(healthcare_gbif@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_healthcare@results
tuneparameters_healthcare@results[which.min(tuneparameters_healthcare@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_healthcare,file = "tuneparameters_Health care")
tp_healthcare<-readRDS("tuneparameters_Health care")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Health care',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(healthcare_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_healthcare<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=0.5',
                                 'linear=TRUE',
                                 'quadratic=TRUE',
                                 'hinge=FALSE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Health care')

#Maxent with simple landcover
maxent_healthcare_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],healthcare_gbif,a=bg_BC,
                                          
                                          factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                          
                                          args=c('betamultiplier=0.5',
                                                 
                                                 'linear=TRUE',
                                                 
                                                 'quadratic=TRUE',
                                                 
                                                 'hinge=FALSE',
                                                 
                                                 'threshold=FALSE',
                                                 
                                                 'product=FALSE',
                                                 
                                                 "-P","-J","replicates=5"),
                                          
                                          path='MaxEntOutput/Healthcare_simplelandcover')


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


#Agriculture convex hull area
#First convert to utm to work in m
agricult_gbif_utm<-project(agricult_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
agricult_hull<-chull(agricult_gbif_utm)
agricult_hul1 <- c(agricult_hull, agricult_hull[1])
agricult_hull.coords <- agricult_gbif_utm[agricult_hull,]
agricult_hull_poly<-Polygon(agricult_hull.coords)
agricult_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Agriculture
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_agriculture<-ENMevaluate(agricult_gbif@coords,
                                        ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_agriculture@results
tuneparameters_agriculture@results[which.min(tuneparameters_agriculture@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_agriculture,file = "tuneparameters_Agriculture")
tp_agriculture<-readRDS("tuneparameters_Agriculture")



#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],agricult_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Agriculture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(agricult_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_agriculture<-maxent(ghana_envvars[[c(4,16,20,26)]],agricult_gbif,a=bg_BC,
                           factors="swa_2000lulc_2km",
                           args=c('betamultiplier=0.5',
                                  'linear=TRUE',
                                  'quadratic=TRUE',
                                  'hinge=FALSE',
                                  'threshold=FALSE',
                                  'product=FALSE',
                                  "-P","-J","replicates=5"),
                           path='MaxEntOutput/Agriculture')

#Maxent with simple landcover
maxent_agriculture_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],agricult_gbif,a=bg_BC,
                                           
                                           factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                           
                                           args=c('betamultiplier=0.5',
                                                  
                                                  'linear=TRUE',
                                                  
                                                  'quadratic=TRUE',
                                                  
                                                  'hinge=FALSE',
                                                  
                                                  'threshold=FALSE',
                                                  
                                                  'product=FALSE',
                                                  
                                                  "-P","-J","replicates=5"),
                                           
                                           path='MaxEntOutput/Agriculture_simplelandcover')

#Merging water purification with gbif recs to a data frame 
purif_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Water purification']),]
dim(purif_gbif)
levels(droplevels(purif_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purification"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purificaion"]))))
ecosyspursp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purification"])))
gbifpursp<-levels(droplevels(purif_gbif$species)) 
ecosyspursp[which(ecosyspursp%in%gbifpursp==F)]


#Water purification convex hull area
#First convert to utm to work in m
purif_gbif_utm<-project(purif_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
purif_hull<-chull(purif_gbif_utm)
purif_hul1 <- c(purif_hull, purif_hull[1])
purif_hull.coords <- purif_gbif_utm[purif_hull,]
purif_hull_poly<-Polygon(purif_hull.coords)
purif_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Water purifiction
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_purification<-ENMevaluate(purif_gbif@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_purification@results
tuneparameters_purification@results[which.min(tuneparameters_purification@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_purification,file = "tuneparameters_Water purification")
tp_purification<-readRDS("tuneparameters_Water purification")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],purif_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Water purification',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(purif_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_purification<-maxent(ghana_envvars[[c(4,16,20,26)]],purif_gbif,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=4.0',
                                   'linear=TRUE',
                                   'quadratic=TRUE',
                                   'hinge=TRUE',
                                   'threshold=FALSE',
                                   'product=TRUE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Water purification')

#Maxent with simple landcover
maxent_purification_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],purif_gbif,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=4.0',
                                                   
                                                   'linear=TRUE',
                                                   
                                                   'quadratic=TRUE',
                                                   
                                                   'hinge=TRUE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=TRUE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Water purification_simplelandcover')


#Merging construction with gbif recs to a data frame
construct_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Construction']),]
dim(construct_gbif)
levels(droplevels(construct_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"]))))
ecosysconssp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"])))
gbifconssp<-levels(droplevels(construct_gbif$species)) 
ecosysconssp[which(ecosysconssp%in%gbifconssp==F)]


#Construction convex hull area
#First convert to utm to work in m
construct_gbif_utm<-project(construct_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
construct_hull<-chull(construct_gbif_utm)
construct_hul1 <- c(construct_hull, construct_hull[1])
construct_hull.coords <- construct_gbif_utm[construct_hull,]
construct_hull_poly<-Polygon(construct_hull.coords)
construct_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Construction
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_construction<-ENMevaluate(construct_gbif@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_construction@results
tuneparameters_construction@results[which.min(tuneparameters_construction@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_construction,file = "tuneparameters_Construction")
tp_construction<-readRDS("tuneparameters_Construction")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],construct_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Construction',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(construct_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_construction<-maxent(ghana_envvars[[c(4,16,20,26)]],construct_gbif,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=0.5',
                                   'linear=TRUE',
                                   'quadratic=TRUE',
                                   'hinge=FALSE',
                                   'threshold=FALSE',
                                   'product=FALSE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Construction')

#Maxent with simple landcover
maxent_construction_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],construct_gbif,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=0.5',
                                                   
                                                   'linear=TRUE',
                                                   
                                                   'quadratic=TRUE',
                                                   
                                                   'hinge=FALSE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Construction_simplelandcover')


#Merging social with gbif recs to a data frame
social_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Social']),]
dim(social_gbif)
levels(droplevels(social_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"]))))
ecosyssocsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"])))
gbifsocsp<-levels(droplevels(social_gbif$species)) 
ecosyssocsp[which(ecosyssocsp%in%gbifsocsp==F)]


#Social convex hull area
#First convert to utm to work in m
social_gbif_utm<-project(social_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
social_hull<-chull(social_gbif_utm)
social_hul1 <- c(social_hull, social_hull[1])
social_hull.coords <- social_gbif_utm[social_hull,]
social_hull_poly<-Polygon(social_hull.coords)
social_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for social
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_social<-ENMevaluate(social_gbif@coords,
                                   ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_social@results
tuneparameters_social@results[which.min(tuneparameters_social@results$AICc),]#Can use other parameters to select best methodsaveRDS(tuneparameters_social,file = "tuneparameters_Social")
tp_social<-readRDS("tuneparameters_Social")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],social_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Social',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(social_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_social<-maxent(ghana_envvars[[c(4,16,20,26)]],social_gbif,a=bg_BC,
                      factors="swa_2000lulc_2km",
                      args=c('betamultiplier=4.0',
                             'linear=TRUE',
                             'quadratic=TRUE',
                             'hinge=TRUE',
                             'threshold=FALSE',
                             'product=TRUE',
                             "-P","-J","replicates=5"),
                      path='MaxEntOutput/Social')

#Maxent with simple landcover
maxent_social_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],social_gbif,a=bg_BC,
                                      
                                      factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                      
                                      args=c('betamultiplier=4.0',
                                             
                                             'linear=TRUE',
                                             
                                             'quadratic=TRUE',
                                             
                                             'hinge=TRUE',
                                             
                                             'threshold=FALSE',
                                             
                                             'product=TRUE',
                                             
                                             "-P","-J","replicates=5"),
                                      
                                      path='MaxEntOutput/Social_simplelandcover')


#Merging energy with gbif recs to a data frame 
energy_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Energy']),]
dim(energy_gbif)
levels(droplevels(energy_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"]))))
ecosysensp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"])))
gbifensp<-levels(droplevels(energy_gbif$species)) 
ecosysensp[which(ecosysensp%in%gbifensp==F)]


#Energy convex hull area
#First convert to utm to work in m
energy_gbif_utm<-project(energy_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
energy_hull<-chull(energy_gbif_utm)
energy_hul1 <- c(energy_hull, energy_hull[1])
energy_hull.coords <- energy_gbif_utm[energy_hull,]
energy_hull_poly<-Polygon(energy_hull.coords)
energy_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for energy
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_energy<-ENMevaluate(energy_gbif@coords,
                                   ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_energy@results
tuneparameters_energy@results[which.min(tuneparameters_energy@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_energy,file = "tuneparameters_Energy")
tp_energy<-readRDS("tuneparameters_Energy")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],energy_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Energy',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(energy_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_energy<-maxent(ghana_envvars[[c(4,16,20,26)]],energy_gbif,a=bg_BC,
                      factors="swa_2000lulc_2km",
                      args=c('betamultiplier=4.0',
                             'linear=FALSE',
                             'quadratic=FALSE',
                             'hinge=TRUE',
                             'threshold=FALSE',
                             'product=FALSE',
                             "-P","-J","replicates=5"),
                      path='MaxEntOutput/Energy')

#Maxent with simple landcover
maxent_energy_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],energy_gbif,a=bg_BC,
                                      
                                      factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                      
                                      args=c('betamultiplier=4.0',
                                             
                                             'linear=FALSE',
                                             
                                             'quadratic=FALSE',
                                             
                                             'hinge=TRUE',
                                             
                                             'threshold=FALSE',
                                             
                                             'product=FALSE',
                                             
                                             "-P","-J","replicates=5"),
                                      
                                      path='MaxEntOutput/Energy_simplelandcover')

#Merging food and nutrition with gbif recs to a data frame
foodnutr_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Food and nutrition']),]
dim(foodnutr_gbif)
levels(droplevels(foodnutr_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"]))))
ecosysfnsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"])))
gbiffnsp<-levels(droplevels(foodnutr_gbif$species)) 
ecosysfnsp[which(ecosysfnsp%in%gbiffnsp==F)]

#Food and nutrition convex hull area
#First convert to utm to work in m
foodnutr_gbif_utm<-project(foodnutr_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
foodnutr_hull<-chull(foodnutr_gbif_utm)
foodnutr_hul1 <- c(foodnutr_hull, foodnutr_hull[1])
foodnutr_hull.coords <- foodnutr_gbif_utm[foodnutr_hull,]
foodnutr_hull_poly<-Polygon(foodnutr_hull.coords)
foodnutr_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Food and nutrition
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_foodnutrition<-ENMevaluate(foodnutr_gbif@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_foodnutrition@results
tuneparameters_foodnutrition@results[which.min(tuneparameters_foodnutrition@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_foodnutrition,file = "tuneparameters_Food and nutrition")
tp_healthcare<-readRDS("tuneparameters_Food and nutrition")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],foodnutr_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Food and nutrition',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(foodnutr_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_foodnutrition<-maxent(ghana_envvars[[c(4,16,20,26)]],foodnutr_gbif,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=0.5',
                                    'linear=TRUE',
                                    'quadratic=TRUE',
                                    'hinge=FALSE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Food and nutrition')

#Maxent with simple landcover
maxent_foodnutrition_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],foodnutr_gbif,a=bg_BC,
                                             
                                             factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                             
                                             args=c('betamultiplier=0.5',
                                                    
                                                    'linear=TRUE',
                                                    
                                                    'quadratic=TRUE',
                                                    
                                                    'hinge=FALSE',
                                                    
                                                    'threshold=FALSE',
                                                    
                                                    'product=FALSE',
                                                    
                                                    "-P","-J","replicates=5"),
                                             
                                             path='MaxEntOutput/Food and nutrition_simplelandcover')


#Merging Culture with gbif recs to a data frame 
cultur_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Culture']),]
dim(cultur_gbif)
levels(droplevels(cultur_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"]))))
ecosysculsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"])))
gbifculsp<-levels(droplevels(cultur_gbif$species)) 
ecosysculsp[which(ecosysculsp%in%gbifculsp==F)]

#Culture convex hull area
#First convert to utm to work in m
cultur_gbif_utm<-project(cultur_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
cultur_hull<-chull(cultur_gbif_utm)
cultur_hul1 <- c(cultur_hull, cultur_hull[1])
cultur_hull.coords <- cultur_gbif_utm[cultur_hull,]
cultur_hull_poly<-Polygon(cultur_hull.coords)
cultur_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for culture
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_culture<-ENMevaluate(cultur_gbif@coords,
                                    ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)


tuneparameters_culture@results
tuneparameters_culture@results[which.min(tuneparameters_culture@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_culture,file = "tuneparameters_Culture")
tp_culture<-readRDS("tuneparameters_Culture")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],cultur_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.5','threshold=FALSE','product=FALSE',"-P","-J"))


maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Culture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(cultur_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_culture<-maxent(ghana_envvars[[c(4,16,20,26)]],cultur_gbif,a=bg_BC,
                       factors="swa_2000lulc_2km",
                       args=c('betamultiplier=3.5',
                              'linear=FALSE',
                              'quadratic=FALSE',
                              'hinge=TRUE',
                              'threshold=FALSE',
                              'product=FALSE',
                              "-P","-J","replicates=5"),
                       path='MaxEntOutput/Culture')

#Maxent with simple landcover
maxent_culture_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],cultur_gbif,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=3.5',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Culture_simplelandcover')

#Category count analysis
speciesdata<-read.csv('FinalDataSpeciesNames.csv')

#Count species in each group within health care
with(droplevels(GhanaUses[GhanaUses$Category=='Health care',]),tapply(ConfirmedSppNames,Group,length))

#Count species within Anaesthetics of health care 
with(droplevels(speciesdata[speciesdata$Group=='Medicine: Anaesthetics',]),tapply(ConfirmedSppNames,Category,length))

#MaxEnt modelling of groups within health care category
#Merging Anaesthetics species with Gbif records
anaesth<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Anaesthetics']),]
dim(anaesth)

levels(droplevels(anaesth$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Anaesthetics"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Anaesthetics"]))))
ecosysansp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Anaesthetics"])))
gbifansp<-levels(droplevels(anaesth$species)) 
ecosysansp[which(ecosysansp%in%gbifansp==F)]


#Anaesthetics convex hull area
#First convert to utm to work in m
anaesth_utm<-project(anaesth@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
anaesth_hull<-chull(anaesth_utm)
anaesth_hul1 <- c(anaesth_hull, anaesth_hull[1])
anaesth_hull.coords <- anaesth_utm[anaesth_hull,]
anaesth_hull_poly<-Polygon(anaesth_hull.coords)
anaesth_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Anaesthetics
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_anaesthetics<-ENMevaluate(anaesth@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_anaesthetics@results
tuneparameters_anaesthetics@results[which.min(tuneparameters_anaesthetics@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_anaesthetics,file = "tuneparameters_Anaesthetics")
tp_anaesthetics<-readRDS("tuneparameters_Anaesthetics")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],anaesth,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Anaesthetics',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(anaesth,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_anaesthetics<-maxent(ghana_envvars[[c(4,16,20,26)]],anaesth,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=0.5',
                                   'linear=TRUE',
                                   'quadratic=TRUE',
                                   'hinge=FALSE',
                                   'threshold=FALSE',
                                   'product=FALSE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Anaesthetics')


#Maxent with simple landcover
maxent_anaesthetics_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],anaesth,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=0.5',
                                                   
                                                   'linear=TRUE',
                                                   
                                                   'quadratic=TRUE',
                                                   
                                                   'hinge=FALSE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Anaesthetics_simplelandcover')


#Do same with other groups
#Merging Dentistry species with Gbif records
dent<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Dentistry']),]
dim(dent)

levels(droplevels(dent$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dentistry"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dentistry"]))))
ecosysdentsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dentistry"])))
gbifdentsp<-levels(droplevels(dent$species)) 
ecosysdentsp[which(ecosysdentsp%in%gbifdentsp==F)]

#Dentistry convex hull area
#First convert to utm to work in m
dent_utm<-project(dent@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
dent_hull<-chull(dent_utm)
dent_hul1 <- c(dent_hull, dent_hull[1])
dent_hull.coords <- dent_utm[dent_hull,]
dent_hull_poly<-Polygon(dent_hull.coords)
dent_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Dentistry
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_dentistry<-ENMevaluate(dent@coords,
                                      ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_dentistry@results
tuneparameters_dentistry@results[which.min(tuneparameters_dentistry@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_dentistry,file = "tuneparameters_Dentistry")
tp_dentistry<-readRDS("tuneparameters_Dentistry")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],dent,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Dentistry',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(dent,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_dentistry<-maxent(ghana_envvars[[c(4,16,20,26)]],dent,a=bg_BC,
                         factors="swa_2000lulc_2km",
                         args=c('betamultiplier=3.5',
                                'linear=FALSE',
                                'quadratic=FALSE',
                                'hinge=TRUE',
                                'threshold=FALSE',
                                'product=FALSE',
                                "-P","-J","replicates=5"),
                         path='MaxEntOutput/Dentistry')


#Maxent with simple landcover
maxent_dentistry_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],dent,a=bg_BC,
                                         
                                         factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                         
                                         args=c('betamultiplier=3.5',
                                                
                                                'linear=FALSE',
                                                
                                                'quadratic=FALSE',
                                                
                                                'hinge=TRUE',
                                                
                                                'threshold=FALSE',
                                                
                                                'product=FALSE',
                                                
                                                "-P","-J","replicates=5"),
                                         
                                         path='MaxEntOutput/Dentistry_simplelandcover')

#Merging Dermatology species with Gbif records
derm<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Dermatology']),]
dim(derm)

levels(droplevels(derm$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dermatology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dermatology"]))))
ecosysdermsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dermatology"])))
gbifdermsp<-levels(droplevels(derm$species)) 
ecosysdermsp[which(ecosysdermsp%in%gbifdermsp==F)]

#Dermatology convex hull area
#First convert to utm to work in m
derm_utm<-project(derm@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
derm_hull<-chull(derm_utm)
derm_hul1 <- c(derm_hull, derm_hull[1])
derm_hull.coords <- derm_utm[derm_hull,]
derm_hull_poly<-Polygon(derm_hull.coords)
derm_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Dermatology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_dermatology<-ENMevaluate(derm@coords,
                                        ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_dermatology@results
tuneparameters_dermatology@results[which.min(tuneparameters_dermatology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_dermatology,file = "tuneparameters_Dermatology")
tp_dermatology<-readRDS("tuneparameters_Dermatology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],derm,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Dermatology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(derm,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_dermatology<-maxent(ghana_envvars[[c(4,16,20,26)]],derm,a=bg_BC,
                           factors="swa_2000lulc_2km",
                           args=c('betamultiplier=0.5',
                                  'linear=TRUE',
                                  'quadratic=TRUE',
                                  'hinge=FALSE',
                                  'threshold=FALSE',
                                  'product=FALSE',
                                  "-P","-J","replicates=5"),
                           path='MaxEntOutput/Dermatology')

#Maxent with simple landcover
maxent_dermatology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],derm,a=bg_BC,
                                           
                                           factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                           
                                           args=c('betamultiplier=0.5',
                                                  
                                                  'linear=TRUE',
                                                  
                                                  'quadratic=TRUE',
                                                  
                                                  'hinge=FALSE',
                                                  
                                                  'threshold=FALSE',
                                                  
                                                  'product=FALSE',
                                                  
                                                  "-P","-J","replicates=5"),
                                           
                                           path='MaxEntOutput/Dermatology_simplelandcover')

#Merging Endocrinology species with Gbif records
endo<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Endocrinology']),]
dim(endo)

levels(droplevels(endo$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Endocrinology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Endocrinology"]))))
ecosysendosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Endocrinology"])))
gbifendosp<-levels(droplevels(endo$species)) 
ecosysendosp[which(ecosysendosp%in%gbifendosp==F)]

#Endocrinology convex hull area
#First convert to utm to work in m
endo_utm<-project(endo@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
endo_hull<-chull(endo_utm)
endo_hul1 <- c(endo_hull, endo_hull[1])
endo_hull.coords <- endo_utm[endo_hull,]
endo_hull_poly<-Polygon(endo_hull.coords)
endo_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Endocrinology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_endocrinology<-ENMevaluate(endo@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_endocrinology@results
tuneparameters_endocrinology@results[which.min(tuneparameters_endocrinology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_endocrinology,file = "tuneparameters_Endocrinology")
tp_endocrinology<-readRDS("tuneparameters_Endocrinology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],endo,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Endocrinology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(endo,col=1))

#Write maxent results to file which you can later read in to make response curves etc in R
maxent_endocrinology<-maxent(ghana_envvars[[c(4,16,20,26)]],endo,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=0.5',
                                    'linear=TRUE',
                                    'quadratic=TRUE',
                                    'hinge=FALSE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Endocrinology')

#Maxent with simple landcover
maxent_endocrinology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],endo,a=bg_BC,
                                             
                                             factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                             
                                             args=c('betamultiplier=0.5',
                                                    
                                                    'linear=TRUE',
                                                    
                                                    'quadratic=TRUE',
                                                    
                                                    'hinge=FALSE',
                                                    
                                                    'threshold=FALSE',
                                                    
                                                    'product=FALSE',
                                                    
                                                    "-P","-J","replicates=5"),
                                             
                                             path='MaxEntOutput/Endocrinology_simplelandcover')


#Merging Excipients species with Gbif records
exci<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Excipients']),]
dim(exci)

levels(droplevels(exci$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Excipients"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Excipients"]))))
ecosysexcisp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Excipients"])))
gbifexcisp<-levels(droplevels(exci$species)) 
ecosysexcisp[which(ecosysexcisp%in%gbifexcisp==F)]


#Excipients convex hull area
#First convert to utm to work in m
exci_utm<-project(exci@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
exci_hull<-chull(exci_utm)
exci_hul1 <- c(exci_hull, exci_hull[1])
exci_hull.coords <- exci_utm[exci_hull,]
exci_hull_poly<-Polygon(exci_hull.coords)
exci_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Excipients
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_excipients<-ENMevaluate(exci@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_excipients@results
tuneparameters_excipients@results[which.min(tuneparameters_excipients@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_excipients,file = "tuneparameters_Excipients")
tp_excipients<-readRDS("tuneparameters_Excipients")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],exci,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.0','threshold=FALSE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Excipients',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(exci,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_excipients<-maxent(ghana_envvars[[c(4,16,20,26)]],exci,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=3.0',
                                 'linear=TRUE',
                                 'quadratic=TRUE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=TRUE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Excipients')

#Maxent with simple landcover
maxent_excipients_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],exci,a=bg_BC,
                                          
                                          factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                          
                                          args=c('betamultiplier=3.0',
                                                 
                                                 'linear=TRUE',
                                                 
                                                 'quadratic=TRUE',
                                                 
                                                 'hinge=TRUE',
                                                 
                                                 'threshold=FALSE',
                                                 
                                                 'product=TRUE',
                                                 
                                                 "-P","-J","replicates=5"),
                                          
                                          path='MaxEntOutput/Excipients_simplelandcover')


#Merging Fever species with Gbif records
fev<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Fever']),]
dim(fev)

levels(droplevels(fev$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Fever"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Fever"]))))
ecosysfevsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Fever"])))
gbiffevsp<-levels(droplevels(fev$species)) 
ecosysfevsp[which(ecosysfevsp%in%gbiffevsp==F)]

#Fever convex hull area
#First convert to utm to work in m
fev_utm<-project(fev@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
fev_hull<-chull(fev_utm)
fev_hul1 <- c(fev_hull, fev_hull[1])
fev_hull.coords <- fev_utm[fev_hull,]
fev_hull_poly<-Polygon(fev_hull.coords)
fev_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Fever
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_fever<-ENMevaluate(fev@coords,
                                  ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_fever@results                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
tuneparameters_fever@results[which.min(tuneparameters_fever@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_fever,file = "tuneparameters_Fever")
tp_fever<-readRDS("tuneparameters_Fever")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],fev,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Fever',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(fev,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_fever<-maxent(ghana_envvars[[c(4,16,20,26)]],fev,a=bg_BC,
                     factors="swa_2000lulc_2km",
                     args=c('betamultiplier=4.0',
                            'linear=TRUE',
                            'quadratic=TRUE',
                            'hinge=FALSE',
                            'threshold=FALSE',
                            'product=FALSE',
                            "-P","-J","replicates=5"),
                     path='MaxEntOutput/Fever')
#Maxent with simple landcover
maxent_fever_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],fev,a=bg_BC,
                                     
                                     factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                     
                                     args=c('betamultiplier=4.0',
                                            
                                            'linear=TRUE',
                                            
                                            'quadratic=TRUE',
                                            
                                            'hinge=FALSE',
                                            
                                            'threshold=FALSE',
                                            
                                            'product=FALSE',
                                            
                                            "-P","-J","replicates=5"),
                                     
                                     path='MaxEntOutput/Fever_simplelandcover')

#Merging Immunology species with Gbif records
immu<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Immunology']),]
dim(immu)

levels(droplevels(immu$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Immunology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Immunology"]))))
ecosysimmusp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Immunology"])))
gbifimmusp<-levels(droplevels(immu$species)) 
ecosysimmusp[which(ecosysimmusp%in%gbifimmusp==F)]

#Immunology convex hull area
#First convert to utm to work in m
immu_utm<-project(immu@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
immu_hull<-chull(immu_utm)
immu_hul1 <- c(immu_hull, immu_hull[1])
immu_hull.coords <- immu_utm[immu_hull,]
immu_hull_poly<-Polygon(immu_hull.coords)
immu_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Immunology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_immunology<-ENMevaluate(immu@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_immunology@results
tuneparameters_immunology@results[which.min(tuneparameters_immunology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_immunology,file = "tuneparameters_Immunology")
tp_immunology<-readRDS("tuneparameters_immunology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],immu,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Immunology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(immu,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_immunology<-maxent(ghana_envvars[[c(4,16,20,26)]],immu,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=3.5',
                                 'linear=FALSE',
                                 'quadratic=FALSE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Immunology')

#Maxent with simple landcover
maxent_immunology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],immu,a=bg_BC,
                                          
                                          factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                          
                                          args=c('betamultiplier=3.5',
                                                 
                                                 'linear=FALSE',
                                                 
                                                 'quadratic=FALSE',
                                                 
                                                 'hinge=TRUE',
                                                 
                                                 'threshold=FALSE',
                                                 
                                                 'product=FALSE',
                                                 
                                                 "-P","-J","replicates=5"),
                                          
                                          path='MaxEntOutput/Immunology_simplelandcover')


#Merging Infertility species with Gbif records
infer<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Infertility']),]
dim(infer)

levels(droplevels(infer$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Infertility"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Infertility"]))))
ecosysinfersp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Infertility"])))
gbifinfersp<-levels(droplevels(infer$species)) 
ecosysinfersp[which(ecosysinfersp%in%gbifinfersp==F)]

#Infertility convex hull area
#First convert to utm to work in m
infer_utm<-project(infer@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
infer_hull<-chull(infer_utm)
infer_hul1 <- c(infer_hull, infer_hull[1])
infer_hull.coords <- infer_utm[infer_hull,]
infer_hull_poly<-Polygon(infer_hull.coords)
infer_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Infertility
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_infertility<-ENMevaluate(infer@coords,
                                        ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_infertility@results
tuneparameters_infertility@results[which.min(tuneparameters_infertility@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_infertility,file = "tuneparameters_Infertility")
tp_infertility<-readRDS("tuneparameters_Infertility")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],infer,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))


maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Infertility',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(infer,col=1))

#Write maxent results to file which you can later read in to make response curves etc in R
#Example with switches for LQHPT
maxent_infertility<-maxent(ghana_envvars[[c(4,16,20,26)]],infer,a=bg_BC,
                           factors="swa_2000lulc_2km",
                           args=c('betamultiplier=4.0',
                                  'linear=FALSE',
                                  'quadratic=FALSE',
                                  'hinge=TRUE',
                                  'threshold=FALSE',
                                  'product=FALSE',
                                  "-P","-J","replicates=5"),
                           path='MaxEntOutput/Infertility')

#Maxent with simple landcover
maxent_infertility_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],infer,a=bg_BC,
                                           
                                           factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                           
                                           args=c('betamultiplier=4.0',
                                                  
                                                  'linear=FALSE',
                                                  
                                                  'quadratic=FALSE',
                                                  
                                                  'hinge=TRUE',
                                                  
                                                  'threshold=FALSE',
                                                  
                                                  'product=FALSE',
                                                  
                                                  "-P","-J","replicates=5"),
                                           
                                           path='MaxEntOutput/Infertility_simplelandcover')

#Merging Malaria species with Gbif records
mal<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Malaria']),]
dim(mal)

levels(droplevels(mal$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Malaria"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Malaria"]))))
ecosysmalsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Malaria"])))
gbifmalsp<-levels(droplevels(mal$species)) 
ecosysmalsp[which(ecosysmalsp%in%gbifmalsp==F)]

#Malaria convex hull area
#First convert to utm to work in m
mal_utm<-project(mal@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
mal_hull<-chull(mal_utm)
mal_hul1 <- c(mal_hull, mal_hull[1])
mal_hull.coords <- mal_utm[mal_hull,]
mal_hull_poly<-Polygon(mal_hull.coords)
mal_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for Malaria
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_malaria<-ENMevaluate(mal@coords,
                                    ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_malaria@results
tuneparameters_malaria@results[which.min(tuneparameters_malaria@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_malaria,file = "tuneparameters_Malaria")
tp_malaria<-readRDS("tuneparameters_Malaria")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],mal,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Malaria',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(mal,col=1))



#Write maxent results to file which you can later read in to make response curves etc in R
#Example with switches for LQHPT
maxent_malaria<-maxent(ghana_envvars[[c(4,16,20,26)]],mal,a=bg_BC,
                       factors="swa_2000lulc_2km",
                       args=c('betamultiplier=2.5',
                              'linear=TRUE',
                              'quadratic=TRUE',
                              'hinge=FALSE',
                              'threshold=FALSE',
                              'product=FALSE',
                              "-P","-J","replicates=5"),
                       path='MaxEntOutput/Malaria')
#Maxent with simple landcover
maxent_malaria_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],mal,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=2.5',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=FALSE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Malaria_simplelandcover')



#Merging Musculoskeletal and cardiology species with Gbif records
muscar<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Musculoskeletal and cardiology']),]
dim(muscar)

levels(droplevels(muscar$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Musculoskeletal and cardiology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Musculoskeletal and cardiology"]))))
ecosysmuscarsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Musculoskeletal and cardiology"])))
gbifmuscarsp<-levels(droplevels(muscar$species)) 
ecosysmuscarsp[which(ecosysmuscarsp%in%gbifmuscarsp==F)]

#Musculoskeletal and cardiology convex hull area
#First convert to utm to work in m
muscar_utm<-project(muscar@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
muscar_hull<-chull(muscar_utm)
muscar_hul1 <- c(muscar_hull, muscar_hull[1])
muscar_hull.coords <- muscar_utm[muscar_hull,]
muscar_hull_poly<-Polygon(muscar_hull.coords)
muscar_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Musculoskeletal and cardiology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_muscardiology<-ENMevaluate(muscar@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_muscardiology@results
tuneparameters_muscardiology@results[which.min(tuneparameters_muscardiology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_muscardiology,file = "tuneparameters_Musculoskeletal and cardiology")
tp_muscardiology<-readRDS("tuneparameters_Musculoskeletal and cardiology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],muscar,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Musculoskeletal and cardiology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(muscar,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
#Example with switches for LQHPT
maxent_muscardiology<-maxent(ghana_envvars[[c(4,16,20,26)]],muscar,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=2.0',
                                    'linear=TRUE',
                                    'quadratic=TRUE',
                                    'hinge=TRUE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Musculoskeletal and cardiology')
#Maxent with simple landcover
maxent_muscardiology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],muscar,a=bg_BC,
                                             
                                             factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                             
                                             args=c('betamultiplier=2.0',
                                                    
                                                    'linear=TRUE',
                                                    
                                                    'quadratic=TRUE',
                                                    
                                                    'hinge=TRUE',
                                                    
                                                    'threshold=FALSE',
                                                    
                                                    'product=FALSE',
                                                    
                                                    "-P","-J","replicates=5"),
                                             
                                             path='MaxEntOutput/Musculoskeletal and cardiology_simplelandcover')



#Merging Neurology species with Gbif records
neuro<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Neurology']),]
dim(neuro)

levels(droplevels(neuro$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Neurology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Neurology"]))))
ecosysneurosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Neurology"])))
gbifneurosp<-levels(droplevels(neuro$species)) 
ecosysneurosp[which(ecosysneurosp%in%gbifneurosp==F)]

#Neurology convex hull area
#First convert to utm to work in m
neuro_utm<-project(neuro@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
neuro_hull<-chull(neuro_utm)
neuro_hul1 <- c(neuro_hull, neuro_hull[1])
neuro_hull.coords <- neuro_utm[neuro_hull,]
neuro_hull_poly<-Polygon(neuro_hull.coords)
neuro_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Neurology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_neurology<-ENMevaluate(neuro@coords,
                                      ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_neurology@results
tuneparameters_neurology@results[which.min(tuneparameters_neurology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_neurology,file = "tuneparameters_Neurology")
tp_neurology<-readRDS("tuneparameters_Neurology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],muscar,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Neurology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(neuro,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R

maxent_neurology<-maxent(ghana_envvars[[c(4,16,20,26)]],neuro,a=bg_BC,
                         factors="swa_2000lulc_2km",
                         args=c('betamultiplier=2.5',
                                'linear=TRUE',
                                'quadratic=TRUE',
                                'hinge=TRUE',
                                'threshold=FALSE',
                                'product=FALSE',
                                "-P","-J","replicates=5"),
                         path='MaxEntOutput/Neurology')

#Maxent with simple landcover
maxent_neurology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],neuro,a=bg_BC,
                                         
                                         factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                         
                                         args=c('betamultiplier=2.5',
                                                
                                                'linear=TRUE',
                                                
                                                'quadratic=TRUE',
                                                
                                                'hinge=TRUE',
                                                
                                                'threshold=FALSE',
                                                
                                                'product=FALSE',
                                                
                                                "-P","-J","replicates=5"),
                                         
                                         path='MaxEntOutput/Neurology_simplelandcover')



#Merging Oncology species with Gbif records
onco<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Oncology']),]
dim(onco)

levels(droplevels(onco$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine:Oncology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Oncology"]))))
ecosysoncosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Oncology"])))
gbifoncosp<-levels(droplevels(onco$species)) 
ecosysoncosp[which(ecosysoncosp%in%gbifoncosp==F)]

#Oncology convex hull area
#First convert to utm to work in m
onco_utm<-project(onco@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
onco_hull<-chull(onco_utm)
onco_hul1 <- c(onco_hull, onco_hull[1])
onco_hull.coords <- onco_utm[onco_hull,]
onco_hull_poly<-Polygon(onco_hull.coords)
onco_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Oncology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_oncology<-ENMevaluate(onco@coords,
                                     ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_oncology@results
tuneparameters_oncology@results[which.min(tuneparameters_oncology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_oncology,file = "tuneparameters_Oncology")
tp_oncology<-readRDS("tuneparameters_Oncology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],onco,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Oncology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(onco,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_oncology<-maxent(ghana_envvars[[c(4,16,20,26)]],onco,a=bg_BC,
                        factors="swa_2000lulc_2km",
                        args=c('betamultiplier=2.5',
                               'linear=TRUE',
                               'quadratic=TRUE',
                               'hinge=FALSE',
                               'threshold=FALSE',
                               'product=FALSE',
                               "-P","-J","replicates=5"),
                        path='MaxEntOutput/Oncology')

#Maxent with simple landcover
maxent_oncology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],onco,a=bg_BC,
                                        
                                        factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                        
                                        args=c('betamultiplier=2.5',
                                               
                                               'linear=TRUE',
                                               
                                               'quadratic=TRUE',
                                               
                                               'hinge=FALSE',
                                               
                                               'threshold=FALSE',
                                               
                                               'product=FALSE',
                                               
                                               "-P","-J","replicates=5"),
                                        
                                        path='MaxEntOutput/Oncology_simplelandcover')


#Merging Ophthalmology species with Gbif records
opht<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Ophthalmology']),]
dim(opht)

levels(droplevels(opht$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine:Ophthalmology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Ophthalmology"]))))
ecosysophtsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Ophthalmology"])))
gbifophtsp<-levels(droplevels(opht$species)) 
ecosysophtsp[which(ecosysophtsp%in%gbifophtsp==F)]

#Ophthalmology convex hull area
#First convert to utm to work in m
opht_utm<-project(opht@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
opht_hull<-chull(opht_utm)
opht_hul1 <- c(opht_hull, opht_hull[1])
opht_hull.coords <- opht_utm[opht_hull,]
opht_hull_poly<-Polygon(opht_hull.coords)
opht_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Ophthalmology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_ophthalmology<-ENMevaluate(opht@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_ophthalmology@results
tuneparameters_ophthalmology@results[which.min(tuneparameters_ophthalmology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_ophthalmology,file = "tuneparameters_Ophthalmology")
tp_ophthalmology<-readRDS("tuneparameters_Ophthalmology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],opht,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Ophthalmology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(opht,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_ophthalmology<-maxent(ghana_envvars[[c(4,16,20,26)]],opht,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=2.0',
                                    'linear=FALSE',
                                    'quadratic=FALSE',
                                    'hinge=TRUE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Ophthalmology')

#Maxent with simple landcover
maxent_ophthalmology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],opht,a=bg_BC,
                                             
                                             factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                             
                                             args=c('betamultiplier=2.0',
                                                    
                                                    'linear=FALSE',
                                                    
                                                    'quadratic=FALSE',
                                                    
                                                    'hinge=TRUE',
                                                    
                                                    'threshold=FALSE',
                                                    
                                                    'product=FALSE',
                                                    
                                                    "-P","-J","replicates=5"),
                                             
                                             path='MaxEntOutput/Ophthalmology_simplelandcover')


ortho<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Orthopaedics']),]
dim(ortho)

levels(droplevels(ortho$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Orthopaedics"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Orthopaedics"]))))
ecosysorthosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Orthopaedics"])))
gbiforthosp<-levels(droplevels(ortho$species)) 
ecosysorthosp[which(ecosysorthosp%in%gbiforthosp==F)]

#Orthopaedics convex hull area
#First convert to utm to work in m
ortho_utm<-project(ortho@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
ortho_hull<-chull(ortho_utm)
ortho_hul1 <- c(ortho_hull, ortho_hull[1])
ortho_hull.coords <- ortho_utm[ortho_hull,]
ortho_hull_poly<-Polygon(ortho_hull.coords)
ortho_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for Orthopaedics
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_orthopaedics<-ENMevaluate(ortho@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_orthopaedics@results
tuneparameters_orthopaedics@results[which.min(tuneparameters_orthopaedics@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_orthopaedics,file = "tuneparameters_Orthopaedics")
tp_orthopaedics<-readRDS("tuneparameters_Orthopaedics")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],ortho,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Orthopaedics',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(ortho,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_orthopaedics<-maxent(ghana_envvars[[c(4,16,20,26)]],ortho,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=3.0',
                                   'linear=FALSE',
                                   'quadratic=FALSE',
                                   'hinge=TRUE',
                                   'threshold=FALSE',
                                   'product=FALSE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Orthopaedics')
#Maxent with simple landcover
maxent_orthopaedics_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],ortho,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=3.0',
                                                   
                                                   'linear=FALSE',
                                                   
                                                   'quadratic=FALSE',
                                                   
                                                   'hinge=TRUE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Orthopaedics_simplelandcover')


psyc<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Psychiatry']),]
dim(psyc)

levels(droplevels(psyc$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Psychiatry"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Psychiatry"]))))
ecosyspsycsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Psychiatry"])))
gbifpsycsp<-levels(droplevels(psyc$species)) 
ecosyspsycsp[which(ecosyspsycsp%in%gbifpsycsp==F)]

#Psychology convex hull area
#First convert to utm to work in m
psyc_utm<-project(psyc@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
psyc_hull<-chull(psyc_utm)
psyc_hul1 <- c(psyc_hull, psyc_hull[1])
psyc_hull.coords <- psyc_utm[psyc_hull,]
psyc_hull_poly<-Polygon(psyc_hull.coords)
psyc_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Psychiatry
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_psychiatry<-ENMevaluate(psyc@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_psychiatry@results
tuneparameters_psychiatry@results[which.min(tuneparameters_psychiatry@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_psychiatry,file = "tuneparameters_Psychiatry")
tp_psychiatry<-readRDS("tuneparameters_Psychiatry")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],psyc,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Psychiatry',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(psyc,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_psychiatry<-maxent(ghana_envvars[[c(4,16,20,26)]],psyc,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=4.0',
                                 'linear=FALSE',
                                 'quadratic=FALSE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Psychiatry')

#Maxent with simple landcover
maxent_psychiatry_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],psyc,a=bg_BC,
                                          
                                          factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                          
                                          args=c('betamultiplier=4.0',
                                                 
                                                 'linear=FALSE',
                                                 
                                                 'quadratic=FALSE',
                                                 
                                                 'hinge=TRUE',
                                                 
                                                 'threshold=FALSE',
                                                 
                                                 'product=FALSE',
                                                 
                                                 "-P","-J","replicates=5"),
                                          
                                          path='MaxEntOutput/Psychiatry_simplelandcover')


obs<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Obstetrics and gynaecology']),]
dim(obs)

levels(droplevels(obs$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Obstetrics and gynaecology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Obstetrics and gynaecology"]))))
ecosysobssp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Obstetrics and gynaecology"])))
gbifobssp<-levels(droplevels(obs$species)) 
ecosysobssp[which(ecosysobssp%in%gbifobssp==F)]

#Obstetrics convex hull area
#First convert to utm to work in m
obs_utm<-project(obs@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
obs_hull<-chull(obs_utm)
obs_hul1 <- c(obs_hull, obs_hull[1])
obs_hull.coords <- obs_utm[obs_hull,]
obs_hull_poly<-Polygon(obs_hull.coords)
obs_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Obstetrics and gynaecology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_obstetrics<-ENMevaluate(obs@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_obstetrics@results
tuneparameters_obstetrics@results[which.min(tuneparameters_obstetrics@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_obstetrics,file = "tuneparameters_Obstetrics and gynaecology")
tp_obstetrics<-readRDS("tuneparameters_Obstetrics and gynaecology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],obs,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Obstetrics and gynaecology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(obs,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_obstetrics<-maxent(ghana_envvars[[c(4,16,20,26)]],obs,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=3.0',
                                 'linear=FALSE',
                                 'quadratic=FALSE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Obstetrics')

#Maxent with simple landcover
maxent_obstetrics_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],obs,a=bg_BC,
                                          
                                          factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                          
                                          args=c('betamultiplier=3.0',
                                                 
                                                 'linear=FALSE',
                                                 
                                                 'quadratic=FALSE',
                                                 
                                                 'hinge=TRUE',
                                                 
                                                 'threshold=FALSE',
                                                 
                                                 'product=FALSE',
                                                 
                                                 "-P","-J","replicates=5"),
                                          
                                          path='MaxEntOutput/Obstetrics_simplelandcover')




#MaxEnt models for each of the malaria spp ####---- 
malspp<-levels(droplevels(mal$species))
malspp<-malspp[tapply(mal$species,droplevels(mal$species),length)>15]
malariaspp<-data.frame(species=malspp,AUC=rep(NA,times=length(malspp)),
                       bio16.contribution=rep(NA,times=length(malspp)),
                       bio4.contribution=rep(NA,times=length(malspp)),
                       gpw2000_30_sec.contribution=rep(NA,times=length(malspp)),
                       simplelc2000.contribution=rep(NA,times=length(malspp)),
                       bio16.permutation.importance=rep(NA,times=length(malspp)),
                       bio4.permutation.importance=rep(NA,times=length(malspp)),
                       gpw2000_30_sec.permutation.importance=rep(NA,times=length(malspp)),
                       simplelc2000.permutation.importance=rep(NA,times=length(malspp)))

for (i in 1:length(malspp)){
  maxent_mal<-maxent(ghana_envvars[[c(4,16,20,29)]],mal@coords[mal$species==malspp[i],],a=bg_BC,
                     factors="simplelc2000",
                     args=c('betamultiplier=2.5',
                            'linear=TRUE',
                            'quadratic=TRUE',
                            'hinge=FALSE',
                            'threshold=FALSE',
                            'product=FALSE'),#Currently using tuning parameters for whole malaria category
                     path=paste0('MaxEntOutput/MalariaSpecies/',malspp[i]))
  
  malariaspp$AUC[i]<-as.data.frame(t(maxent_mal@results))$Training.AUC
  malariaspp$bio16.contribution[i]<-as.data.frame(t(maxent_mal@results))$bio16.contribution
  malariaspp$bio4.contribution[i]<-as.data.frame(t(maxent_mal@results))$bio4.contribution
  malariaspp$gpw2000_30_sec.contribution[i]<-as.data.frame(t(maxent_mal@results))$gpw2000_30_sec.contribution
  malariaspp$simplelc2000.contribution[i]<-as.data.frame(t(maxent_mal@results))$simplelc2000.contribution
  malariaspp$bio16.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$bio16.permutation.importance
  malariaspp$bio4.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$bio4.permutation.importance
  malariaspp$gpw2000_30_sec.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$gpw2000_30_sec.permutation.importance
  malariaspp$simplelc2000.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$simplelc2000.permutation.importance
}
head(malariaspp)
write.csv(malariaspp,'MalariaSppMaxEntOutput.csv')

hist(malariaspp$AUC)
hist(malariaspp$bio16.contribution) #Repeat for all variables
hist(malariaspp$bio4.contribution)
hist(malariaspp$gpw2000_30_sec.contribution)
hist(malariaspp$simplelc2000.contribution)
hist(malariaspp$bio16.permutation.importance)
hist(malariaspp$bio4.permutation.importance)
hist(malariaspp$gpw2000_30_sec.permutation.importance)
hist(malariaspp$simplelc2000.permutation.importance)


#### AUC scatter graph plots ####
#### No.of species vs AUC values  in categories ####
#Making point plots of no.of species in categories vs AUC values
#read in table
categories<-read.csv('categories.csv')
library("ggplot2")
summary(categories)
#View(categories)
SppAUCCat<-ggplot (categories, aes(x=No_sp, y=AUC)) 
SppAUCCat<-SppAUCCat+geom_point(size=3.5, shape=21, fill="white", stroke =1) 
SppAUCCat<-SppAUCCat+geom_text(aes(label=Categories),hjust=-0.05, vjust=-0.85,size=3.5)
SppAUCCat<-SppAUCCat+geom_text(x=310, y=0.81, label = "(a) Number of species across categories", size=4.5)
SppAUCCat<-SppAUCCat+scale_x_continuous(limits = c(-19,1050), expand = c(0,0.5))
SppAUCCat<-SppAUCCat+scale_y_continuous(limits = c(0.5,.815), expand = c(0,0))
SppAUCCat<-SppAUCCat+ xlab("Number of species")+ylab("Area Under Curve (AUC)")
SppAUCCat<-SppAUCCat+ theme_classic()
SppAUCCat

#### No.of species vs AUC values heathcare groups ####
#Making point plots of no.of species in health care vs AUC values
#read in table
healthcare_groups<-read.csv('healthcare_groups.csv')
summary(healthcare_groups)
#View(healthcare_groups)
SppAUCHealth<-ggplot (healthcare_groups, aes(x=No_sp, y=AUC))
SppAUCHealth<-SppAUCHealth+geom_point(size=3.5, shape=21, fill="dark grey", colour="dark grey", stroke =1) 
SppAUCHealth<-SppAUCHealth+geom_text(aes(label=Groups),hjust=-0.03, vjust=-0.85,size=3.5)
SppAUCHealth<-SppAUCHealth+geom_text(x=72, y=0.815, label = "(b) Number of species in Health care", size=4.5)
SppAUCHealth<-SppAUCHealth+scale_x_continuous(limits = c(-10,255), expand = c(0,0.35))
SppAUCHealth<-SppAUCHealth+scale_y_continuous(limits = c(0.5,.82), expand = c(0,0))
SppAUCHealth<-SppAUCHealth+xlab("Number of species")+ylab("")
SppAUCHealth<-SppAUCHealth+ theme_classic()
SppAUCHealth

#### No. of records in categories vs AUC values ####
#Making points plots of no. of records in categories vs AUC values
#read in table
cat_records<-read.csv('cat_records.csv')
summary(cat_records)
#View(cat_records)
RecAUCCat<-ggplot (cat_records, aes(x=No_records, y=AUC)) 
RecAUCCat<-RecAUCCat+geom_point(size=3.5, shape=21, fill="white", stroke =1) 
RecAUCCat<-RecAUCCat+geom_text(aes(label=Categories),hjust=-0.05, vjust=-0.85,size=3.5)
RecAUCCat<-RecAUCCat+geom_text(x=18500, y=0.795, label = "(c) Number of records across categories", size=4.5)
RecAUCCat<-RecAUCCat+scale_x_continuous(limits = c(-500,61000), expand = c(0,0.35))
RecAUCCat<-RecAUCCat+scale_y_continuous(limits = c(0.5,.8), expand = c(0,0))
RecAUCCat<-RecAUCCat+ xlab("Number of records")+ylab("Area Under Curve (AUC)")
RecAUCCat<-RecAUCCat+theme_classic()
RecAUCCat

#### No. of records in healthcare groups vs AUC value ####
#Making points plots of no. of records in healthcare groups vs AUC values
#read in table
groups_records<-read.csv('groups_records.csv')
summary(groups_records)
#View(groups_records)
RecAUCHealth<-ggplot (groups_records, aes(x=No_records, y=AUC))
RecAUCHealth<-RecAUCHealth+geom_point(size=3.5, shape=21, fill="dark grey", colour="dark grey", stroke =1) 
RecAUCHealth<-RecAUCHealth+geom_text(aes(label=Groups),hjust=-0.03, vjust=-0.85,size=3.5)
RecAUCHealth<-RecAUCHealth+geom_text(x=4500, y=0.815, label = "(d) Number of records in Health care", size=4.5)
RecAUCHealth<-RecAUCHealth+scale_x_continuous(limits = c(-100,15500), expand = c(0,0))
RecAUCHealth<-RecAUCHealth+scale_y_continuous(limits = c(0.5,.82), expand = c(0,0))
RecAUCHealth<-RecAUCHealth+xlab("Number of records")+ylab("")
RecAUCHealth<-RecAUCHealth+theme_classic()
RecAUCHealth

#### Convex hull categories and AUC ####
#Making point plots of area of convex hull in categories vs AUC values
#read in table

cat_hull<-read.csv('cat_hull.csv')
names(cat_hull)
library(scales)
summary(cat_hull)
#View(cat_hull)
cat_hull$Lconvex_hull.km<-log(cat_hull$convex_hull.km)
HullAUCCat<-ggplot (cat_hull, aes(x=convex_hull.km, y=AUC)) 
HullAUCCat<-HullAUCCat+geom_point(size=3.5, shape=21, fill="white", stroke =1) 
HullAUCCat<-HullAUCCat+geom_text(aes(label=Categories),hjust=0.9, vjust=-0.85,size=3.5)
HullAUCCat<-HullAUCCat+geom_text(x=54000, y=0.81, label = "(e) Area across categories", size=4.5)
HullAUCCat<-HullAUCCat+scale_x_continuous(limits = c(0,270000), expand = c(0,0))
HullAUCCat<-HullAUCCat+scale_y_continuous(limits = c(0.5,.815), expand = c(0,0))
HullAUCCat<-HullAUCCat+xlab("Area (km sq)")+ylab("Area Under Curve (AUC)")
HullAUCCat<-HullAUCCat+theme_classic()
HullAUCCat


#### Convex hull health care and AUC ####
#Making point plots of area of convex hull in healthcare groups vs AUC values
#read in table
groups_hull<-read.csv('groups_hull.csv')
library("ggplot2")
summary(groups_hull)
names(groups_hull)
HullAUCHealth<-ggplot (groups_hull, aes(x=convex_hull.km.sq, y=AUC)) 
HullAUCHealth<-HullAUCHealth+geom_point(size=3.5, shape=21, fill="dark grey", colour="dark grey", stroke =1) 
HullAUCHealth<-HullAUCHealth+geom_text(aes(label=Groups),hjust=0.9, vjust=-0.85,size=3.5)
HullAUCHealth<-HullAUCHealth+geom_text(x=56000, y=0.815, label = "(f) Area across Health care", size=4.5)
HullAUCHealth<-HullAUCHealth+scale_x_continuous(limits = c(0,270000), expand = c(0,0))
HullAUCHealth<-HullAUCHealth+scale_y_continuous(limits = c(0.5,.82), expand = c(0,0))
HullAUCHealth<-HullAUCHealth+xlab("Area (km sq)")+ylab("")
HullAUCHealth<-HullAUCHealth+theme_classic()
HullAUCHealth


#Combine: categories, healthcare_groups, cat_records, groups_records
# Packages
library(grid)
require(gridExtra)

# Hashed out the code to make a 

#GhanaMAPS <- paste0("Ghana.maps", "_",Sys.Date(), ".jpeg" )
#jpeg (GhanaMAPS, width=15, height=20, res=400, unit="cm")
grid.arrange(SppAUCCat,SppAUCHealth, RecAUCCat, RecAUCHealth,HullAUCCat,HullAUCHealth,
widths=c(1,1), heights=c(1,1,1),
layout_matrix = cbind(c(1,3,5), c(2,4,6)))

filename <- paste0("AUC.spp.records.area", "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=30, height=32, res=400, unit="cm")
grid.arrange(SppAUCCat,SppAUCHealth, RecAUCCat, RecAUCHealth,HullAUCCat,HullAUCHealth,
             widths=c(1,1), heights=c(1,1,1),
             layout_matrix = cbind(c(1,3,5), c(2,4,6)))
dev.off()

#Making summaries of AUC and variable contributions for all malaria species
#Read in malaria species data
MalariaSppMaxEntOutput<-read.csv('MalariaSppMaxEntOutput.csv')
apply(malariaspp[,2:ncol(malariaspp)],2,mean)
apply(malariaspp[,2:ncol(malariaspp)],2,sd)

##########################################################################################
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
require(rgdal)
require(rgeos)
require(maptools)

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
class(ghanamap)
plot(ghanamap)
plot(dataSpatial, add=T)
crs(ghanamap)
points(ghana_species_pts,pch=16,cex=0.1,col='red') #Species occurence points

#Extract records outside of Ghana land (i.e. sea, other countries)
ghana_species_pts_insidemap<-ghana_species_pts[ghanamap,]
points(ghana_species_pts_insidemap,cex=0.1,pch=16,col='blue')
legend('r',pch=16,col=c('red','blue'),c('Discarded records','Retained records'))
title( main = "Species Occurrence in Ghana")

# Need a background raster for levelplot
r <- raster(ncol=500, nrow=500)
extent(r) <- extent(ghanamap)
ghanamap_ras<-rasterize(ghanamap, r)

# Levelplot for species points
myTheme <- BTCTheme()
myTheme$regions$col = c('white')
GhanaSpp<-levelplot(ghanamap_ras,main='Species records',margin=F, colorkey=NULL,par.settings="myTheme") #scales = list(draw = FALSE)
GhanaSpp<-GhanaSpp+layer(sp.polygons(ghanamap))
GhanaSpp<-GhanaSpp+layer(sp.points(ghana_species_pts_insidemap, pch =3, cex =.25, fill="black",col="black"))
GhanaSpp

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
class(ghanaalt)
levelplot(ghanaalt,margin=F,main='Elevation (m)')+
  layer(sp.polygons(ghanamap))

#WorldClim data
#2.5minute resolution is ca.4.5km at equator
gc1<-getData('worldclim',var='bio',res=2.5)
#Crop and mask to elevation data
ghanaC<-crop(gc1,ghanamap)
ghana1<-mask(ghanaC,ghanamap)
GhanaRain<-levelplot(ghana1$bio12,margin=F,main='Annual precipitation (mm)',par.settings='RdBuTheme')
GhanaRain<-GhanaRain+layer(sp.polygons(ghanamap))
GhanaRain

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

ghanalc1975@data@attributes[[1]][,1:5]

#make a new raster, leaving out the attribute table for simplicity
ghanalc1975_simple <- setValues(raster(ghanalc1975),ghanalc1975[])

# 1 represents forest
ghanalc1975_simple[ghanalc1975_simple==25]<-1
ghanalc1975_simple[ghanalc1975_simple==15]<-1
ghanalc1975_simple[ghanalc1975_simple==21]<-1
ghanalc1975_simple[ghanalc1975_simple==23]<-1
ghanalc1975_simple[ghanalc1975_simple==28]<-1

#2 represents savanna
ghanalc1975_simple[ghanalc1975_simple==4]<-2
ghanalc1975_simple[ghanalc1975_simple==22]<-2
ghanalc1975_simple[ghanalc1975_simple==29]<-2
ghanalc1975_simple[ghanalc1975_simple==31]<-2
ghanalc1975_simple[ghanalc1975_simple==32]<-2

#3 represents wetlands
ghanalc1975_simple[ghanalc1975_simple==7]<-3
ghanalc1975_simple[ghanalc1975_simple==9]<-3

#4 represents agriculture
ghanalc1975_simple[ghanalc1975_simple==6]<-4
ghanalc1975_simple[ghanalc1975_simple==8]<-4
ghanalc1975_simple[ghanalc1975_simple==14]<-4
ghanalc1975_simple[ghanalc1975_simple==24]<-4
ghanalc1975_simple[ghanalc1975_simple==27]<-4

#5 represents landscape area
ghanalc1975_simple[ghanalc1975_simple==10]<-5
ghanalc1975_simple[ghanalc1975_simple==11]<-5
ghanalc1975_simple[ghanalc1975_simple==12]<-5
ghanalc1975_simple[ghanalc1975_simple==13]<-5
ghanalc1975_simple[ghanalc1975_simple==78]<-5

#6 represents clouds
ghanalc1975_simple[ghanalc1975_simple==99]<-6

#Checking summary of simplified habitat areas
ghanalc1975_simple@data@attributes
ghanalc1975_simple
summary(as.factor(ghanalc1975_simple))
summary(as.factor(getValues(ghanalc1975_simple)))
#NB legend is in RAT table
levels(ghanalc1975_simple)

#Do the same with other years too
#Using lc2000 for maxent modelling
lc2000<-raster('Landcover maps/west_africa_land-use_land-cover_2000_2km/swa_2000lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc2000))
ghanalc2000<-mask(crop(lc2000,ghanat),ghanat)

#checking table that describes the different values associated with the values
ghanalc2000@data@attributes[[1]][,1:5]

#make a new raster, leaving out the attribute table for simplicity
ghanalc2000_simple <- setValues(raster(ghanalc2000),ghanalc2000[])

#Simplifying habitat areas
#1 represents forest
ghanalc2000_simple[ghanalc2000_simple==25]<-1
ghanalc2000_simple[ghanalc2000_simple==15]<-1
ghanalc2000_simple[ghanalc2000_simple==21]<-1
ghanalc2000_simple[ghanalc2000_simple==23]<-1
ghanalc2000_simple[ghanalc2000_simple==28]<-1

#2 represents savanna
ghanalc2000_simple[ghanalc2000_simple==4]<-2
ghanalc2000_simple[ghanalc2000_simple==22]<-2
ghanalc2000_simple[ghanalc2000_simple==29]<-2
ghanalc2000_simple[ghanalc2000_simple==31]<-2
ghanalc2000_simple[ghanalc2000_simple==32]<-2

#3 represents wetlands
ghanalc2000_simple[ghanalc2000_simple==7]<-3
ghanalc2000_simple[ghanalc2000_simple==9]<-3

#4 represents agriculture
ghanalc2000_simple[ghanalc2000_simple==6]<-4
ghanalc2000_simple[ghanalc2000_simple==8]<-4
ghanalc2000_simple[ghanalc2000_simple==14]<-4
ghanalc2000_simple[ghanalc2000_simple==24]<-4
ghanalc2000_simple[ghanalc2000_simple==27]<-4

#5 represents landscape area
ghanalc2000_simple[ghanalc2000_simple==10]<-5
ghanalc2000_simple[ghanalc2000_simple==11]<-5
ghanalc2000_simple[ghanalc2000_simple==12]<-5
ghanalc2000_simple[ghanalc2000_simple==13]<-5
ghanalc2000_simple[ghanalc2000_simple==78]<-5

#6 represents clouds
ghanalc2000_simple[ghanalc2000_simple==99]<-6

#Checking summary of simplified habitat areas
ghanalc2000_simple@data@attributes
ghanalc2000_simple
summary(as.factor(ghanalc2000_simple))
summary(as.factor(getValues(ghanalc2000_simple)))

plot(ghanalc2000_simple)
levels(ghanalc2000_simple)
title( main = "Land cover 2000")
legend('r',pch=16,col=c('1','2','3','4','5','6'),c('forest','savanna','wetlands','agriculture','landcape area','cloud'))
levels(ghanalc2000_simple)

lc2013<-raster('Landcover maps/west_africa_land-use_land-cover_2013_2km/swa_2013lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc2013))
ghanalc2013<-mask(crop(lc2013,ghanat),ghanat)
plot(ghanalc2013)
levels(ghanalc2013)
#checking table that describes the different values associated with the values
ghanalc2013@data@attributes[[1]][,1:5]

#make a new raster, leaving out the attribute table for simplicity
ghanalc2013_simple <- setValues(raster(ghanalc2013),ghanalc2013[])

#Simplifying habitat areas
#1 represents forest
ghanalc2013_simple[ghanalc2013_simple==25]<-1
ghanalc2013_simple[ghanalc2013_simple==15]<-1
ghanalc2013_simple[ghanalc2013_simple==21]<-1
ghanalc2013_simple[ghanalc2013_simple==23]<-1
ghanalc2013_simple[ghanalc2013_simple==28]<-1

#2 represents savanna
ghanalc2013_simple[ghanalc2013_simple==4]<-2
ghanalc2013_simple[ghanalc2013_simple==22]<-2
ghanalc2013_simple[ghanalc2013_simple==29]<-2
ghanalc2013_simple[ghanalc2013_simple==31]<-2
ghanalc2013_simple[ghanalc2013_simple==32]<-2

#3 represents wetlands
ghanalc2013_simple[ghanalc2013_simple==7]<-3
ghanalc2013_simple[ghanalc2013_simple==9]<-3

#4 represents agriculture
ghanalc2013_simple[ghanalc2013_simple==6]<-4
ghanalc2013_simple[ghanalc2013_simple==8]<-4
ghanalc2013_simple[ghanalc2013_simple==14]<-4
ghanalc2013_simple[ghanalc2013_simple==24]<-4
ghanalc2013_simple[ghanalc2013_simple==27]<-4

#5 represents landscape area
ghanalc2013_simple[ghanalc2013_simple==10]<-5
ghanalc2013_simple[ghanalc2013_simple==11]<-5
ghanalc2013_simple[ghanalc2013_simple==12]<-5
ghanalc2013_simple[ghanalc2013_simple==13]<-5
ghanalc2013_simple[ghanalc2013_simple==78]<-5

#6 represents clouds
ghanalc2013_simple[ghanalc2013_simple==99]<-6

#Checking summary of simplified habitat areas
ghanalc2013_simple@data@attributes
ghanalc2013_simple
summary(as.factor(ghanalc2013_simple))
summary(as.factor(getValues(ghanalc2013_simple)))

GhanaLand2013<-levelplot(ghanalc2013_simple,margin=F,main='Land cover 2013',par.settings='GrTheme')
GhanaLand2013<-GhanaLand2013+layer(sp.polygons(ghanamap))
GhanaLand2013

#### Combining levelplots in a single figure ####

# Combine: GhanaSpp, GhanaRain, GhanaLand2013
# Packages
library(grid)
require(gridExtra)

# Hashed out the code to make a 

#GhanaMAPS <- paste0("Ghana.maps", "_",Sys.Date(), ".jpeg" )
#jpeg (GhanaMAPS, width=28, height=10, res=400, unit="cm")
grid.arrange(GhanaSpp, GhanaRain, GhanaLand2013, ncol=3, nrow=1, widths=c(1,1,1), heights=c(1),vp = grid::viewport(width=1,height=1),layout_matrix = cbind(c(1), c(2),c(3)))
#dev.off()

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

#Using population data 2000 for maxent modelling
#Stack up popdata
popdat<-stack(pd2000ghana,pd2005ghana,pd2010ghana,pd2015ghana,pd2020ghana)

#Resample to same grid as climate data
popdatrs<-resample(popdat,ghana1,method="bilinear")
writeRaster(popdatrs,'GhanaPopData')
ghanapopdat<-stack('GhanaPopData')

#Stack up lcdata
lcdat<-stack(ghanalc1975, ghanalc2000, ghanalc2013, ghanalc1975_simple,ghanalc2000_simple,ghanalc2013_simple)

#Check the names
names(lcdat)
names(lcdat)[4:6]<-c("simplelc1975","simplelc2000","simplelc2013")

#Project to same crs as climate data
lcdatp<-projectRaster(lcdat,ghana1,method="ngb")

#Resample to same grid as climate data
lcdatrs<-resample(lcdatp,ghana1,method="ngb")#Nearest neighbour as categorical

ghana_envvars<-stack(ghana1,mask(ghanapopdat,ghana1[[1]]),mask(lcdatrs,ghana1[[1]]))
names(ghana_envvars)

#Merging health care with gbif recs to a data frame 
healthcare_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Health care']),]
dim(healthcare_gbif)
levels(droplevels(healthcare_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"]))))
ecosyshcsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Health care"])))
gbifhcsp<-levels(droplevels(healthcare_gbif$species)) 
ecosyshcsp[which(ecosyshcsp%in%gbifhcsp==F)]

#Health care convex hull area
#First convert to utm to work in m
healthcare_gbif_utm<-project(healthcare_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
health_hull<-chull(healthcare_gbif_utm)
health_hul1 <- c(health_hull, health_hull[1])
health_hull.coords <- healthcare_gbif_utm[health_hull,]
health_hull_poly<-Polygon(health_hull.coords)
health_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Health care
library(rJava)
dat=healthcare_gbif@data
m1<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,factors=c("ghana"),args=c("-P"))#Selecting 2000 for pop and lc 
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
tuneparameters_healthcare<-ENMevaluate(healthcare_gbif@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_healthcare@results
tuneparameters_healthcare@results[which.min(tuneparameters_healthcare@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_healthcare,file = "tuneparameters_Health care")
tp_healthcare<-readRDS("tuneparameters_Health care")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Health care',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(healthcare_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_healthcare<-maxent(ghana_envvars[[c(4,16,20,26)]],healthcare_gbif,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=0.5',
                                 'linear=TRUE',
                                 'quadratic=TRUE',
                                 'hinge=FALSE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Health care')

#Maxent with simple landcover
maxent_healthcare_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],healthcare_gbif,a=bg_BC,
                                           
                                           factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                           
                                           args=c('betamultiplier=0.5',
                                                  
                                                  'linear=TRUE',
                                                  
                                                  'quadratic=TRUE',
                                                  
                                                  'hinge=FALSE',
                                                  
                                                  'threshold=FALSE',
                                                  
                                                  'product=FALSE',
                                                  
                                                  "-P","-J","replicates=5"),
                                           
                                           path='MaxEntOutput/Healthcare_simplelandcover')


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


#Agriculture convex hull area
#First convert to utm to work in m
agricult_gbif_utm<-project(agricult_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
agricult_hull<-chull(agricult_gbif_utm)
agricult_hul1 <- c(agricult_hull, agricult_hull[1])
agricult_hull.coords <- agricult_gbif_utm[agricult_hull,]
agricult_hull_poly<-Polygon(agricult_hull.coords)
agricult_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Agriculture
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_agriculture<-ENMevaluate(agricult_gbif@coords,
                                        ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_agriculture@results
tuneparameters_agriculture@results[which.min(tuneparameters_agriculture@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_agriculture,file = "tuneparameters_Agriculture")
tp_agriculture<-readRDS("tuneparameters_Agriculture")



#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],agricult_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Agriculture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(agricult_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_agriculture<-maxent(ghana_envvars[[c(4,16,20,26)]],agricult_gbif,a=bg_BC,
                           factors="swa_2000lulc_2km",
                           args=c('betamultiplier=0.5',
                                  'linear=TRUE',
                                  'quadratic=TRUE',
                                  'hinge=FALSE',
                                  'threshold=FALSE',
                                  'product=FALSE',
                                  "-P","-J","replicates=5"),
                           path='MaxEntOutput/Agriculture')

#Maxent with simple landcover
maxent_agriculture_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],agricult_gbif,a=bg_BC,
                                           
                                           factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                           
                                           args=c('betamultiplier=0.5',
                                                  
                                                  'linear=TRUE',
                                                  
                                                  'quadratic=TRUE',
                                                  
                                                  'hinge=FALSE',
                                                  
                                                  'threshold=FALSE',
                                                  
                                                  'product=FALSE',
                                                  
                                                  "-P","-J","replicates=5"),
                                           
                                           path='MaxEntOutput/Agriculture_simplelandcover')

#Merging water purification with gbif recs to a data frame 
purif_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Water purification']),]
dim(purif_gbif)
levels(droplevels(purif_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purification"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purificaion"]))))
ecosyspursp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Water purification"])))
gbifpursp<-levels(droplevels(purif_gbif$species)) 
ecosyspursp[which(ecosyspursp%in%gbifpursp==F)]


#Water purification convex hull area
#First convert to utm to work in m
purif_gbif_utm<-project(purif_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
purif_hull<-chull(purif_gbif_utm)
purif_hul1 <- c(purif_hull, purif_hull[1])
purif_hull.coords <- purif_gbif_utm[purif_hull,]
purif_hull_poly<-Polygon(purif_hull.coords)
purif_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Water purifiction
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_purification<-ENMevaluate(purif_gbif@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_purification@results
tuneparameters_purification@results[which.min(tuneparameters_purification@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_purification,file = "tuneparameters_Water purification")
tp_purification<-readRDS("tuneparameters_Water purification")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],purif_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Water purification',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(purif_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_purification<-maxent(ghana_envvars[[c(4,16,20,26)]],purif_gbif,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=4.0',
                                   'linear=TRUE',
                                   'quadratic=TRUE',
                                   'hinge=TRUE',
                                   'threshold=FALSE',
                                   'product=TRUE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Water purification')

#Maxent with simple landcover
maxent_purification_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],purif_gbif,a=bg_BC,
                                           
                                           factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                           
                                           args=c('betamultiplier=4.0',
                                                  
                                                  'linear=TRUE',
                                                  
                                                  'quadratic=TRUE',
                                                  
                                                  'hinge=TRUE',
                                                  
                                                  'threshold=FALSE',
                                                  
                                                  'product=TRUE',
                                                  
                                                  "-P","-J","replicates=5"),
                                           
                                           path='MaxEntOutput/Water purification_simplelandcover')


#Merging construction with gbif recs to a data frame
construct_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Construction']),]
dim(construct_gbif)
levels(droplevels(construct_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"]))))
ecosysconssp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Construction"])))
gbifconssp<-levels(droplevels(construct_gbif$species)) 
ecosysconssp[which(ecosysconssp%in%gbifconssp==F)]


#Construction convex hull area
#First convert to utm to work in m
construct_gbif_utm<-project(construct_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
construct_hull<-chull(construct_gbif_utm)
construct_hul1 <- c(construct_hull, construct_hull[1])
construct_hull.coords <- construct_gbif_utm[construct_hull,]
construct_hull_poly<-Polygon(construct_hull.coords)
construct_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Construction
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_construction<-ENMevaluate(construct_gbif@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_construction@results
tuneparameters_construction@results[which.min(tuneparameters_construction@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_construction,file = "tuneparameters_Construction")
tp_construction<-readRDS("tuneparameters_Construction")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],construct_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Construction',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(construct_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_construction<-maxent(ghana_envvars[[c(4,16,20,26)]],construct_gbif,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=0.5',
                                   'linear=TRUE',
                                   'quadratic=TRUE',
                                   'hinge=FALSE',
                                   'threshold=FALSE',
                                   'product=FALSE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Construction')

#Maxent with simple landcover
maxent_construction_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],construct_gbif,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=0.5',
                                                   
                                                   'linear=TRUE',
                                                   
                                                   'quadratic=TRUE',
                                                   
                                                   'hinge=FALSE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Construction_simplelandcover')


#Merging social with gbif recs to a data frame
social_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Social']),]
dim(social_gbif)
levels(droplevels(social_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"]))))
ecosyssocsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Social"])))
gbifsocsp<-levels(droplevels(social_gbif$species)) 
ecosyssocsp[which(ecosyssocsp%in%gbifsocsp==F)]


#Social convex hull area
#First convert to utm to work in m
social_gbif_utm<-project(social_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
social_hull<-chull(social_gbif_utm)
social_hul1 <- c(social_hull, social_hull[1])
social_hull.coords <- social_gbif_utm[social_hull,]
social_hull_poly<-Polygon(social_hull.coords)
social_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for social
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_social<-ENMevaluate(social_gbif@coords,
                                   ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_social@results
tuneparameters_social@results[which.min(tuneparameters_social@results$AICc),]#Can use other parameters to select best methodsaveRDS(tuneparameters_social,file = "tuneparameters_Social")
tp_social<-readRDS("tuneparameters_Social")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],social_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Social',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(social_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_social<-maxent(ghana_envvars[[c(4,16,20,26)]],social_gbif,a=bg_BC,
                      factors="swa_2000lulc_2km",
                      args=c('betamultiplier=4.0',
                             'linear=TRUE',
                             'quadratic=TRUE',
                             'hinge=TRUE',
                             'threshold=FALSE',
                             'product=TRUE',
                             "-P","-J","replicates=5"),
                      path='MaxEntOutput/Social')

#Maxent with simple landcover
maxent_social_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],social_gbif,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=4.0',
                                                   
                                                   'linear=TRUE',
                                                   
                                                   'quadratic=TRUE',
                                                   
                                                   'hinge=TRUE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=TRUE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Social_simplelandcover')


#Merging energy with gbif recs to a data frame 
energy_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Energy']),]
dim(energy_gbif)
levels(droplevels(energy_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"]))))
ecosysensp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Energy"])))
gbifensp<-levels(droplevels(energy_gbif$species)) 
ecosysensp[which(ecosysensp%in%gbifensp==F)]


#Energy convex hull area
#First convert to utm to work in m
energy_gbif_utm<-project(energy_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
energy_hull<-chull(energy_gbif_utm)
energy_hul1 <- c(energy_hull, energy_hull[1])
energy_hull.coords <- energy_gbif_utm[energy_hull,]
energy_hull_poly<-Polygon(energy_hull.coords)
energy_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for energy
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_energy<-ENMevaluate(energy_gbif@coords,
                                   ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_energy@results
tuneparameters_energy@results[which.min(tuneparameters_energy@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_energy,file = "tuneparameters_Energy")
tp_energy<-readRDS("tuneparameters_Energy")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],energy_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Energy',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(energy_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_energy<-maxent(ghana_envvars[[c(4,16,20,26)]],energy_gbif,a=bg_BC,
                      factors="swa_2000lulc_2km",
                      args=c('betamultiplier=4.0',
                             'linear=FALSE',
                             'quadratic=FALSE',
                             'hinge=TRUE',
                             'threshold=FALSE',
                             'product=FALSE',
                             "-P","-J","replicates=5"),
                      path='MaxEntOutput/Energy')

#Maxent with simple landcover
maxent_energy_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],energy_gbif,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=4.0',
                                                   
                                                   'linear=FALSE',
                                                   
                                                   'quadratic=FALSE',
                                                   
                                                   'hinge=TRUE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Energy_simplelandcover')

#Merging food and nutrition with gbif recs to a data frame
foodnutr_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Food and nutrition']),]
dim(foodnutr_gbif)
levels(droplevels(foodnutr_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"]))))
ecosysfnsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Food and nutrition"])))
gbiffnsp<-levels(droplevels(foodnutr_gbif$species)) 
ecosysfnsp[which(ecosysfnsp%in%gbiffnsp==F)]

#Food and nutrition convex hull area
#First convert to utm to work in m
foodnutr_gbif_utm<-project(foodnutr_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
foodnutr_hull<-chull(foodnutr_gbif_utm)
foodnutr_hul1 <- c(foodnutr_hull, foodnutr_hull[1])
foodnutr_hull.coords <- foodnutr_gbif_utm[foodnutr_hull,]
foodnutr_hull_poly<-Polygon(foodnutr_hull.coords)
foodnutr_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Food and nutrition
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_foodnutrition<-ENMevaluate(foodnutr_gbif@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_foodnutrition@results
tuneparameters_foodnutrition@results[which.min(tuneparameters_foodnutrition@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_foodnutrition,file = "tuneparameters_Food and nutrition")
tp_healthcare<-readRDS("tuneparameters_Food and nutrition")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],foodnutr_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Food and nutrition',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(foodnutr_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_foodnutrition<-maxent(ghana_envvars[[c(4,16,20,26)]],foodnutr_gbif,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=0.5',
                                    'linear=TRUE',
                                    'quadratic=TRUE',
                                    'hinge=FALSE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Food and nutrition')

#Maxent with simple landcover
maxent_foodnutrition_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],foodnutr_gbif,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=0.5',
                                                   
                                                   'linear=TRUE',
                                                   
                                                   'quadratic=TRUE',
                                                   
                                                   'hinge=FALSE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Food and nutrition_simplelandcover')


#Merging Culture with gbif recs to a data frame 
cultur_gbif<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Category=='Culture']),]
dim(cultur_gbif)
levels(droplevels(cultur_gbif$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"]))))
ecosysculsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Category=="Culture"])))
gbifculsp<-levels(droplevels(cultur_gbif$species)) 
ecosysculsp[which(ecosysculsp%in%gbifculsp==F)]

#Culture convex hull area
#First convert to utm to work in m
cultur_gbif_utm<-project(cultur_gbif@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
cultur_hull<-chull(cultur_gbif_utm)
cultur_hul1 <- c(cultur_hull, cultur_hull[1])
cultur_hull.coords <- cultur_gbif_utm[cultur_hull,]
cultur_hull_poly<-Polygon(cultur_hull.coords)
cultur_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for culture
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data

tuneparameters_culture<-ENMevaluate(cultur_gbif@coords,
                                    ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)


tuneparameters_culture@results
tuneparameters_culture@results[which.min(tuneparameters_culture@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_culture,file = "tuneparameters_Culture")
tp_culture<-readRDS("tuneparameters_Culture")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data

maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],cultur_gbif,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.5','threshold=FALSE','product=FALSE',"-P","-J"))


maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Culture',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(cultur_gbif,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_culture<-maxent(ghana_envvars[[c(4,16,20,26)]],cultur_gbif,a=bg_BC,
                       factors="swa_2000lulc_2km",
                       args=c('betamultiplier=3.5',
                              'linear=FALSE',
                              'quadratic=FALSE',
                              'hinge=TRUE',
                              'threshold=FALSE',
                              'product=FALSE',
                              "-P","-J","replicates=5"),
                       path='MaxEntOutput/Culture')

#Maxent with simple landcover
maxent_culture_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],cultur_gbif,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=3.5',
                                                   
                                                   'linear=FALSE',
                                                   
                                                   'quadratic=FALSE',
                                                   
                                                   'hinge=TRUE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Culture_simplelandcover')

#Category count analysis
speciesdata<-read.csv('FinalDataSpeciesNames.csv')

#Count species in each group within health care
with(droplevels(GhanaUses[GhanaUses$Category=='Health care',]),tapply(ConfirmedSppNames,Group,length))

#Count species within Anaesthetics of health care 
with(droplevels(speciesdata[speciesdata$Group=='Medicine: Anaesthetics',]),tapply(ConfirmedSppNames,Category,length))

#MaxEnt modelling of groups within health care category
#Merging Anaesthetics species with Gbif records
anaesth<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Anaesthetics']),]
dim(anaesth)

levels(droplevels(anaesth$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Anaesthetics"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Anaesthetics"]))))
ecosysansp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Anaesthetics"])))
gbifansp<-levels(droplevels(anaesth$species)) 
ecosysansp[which(ecosysansp%in%gbifansp==F)]


#Anaesthetics convex hull area
#First convert to utm to work in m
anaesth_utm<-project(anaesth@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
anaesth_hull<-chull(anaesth_utm)
anaesth_hul1 <- c(anaesth_hull, anaesth_hull[1])
anaesth_hull.coords <- anaesth_utm[anaesth_hull,]
anaesth_hull_poly<-Polygon(anaesth_hull.coords)
anaesth_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Anaesthetics
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_anaesthetics<-ENMevaluate(anaesth@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_anaesthetics@results
tuneparameters_anaesthetics@results[which.min(tuneparameters_anaesthetics@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_anaesthetics,file = "tuneparameters_Anaesthetics")
tp_anaesthetics<-readRDS("tuneparameters_Anaesthetics")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],anaesth,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Anaesthetics',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(anaesth,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_anaesthetics<-maxent(ghana_envvars[[c(4,16,20,26)]],anaesth,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=0.5',
                                   'linear=TRUE',
                                   'quadratic=TRUE',
                                   'hinge=FALSE',
                                   'threshold=FALSE',
                                   'product=FALSE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Anaesthetics')


#Maxent with simple landcover
maxent_anaesthetics_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],anaesth,a=bg_BC,
                                            
                                            factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                            
                                            args=c('betamultiplier=0.5',
                                                   
                                                   'linear=TRUE',
                                                   
                                                   'quadratic=TRUE',
                                                   
                                                   'hinge=FALSE',
                                                   
                                                   'threshold=FALSE',
                                                   
                                                   'product=FALSE',
                                                   
                                                   "-P","-J","replicates=5"),
                                            
                                            path='MaxEntOutput/Anaesthetics_simplelandcover')


#Do same with other groups
#Merging Dentistry species with Gbif records
dent<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Dentistry']),]
dim(dent)

levels(droplevels(dent$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dentistry"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dentistry"]))))
ecosysdentsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dentistry"])))
gbifdentsp<-levels(droplevels(dent$species)) 
ecosysdentsp[which(ecosysdentsp%in%gbifdentsp==F)]

#Dentistry convex hull area
#First convert to utm to work in m
dent_utm<-project(dent@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
dent_hull<-chull(dent_utm)
dent_hul1 <- c(dent_hull, dent_hull[1])
dent_hull.coords <- dent_utm[dent_hull,]
dent_hull_poly<-Polygon(dent_hull.coords)
dent_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Dentistry
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_dentistry<-ENMevaluate(dent@coords,
                                      ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_dentistry@results
tuneparameters_dentistry@results[which.min(tuneparameters_dentistry@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_dentistry,file = "tuneparameters_Dentistry")
tp_dentistry<-readRDS("tuneparameters_Dentistry")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],dent,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Dentistry',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(dent,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_dentistry<-maxent(ghana_envvars[[c(4,16,20,26)]],dent,a=bg_BC,
                         factors="swa_2000lulc_2km",
                         args=c('betamultiplier=3.5',
                                'linear=FALSE',
                                'quadratic=FALSE',
                                'hinge=TRUE',
                                'threshold=FALSE',
                                'product=FALSE',
                                "-P","-J","replicates=5"),
                         path='MaxEntOutput/Dentistry')


#Maxent with simple landcover
maxent_dentistry_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],dent,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=3.5',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Dentistry_simplelandcover')

#Merging Dermatology species with Gbif records
derm<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Dermatology']),]
dim(derm)

levels(droplevels(derm$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dermatology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dermatology"]))))
ecosysdermsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Dermatology"])))
gbifdermsp<-levels(droplevels(derm$species)) 
ecosysdermsp[which(ecosysdermsp%in%gbifdermsp==F)]

#Dermatology convex hull area
#First convert to utm to work in m
derm_utm<-project(derm@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
derm_hull<-chull(derm_utm)
derm_hul1 <- c(derm_hull, derm_hull[1])
derm_hull.coords <- derm_utm[derm_hull,]
derm_hull_poly<-Polygon(derm_hull.coords)
derm_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Dermatology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_dermatology<-ENMevaluate(derm@coords,
                                        ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_dermatology@results
tuneparameters_dermatology@results[which.min(tuneparameters_dermatology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_dermatology,file = "tuneparameters_Dermatology")
tp_dermatology<-readRDS("tuneparameters_Dermatology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],derm,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Dermatology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(derm,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_dermatology<-maxent(ghana_envvars[[c(4,16,20,26)]],derm,a=bg_BC,
                           factors="swa_2000lulc_2km",
                           args=c('betamultiplier=0.5',
                                  'linear=TRUE',
                                  'quadratic=TRUE',
                                  'hinge=FALSE',
                                  'threshold=FALSE',
                                  'product=FALSE',
                                  "-P","-J","replicates=5"),
                           path='MaxEntOutput/Dermatology')

#Maxent with simple landcover
maxent_dermatology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],derm,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=0.5',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=FALSE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Dermatology_simplelandcover')

#Merging Endocrinology species with Gbif records
endo<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Endocrinology']),]
dim(endo)

levels(droplevels(endo$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Endocrinology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Endocrinology"]))))
ecosysendosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Endocrinology"])))
gbifendosp<-levels(droplevels(endo$species)) 
ecosysendosp[which(ecosysendosp%in%gbifendosp==F)]

#Endocrinology convex hull area
#First convert to utm to work in m
endo_utm<-project(endo@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
endo_hull<-chull(endo_utm)
endo_hul1 <- c(endo_hull, endo_hull[1])
endo_hull.coords <- endo_utm[endo_hull,]
endo_hull_poly<-Polygon(endo_hull.coords)
endo_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Endocrinology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_endocrinology<-ENMevaluate(endo@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_endocrinology@results
tuneparameters_endocrinology@results[which.min(tuneparameters_endocrinology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_endocrinology,file = "tuneparameters_Endocrinology")
tp_endocrinology<-readRDS("tuneparameters_Endocrinology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],endo,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=0.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Endocrinology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(endo,col=1))

#Write maxent results to file which you can later read in to make response curves etc in R
maxent_endocrinology<-maxent(ghana_envvars[[c(4,16,20,26)]],endo,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=0.5',
                                    'linear=TRUE',
                                    'quadratic=TRUE',
                                    'hinge=FALSE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Endocrinology')

#Maxent with simple landcover
maxent_endocrinology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],endo,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=0.5',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=FALSE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Endocrinology_simplelandcover')


#Merging Excipients species with Gbif records
exci<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Excipients']),]
dim(exci)

levels(droplevels(exci$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Excipients"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Excipients"]))))
ecosysexcisp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Excipients"])))
gbifexcisp<-levels(droplevels(exci$species)) 
ecosysexcisp[which(ecosysexcisp%in%gbifexcisp==F)]


#Excipients convex hull area
#First convert to utm to work in m
exci_utm<-project(exci@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
exci_hull<-chull(exci_utm)
exci_hul1 <- c(exci_hull, exci_hull[1])
exci_hull.coords <- exci_utm[exci_hull,]
exci_hull_poly<-Polygon(exci_hull.coords)
exci_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Excipients
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_excipients<-ENMevaluate(exci@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_excipients@results
tuneparameters_excipients@results[which.min(tuneparameters_excipients@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_excipients,file = "tuneparameters_Excipients")
tp_excipients<-readRDS("tuneparameters_Excipients")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],exci,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.0','threshold=FALSE','product=TRUE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Excipients',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(exci,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_excipients<-maxent(ghana_envvars[[c(4,16,20,26)]],exci,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=3.0',
                                 'linear=TRUE',
                                 'quadratic=TRUE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=TRUE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Excipients')

#Maxent with simple landcover
maxent_excipients_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],exci,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=3.0',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=TRUE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Excipients_simplelandcover')


#Merging Fever species with Gbif records
fev<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Fever']),]
dim(fev)

levels(droplevels(fev$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Fever"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Fever"]))))
ecosysfevsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Fever"])))
gbiffevsp<-levels(droplevels(fev$species)) 
ecosysfevsp[which(ecosysfevsp%in%gbiffevsp==F)]

#Fever convex hull area
#First convert to utm to work in m
fev_utm<-project(fev@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
fev_hull<-chull(fev_utm)
fev_hul1 <- c(fev_hull, fev_hull[1])
fev_hull.coords <- fev_utm[fev_hull,]
fev_hull_poly<-Polygon(fev_hull.coords)
fev_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Fever
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_fever<-ENMevaluate(fev@coords,
                                  ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_fever@results                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
tuneparameters_fever@results[which.min(tuneparameters_fever@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_fever,file = "tuneparameters_Fever")
tp_fever<-readRDS("tuneparameters_Fever")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],fev,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Fever',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(fev,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_fever<-maxent(ghana_envvars[[c(4,16,20,26)]],fev,a=bg_BC,
                     factors="swa_2000lulc_2km",
                     args=c('betamultiplier=4.0',
                            'linear=TRUE',
                            'quadratic=TRUE',
                            'hinge=FALSE',
                            'threshold=FALSE',
                            'product=FALSE',
                            "-P","-J","replicates=5"),
                     path='MaxEntOutput/Fever')
#Maxent with simple landcover
maxent_fever_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],fev,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=4.0',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=FALSE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Fever_simplelandcover')

#Merging Immunology species with Gbif records
immu<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Immunology']),]
dim(immu)

levels(droplevels(immu$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Immunology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Immunology"]))))
ecosysimmusp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Immunology"])))
gbifimmusp<-levels(droplevels(immu$species)) 
ecosysimmusp[which(ecosysimmusp%in%gbifimmusp==F)]

#Immunology convex hull area
#First convert to utm to work in m
immu_utm<-project(immu@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
immu_hull<-chull(immu_utm)
immu_hul1 <- c(immu_hull, immu_hull[1])
immu_hull.coords <- immu_utm[immu_hull,]
immu_hull_poly<-Polygon(immu_hull.coords)
immu_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Immunology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_immunology<-ENMevaluate(immu@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_immunology@results
tuneparameters_immunology@results[which.min(tuneparameters_immunology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_immunology,file = "tuneparameters_Immunology")
tp_immunology<-readRDS("tuneparameters_immunology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],immu,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Immunology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(immu,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_immunology<-maxent(ghana_envvars[[c(4,16,20,26)]],immu,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=3.5',
                                 'linear=FALSE',
                                 'quadratic=FALSE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Immunology')

#Maxent with simple landcover
maxent_immunology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],immu,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=3.5',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Immunology_simplelandcover')


#Merging Infertility species with Gbif records
infer<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Infertility']),]
dim(infer)

levels(droplevels(infer$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Infertility"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Infertility"]))))
ecosysinfersp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Infertility"])))
gbifinfersp<-levels(droplevels(infer$species)) 
ecosysinfersp[which(ecosysinfersp%in%gbifinfersp==F)]

#Infertility convex hull area
#First convert to utm to work in m
infer_utm<-project(infer@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
infer_hull<-chull(infer_utm)
infer_hul1 <- c(infer_hull, infer_hull[1])
infer_hull.coords <- infer_utm[infer_hull,]
infer_hull_poly<-Polygon(infer_hull.coords)
infer_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Infertility
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_infertility<-ENMevaluate(infer@coords,
                                        ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_infertility@results
tuneparameters_infertility@results[which.min(tuneparameters_infertility@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_infertility,file = "tuneparameters_Infertility")
tp_infertility<-readRDS("tuneparameters_Infertility")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],infer,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))


maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Infertility',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(infer,col=1))

#Write maxent results to file which you can later read in to make response curves etc in R
#Example with switches for LQHPT
maxent_infertility<-maxent(ghana_envvars[[c(4,16,20,26)]],infer,a=bg_BC,
                           factors="swa_2000lulc_2km",
                           args=c('betamultiplier=4.0',
                                  'linear=FALSE',
                                  'quadratic=FALSE',
                                  'hinge=TRUE',
                                  'threshold=FALSE',
                                  'product=FALSE',
                                  "-P","-J","replicates=5"),
                           path='MaxEntOutput/Infertility')

#Maxent with simple landcover
maxent_infertility_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],infer,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=4.0',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Infertility_simplelandcover')

#Merging Malaria species with Gbif records
mal<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Malaria']),]
dim(mal)

levels(droplevels(mal$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Malaria"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Malaria"]))))
ecosysmalsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Malaria"])))
gbifmalsp<-levels(droplevels(mal$species)) 
ecosysmalsp[which(ecosysmalsp%in%gbifmalsp==F)]

#Malaria convex hull area
#First convert to utm to work in m
mal_utm<-project(mal@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
mal_hull<-chull(mal_utm)
mal_hul1 <- c(mal_hull, mal_hull[1])
mal_hull.coords <- mal_utm[mal_hull,]
mal_hull_poly<-Polygon(mal_hull.coords)
mal_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for Malaria
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_malaria<-ENMevaluate(mal@coords,
                                    ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_malaria@results
tuneparameters_malaria@results[which.min(tuneparameters_malaria@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_malaria,file = "tuneparameters_Malaria")
tp_malaria<-readRDS("tuneparameters_Malaria")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],mal,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Malaria',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(mal,col=1))



#Write maxent results to file which you can later read in to make response curves etc in R
#Example with switches for LQHPT
maxent_malaria<-maxent(ghana_envvars[[c(4,16,20,26)]],mal,a=bg_BC,
                       factors="swa_2000lulc_2km",
                       args=c('betamultiplier=2.5',
                              'linear=TRUE',
                              'quadratic=TRUE',
                              'hinge=FALSE',
                              'threshold=FALSE',
                              'product=FALSE',
                              "-P","-J","replicates=5"),
                       path='MaxEntOutput/Malaria')
#Maxent with simple landcover
maxent_malaria_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],mal,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=2.5',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=FALSE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Malaria_simplelandcover')



#Merging Musculoskeletal and cardiology species with Gbif records
muscar<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Musculoskeletal and cardiology']),]
dim(muscar)

levels(droplevels(muscar$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Musculoskeletal and cardiology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Musculoskeletal and cardiology"]))))
ecosysmuscarsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Musculoskeletal and cardiology"])))
gbifmuscarsp<-levels(droplevels(muscar$species)) 
ecosysmuscarsp[which(ecosysmuscarsp%in%gbifmuscarsp==F)]

#Musculoskeletal and cardiology convex hull area
#First convert to utm to work in m
muscar_utm<-project(muscar@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
muscar_hull<-chull(muscar_utm)
muscar_hul1 <- c(muscar_hull, muscar_hull[1])
muscar_hull.coords <- muscar_utm[muscar_hull,]
muscar_hull_poly<-Polygon(muscar_hull.coords)
muscar_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Musculoskeletal and cardiology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_muscardiology<-ENMevaluate(muscar@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_muscardiology@results
tuneparameters_muscardiology@results[which.min(tuneparameters_muscardiology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_muscardiology,file = "tuneparameters_Musculoskeletal and cardiology")
tp_muscardiology<-readRDS("tuneparameters_Musculoskeletal and cardiology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],muscar,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Musculoskeletal and cardiology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(muscar,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
#Example with switches for LQHPT
maxent_muscardiology<-maxent(ghana_envvars[[c(4,16,20,26)]],muscar,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=2.0',
                                    'linear=TRUE',
                                    'quadratic=TRUE',
                                    'hinge=TRUE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Musculoskeletal and cardiology')
#Maxent with simple landcover
maxent_muscardiology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],muscar,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=2.0',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Musculoskeletal and cardiology_simplelandcover')



#Merging Neurology species with Gbif records
neuro<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Neurology']),]
dim(neuro)

levels(droplevels(neuro$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Neurology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Neurology"]))))
ecosysneurosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Neurology"])))
gbifneurosp<-levels(droplevels(neuro$species)) 
ecosysneurosp[which(ecosysneurosp%in%gbifneurosp==F)]

#Neurology convex hull area
#First convert to utm to work in m
neuro_utm<-project(neuro@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
neuro_hull<-chull(neuro_utm)
neuro_hul1 <- c(neuro_hull, neuro_hull[1])
neuro_hull.coords <- neuro_utm[neuro_hull,]
neuro_hull_poly<-Polygon(neuro_hull.coords)
neuro_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Neurology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_neurology<-ENMevaluate(neuro@coords,
                                      ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_neurology@results
tuneparameters_neurology@results[which.min(tuneparameters_neurology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_neurology,file = "tuneparameters_Neurology")
tp_neurology<-readRDS("tuneparameters_Neurology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],muscar,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Neurology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(neuro,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R

maxent_neurology<-maxent(ghana_envvars[[c(4,16,20,26)]],neuro,a=bg_BC,
                         factors="swa_2000lulc_2km",
                         args=c('betamultiplier=2.5',
                                'linear=TRUE',
                                'quadratic=TRUE',
                                'hinge=TRUE',
                                'threshold=FALSE',
                                'product=FALSE',
                                "-P","-J","replicates=5"),
                         path='MaxEntOutput/Neurology')

#Maxent with simple landcover
maxent_neurology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],neuro,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=2.5',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Neurology_simplelandcover')



#Merging Oncology species with Gbif records
onco<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Oncology']),]
dim(onco)

levels(droplevels(onco$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine:Oncology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Oncology"]))))
ecosysoncosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Oncology"])))
gbifoncosp<-levels(droplevels(onco$species)) 
ecosysoncosp[which(ecosysoncosp%in%gbifoncosp==F)]

#Oncology convex hull area
#First convert to utm to work in m
onco_utm<-project(onco@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
onco_hull<-chull(onco_utm)
onco_hul1 <- c(onco_hull, onco_hull[1])
onco_hull.coords <- onco_utm[onco_hull,]
onco_hull_poly<-Polygon(onco_hull.coords)
onco_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Oncology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_oncology<-ENMevaluate(onco@coords,
                                     ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_oncology@results
tuneparameters_oncology@results[which.min(tuneparameters_oncology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_oncology,file = "tuneparameters_Oncology")
tp_oncology<-readRDS("tuneparameters_Oncology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],onco,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.5','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Oncology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(onco,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_oncology<-maxent(ghana_envvars[[c(4,16,20,26)]],onco,a=bg_BC,
                        factors="swa_2000lulc_2km",
                        args=c('betamultiplier=2.5',
                               'linear=TRUE',
                               'quadratic=TRUE',
                               'hinge=FALSE',
                               'threshold=FALSE',
                               'product=FALSE',
                               "-P","-J","replicates=5"),
                        path='MaxEntOutput/Oncology')

#Maxent with simple landcover
maxent_oncology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],onco,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=2.5',
                                              
                                              'linear=TRUE',
                                              
                                              'quadratic=TRUE',
                                              
                                              'hinge=FALSE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Oncology_simplelandcover')


#Merging Ophthalmology species with Gbif records
opht<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Ophthalmology']),]
dim(opht)

levels(droplevels(opht$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine:Ophthalmology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Ophthalmology"]))))
ecosysophtsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Ophthalmology"])))
gbifophtsp<-levels(droplevels(opht$species)) 
ecosysophtsp[which(ecosysophtsp%in%gbifophtsp==F)]

#Ophthalmology convex hull area
#First convert to utm to work in m
opht_utm<-project(opht@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
opht_hull<-chull(opht_utm)
opht_hul1 <- c(opht_hull, opht_hull[1])
opht_hull.coords <- opht_utm[opht_hull,]
opht_hull_poly<-Polygon(opht_hull.coords)
opht_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Ophthalmology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_ophthalmology<-ENMevaluate(opht@coords,
                                          ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_ophthalmology@results
tuneparameters_ophthalmology@results[which.min(tuneparameters_ophthalmology@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_ophthalmology,file = "tuneparameters_Ophthalmology")
tp_ophthalmology<-readRDS("tuneparameters_Ophthalmology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],opht,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=2.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Ophthalmology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(opht,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_ophthalmology<-maxent(ghana_envvars[[c(4,16,20,26)]],opht,a=bg_BC,
                             factors="swa_2000lulc_2km",
                             args=c('betamultiplier=2.0',
                                    'linear=FALSE',
                                    'quadratic=FALSE',
                                    'hinge=TRUE',
                                    'threshold=FALSE',
                                    'product=FALSE',
                                    "-P","-J","replicates=5"),
                             path='MaxEntOutput/Ophthalmology')

#Maxent with simple landcover
maxent_ophthalmology_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],opht,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=2.0',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Ophthalmology_simplelandcover')


ortho<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Orthopaedics']),]
dim(ortho)

levels(droplevels(ortho$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Orthopaedics"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Orthopaedics"]))))
ecosysorthosp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Orthopaedics"])))
gbiforthosp<-levels(droplevels(ortho$species)) 
ecosysorthosp[which(ecosysorthosp%in%gbiforthosp==F)]

#Orthopaedics convex hull area
#First convert to utm to work in m
ortho_utm<-project(ortho@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
ortho_hull<-chull(ortho_utm)
ortho_hul1 <- c(ortho_hull, ortho_hull[1])
ortho_hull.coords <- ortho_utm[ortho_hull,]
ortho_hull_poly<-Polygon(ortho_hull.coords)
ortho_hull_poly@area/(1000*1000)


#Basic MaxEnt with two predictors for Orthopaedics
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_orthopaedics<-ENMevaluate(ortho@coords,
                                         ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_orthopaedics@results
tuneparameters_orthopaedics@results[which.min(tuneparameters_orthopaedics@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_orthopaedics,file = "tuneparameters_Orthopaedics")
tp_orthopaedics<-readRDS("tuneparameters_Orthopaedics")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],ortho,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Orthopaedics',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(ortho,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_orthopaedics<-maxent(ghana_envvars[[c(4,16,20,26)]],ortho,a=bg_BC,
                            factors="swa_2000lulc_2km",
                            args=c('betamultiplier=3.0',
                                   'linear=FALSE',
                                   'quadratic=FALSE',
                                   'hinge=TRUE',
                                   'threshold=FALSE',
                                   'product=FALSE',
                                   "-P","-J","replicates=5"),
                            path='MaxEntOutput/Orthopaedics')
#Maxent with simple landcover
maxent_orthopaedics_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],ortho,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=3.0',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Orthopaedics_simplelandcover')


psyc<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Psychiatry']),]
dim(psyc)

levels(droplevels(psyc$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Psychiatry"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Psychiatry"]))))
ecosyspsycsp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Psychiatry"])))
gbifpsycsp<-levels(droplevels(psyc$species)) 
ecosyspsycsp[which(ecosyspsycsp%in%gbifpsycsp==F)]

#Psychology convex hull area
#First convert to utm to work in m
psyc_utm<-project(psyc@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
psyc_hull<-chull(psyc_utm)
psyc_hul1 <- c(psyc_hull, psyc_hull[1])
psyc_hull.coords <- psyc_utm[psyc_hull,]
psyc_hull_poly<-Polygon(psyc_hull.coords)
psyc_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Psychiatry
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_psychiatry<-ENMevaluate(psyc@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_psychiatry@results
tuneparameters_psychiatry@results[which.min(tuneparameters_psychiatry@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_psychiatry,file = "tuneparameters_Psychiatry")
tp_psychiatry<-readRDS("tuneparameters_Psychiatry")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],psyc,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=4.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Psychiatry',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(psyc,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_psychiatry<-maxent(ghana_envvars[[c(4,16,20,26)]],psyc,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=4.0',
                                 'linear=FALSE',
                                 'quadratic=FALSE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Psychiatry')

#Maxent with simple landcover
maxent_psychiatry_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],psyc,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=4.0',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Psychiatry_simplelandcover')


obs<-ghana_species_pts_insidemap[which(ghana_species_pts_insidemap$species%in%GhanaUses$ConfirmedSppNames[GhanaUses$Group=='Medicine: Obstetrics and gynaecology']),]
dim(obs)

levels(droplevels(obs$species)) #Check this against the species in your table 
length(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Obstetrics and gynaecology"]))))
(levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Obstetrics and gynaecology"]))))
ecosysobssp<-levels(droplevels(as.factor(GhanaUses$ConfirmedSppNames[GhanaUses$Group=="Medicine: Obstetrics and gynaecology"])))
gbifobssp<-levels(droplevels(obs$species)) 
ecosysobssp[which(ecosysobssp%in%gbifobssp==F)]

#Obstetrics convex hull area
#First convert to utm to work in m
obs_utm<-project(obs@coords,'+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')
obs_hull<-chull(obs_utm)
obs_hul1 <- c(obs_hull, obs_hull[1])
obs_hull.coords <- obs_utm[obs_hull,]
obs_hull_poly<-Polygon(obs_hull.coords)
obs_hull_poly@area/(1000*1000)

#Basic MaxEnt with two predictors for Obstetrics and gynaecology
#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters_obstetrics<-ENMevaluate(obs@coords,
                                       ghana_envvars[[c(4,16,20,26)]],algorithm="maxent.jar",categoricals="swa_2000lulc_2km",method="block",bg.coords=bg_BC)

tuneparameters_obstetrics@results
tuneparameters_obstetrics@results[which.min(tuneparameters_obstetrics@results$AICc),]#Can use other parameters to select best method
saveRDS(tuneparameters_obstetrics,file = "tuneparameters_Obstetrics and gynaecology")
tp_obstetrics<-readRDS("tuneparameters_Obstetrics and gynaecology")

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana_envvars[[c(4,16,20,26)]],obs,a=bg_BC,factors="swa_2000lulc_2km",args=c('betamultiplier=3.0','threshold=FALSE','product=FALSE',"-P","-J"))

maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana_envvars)

levelplot(pfull,par.settings='rtheme',margin=F,main='Obstetrics and gynaecology',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))+
  layer(sp.points(obs,col=1))


#Write maxent results to file which you can later read in to make response curves etc in R
maxent_obstetrics<-maxent(ghana_envvars[[c(4,16,20,26)]],obs,a=bg_BC,
                          factors="swa_2000lulc_2km",
                          args=c('betamultiplier=3.0',
                                 'linear=FALSE',
                                 'quadratic=FALSE',
                                 'hinge=TRUE',
                                 'threshold=FALSE',
                                 'product=FALSE',
                                 "-P","-J","replicates=5"),
                          path='MaxEntOutput/Obstetrics')

#Maxent with simple landcover
maxent_obstetrics_simplelandcover<-maxent(ghana_envvars[[c(4,16,20,29)]],obs,a=bg_BC,
                                       
                                       factors="simplelc2000",#ALSO CHANGE THIS TO THE NAME OF THE SIMPLE LANDCOVER VARIABLE
                                       
                                       args=c('betamultiplier=3.0',
                                              
                                              'linear=FALSE',
                                              
                                              'quadratic=FALSE',
                                              
                                              'hinge=TRUE',
                                              
                                              'threshold=FALSE',
                                              
                                              'product=FALSE',
                                              
                                              "-P","-J","replicates=5"),
                                       
                                       path='MaxEntOutput/Obstetrics_simplelandcover')


#Making point plots of no.of species in categories vs AUC values
#read in table
categories<-read.csv('categories.csv')
library("ggplot2")
summary(categories)
View(categories)
ggplot (categories, aes(x=No_sp, y=AUC)) + geom_point (aes(x=No_sp, y=AUC))
ggplot(categories, aes(x= No_sp, y= AUC, label=Categories))+
  geom_point() +geom_text(aes(label=Categories),hjust=0.3, vjust=0.5)

#Making point plots of no.of species in health care vs AUC values
#read in table
healthcare_groups<-read.csv('healthcare_groups.csv')
summary(healthcare_groups)
View(healthcare_groups)
ggplot (healthcare_groups, aes(x=No_sp, y=AUC)) + geom_point (aes(x=No_sp, y=AUC))
ggplot(healthcare_groups, aes(x= No_sp, y= AUC, label=Groups))+
  geom_point() +geom_text(aes(label=Groups),hjust=0.3, vjust=0.5)


#Making points plots of no. of records in categories vs AUC values
#read in table
cat_records<-read.csv('cat_records.csv')
summary(cat_records)
View(cat_records)
ggplot (cat_records, aes(x=No_records, y=AUC)) + geom_point (aes(x=No_records, y=AUC))
ggplot(cat_records, aes(x= No_records, y= AUC, label=Categories))+
  geom_point() +geom_text(aes(label=Categories),hjust=0.3, vjust=0.5)

#Making points plots of no. of records in healthcare groups vs AUC values
#read in table
groups_records<-read.csv('groups_records.csv')
summary(groups_records)
View(groups_records)
ggplot (groups_records, aes(x=No_records, y=AUC)) + geom_point (aes(x=No_records, y=AUC))
ggplot(groups_records, aes(x= No_records, y= AUC, label=Groups))+
  geom_point() +geom_text(aes(label=Groups),hjust=0.15, vjust=0.5)


#MaxEnt models for each of the malaria spp ####---- 
malspp<-levels(droplevels(mal$species))
malspp<-malspp[tapply(mal$species,droplevels(mal$species),length)>15]
malariaspp<-data.frame(species=malspp,AUC=rep(NA,times=length(malspp)),
                       bio16.contribution=rep(NA,times=length(malspp)),
                       bio4.contribution=rep(NA,times=length(malspp)),
                       gpw2000_30_sec.contribution=rep(NA,times=length(malspp)),
                       simplelc2000.contribution=rep(NA,times=length(malspp)),
                       bio16.permutation.importance=rep(NA,times=length(malspp)),
                       bio4.permutation.importance=rep(NA,times=length(malspp)),
                       gpw2000_30_sec.permutation.importance=rep(NA,times=length(malspp)),
                       simplelc2000.permutation.importance=rep(NA,times=length(malspp)))

for (i in 1:length(malspp)){
  maxent_mal<-maxent(ghana_envvars[[c(4,16,20,29)]],mal@coords[mal$species==malspp[i],],a=bg_BC,
                     factors="simplelc2000",
                     args=c('betamultiplier=2.5',
                            'linear=TRUE',
                            'quadratic=TRUE',
                            'hinge=FALSE',
                            'threshold=FALSE',
                            'product=FALSE'),#Currently using tuning parameters for whole malaria category
                     path=paste0('MaxEntOutput/MalariaSpecies/',malspp[i]))
  
  malariaspp$AUC[i]<-as.data.frame(t(maxent_mal@results))$Training.AUC
  malariaspp$bio16.contribution[i]<-as.data.frame(t(maxent_mal@results))$bio16.contribution
  malariaspp$bio4.contribution[i]<-as.data.frame(t(maxent_mal@results))$bio4.contribution
  malariaspp$gpw2000_30_sec.contribution[i]<-as.data.frame(t(maxent_mal@results))$gpw2000_30_sec.contribution
  malariaspp$simplelc2000.contribution[i]<-as.data.frame(t(maxent_mal@results))$simplelc2000.contribution
  malariaspp$bio16.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$bio16.permutation.importance
  malariaspp$bio4.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$bio4.permutation.importance
  malariaspp$gpw2000_30_sec.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$gpw2000_30_sec.permutation.importance
  malariaspp$simplelc2000.permutation.importance[i]<-as.data.frame(t(maxent_mal@results))$simplelc2000.permutation.importance
}
head(malariaspp)
write.csv(malariaspp,'MalariaSppMaxEntOutput.csv')

hist(malariaspp$AUC)
hist(malariaspp$bio16.contribution) #Repeat for all variables
hist(malariaspp$bio4.contribution)
hist(malariaspp$gpw2000_30_sec.contribution)
hist(malariaspp$simplelc2000.contribution)
hist(malariaspp$bio16.permutation.importance)
hist(malariaspp$bio4.permutation.importance)
hist(malariaspp$gpw2000_30_sec.permutation.importance)
hist(malariaspp$simplelc2000.permutation.importance)

#Making point plots of area of convex hull in categories vs AUC values
#read in table
cat_hull<-read.csv('cat_hull.csv')
library("ggplot2")
summary(cat_hull)
View(cat_hull)
ggplot (cat_hull, aes(x=convex_hull.km.sq, y=AUC)) + geom_point (aes(x=convex_hull, y=AUC))
ggplot(cat_hull, aes(x=convex_hull, y=AUC, label=Categories))+
  geom_point() +geom_text(aes(label=Categories),hjust=0.3, vjust=0.5)


#Making point plots of area of convex hull in healthcare groups vs AUC values
#read in table
groups_hull<-read.csv('groups_hull.csv')
library("ggplot2")
summary(groups_hull)
View(groups_hull)
ggplot (groups_hull, aes(x=convex_hull.km.sq, y=AUC)) + geom_point (aes(x=convex_hull.km.sq, y=AUC))
ggplot(groups_hull, aes(x=convex_hull.km.sq, y=AUC, label=Groups))+
  geom_point() +geom_text(aes(label=Groups),hjust=0.3, vjust=0.5)
>>>>>>> a7e0e2a518ffa33efe93e8256fabaa66dd9a8e36
