#Ghana plants
require(data.table)
require(raster)
require(dismo)
require(rasterVis)
require(ENMeval)

#Get data
ghanaplants<-fread('occurrence.txt')

head(ghanaplants)
summary(ghanaplants)

#Number of species
length(levels(as.factor(ghanaplants$species)))

#Drop records missing species
ghana_species<-ghanaplants[ghanaplants$species!="",]

#Make spatial point data frame
ghana_species_pts<-SpatialPointsDataFrame(cbind(ghana_species$decimalLongitude,ghana_species$decimalLatitude),ghana_species,
                                          proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

#Plot map of Ghana regions
ghanamap<-getData('GADM',country='GHA',level=1)
plot(ghanamap)
points(ghana_species_pts,pch=16,cex=0.1,col='red') #Species occurence points

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
lc1975<-raster('S:\\Supervision\\Sarah\\Landcover maps\\west_africa_land-use_land-cover_1975_2km\\swa_1975lulc_2km.tif')
ghanat<-spTransform(ghanamap,crs(lc1975))
ghanalc1975<-mask(crop(lc1975,ghanat),ghanat)
plot(ghanalc1975)

#Choose a species #Pterocarpus erinaceus
pteeri<-ghana_species_pts_insidemap[ghana_species_pts_insidemap$species=='Pterocarpus erinaceus',]

#Basic MaxEnt with two predictors
m1<-maxent(ghana1[[c(4,16)]],pteeri)
m1#View output as html
#Predict habitat suitability for the species
p1<-predict(m1,ghana1)
rtheme<-rasterTheme(region=brewer.pal(9,'Blues'))
levelplot(p1,par.settings='rtheme',margin=F,main='Pterocarpus erinaceus',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


# More complex and proper application of maxent ---------------------------

#Extensions
#Bias file (to take account of sampling bias)
densgbifrecs <- kde2d(ghana_species_pts_insidemap@coords[,1],ghana_species_pts_insidemap@coords[,2],n=100)#Default bandwidths
densgbifrecs_ras <- raster(densgbifrecs)
levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.polygons(ghanamap))

bg_BC<-randomPoints(densgbifrecs_ras,2000,prob=T) #Weighted selection of background by biasfile
levelplot(densgbifrecs_ras,scales=list(draw=F),margin=F,par.settings='rtheme')+
  layer(sp.points(Sp.Points(bg_BC)))

#Tuning #Note this takes a while to estimate
#Use block or checkerboard methods for tuning spatially variable data
tuneparameters<-ENMevaluate(pteeri@coords,ghana1[[c(4,16)]],method="block",bg.coords=bg_BC)
tuneparameters@results
tuneparameters@results[which.min(tuneparameters@results$AICc),]#Can use other parameters to select best method

#With response curves, tuned parameterisation and bias file
#-P turns on response curves
#-J turns on jackknifing
#Betamultiplier to 4 and threshold features off
#a gives background data
maxentfull<-maxent(ghana1[[c(4,16)]],pteeri,a=bg_BC,args=c('betamultiplier=1.5','threshold=FALSE','product=FALSE',"-P","-J"))
maxentfull
maxentfull@results #Note AUC
pfull<-predict(maxentfull,ghana1)                   
levelplot(pfull,par.settings='rtheme',margin=F,main='Pterocarpus erinaceus',scales=list(draw=F),xlab=NULL,ylab=NULL)+
  layer(sp.polygons(ghanamap))


#Write maxent results to file which you can later read in to make response curves etc in R
maxentwrite<-maxent(ghana1[[c(4,16)]],pteeri,a=bg_BC,args=c('betamultiplier=1.5','threshold=FALSE','product=FALSE',"-P","-J"),
                    path='S:\\Supervision\\Sarah\\ExampleMaxEnt')


