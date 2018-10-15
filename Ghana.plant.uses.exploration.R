########################################################################
#Exploring Ghana plant species ecosystem service dataset
#Stuart Smith
#21/08/2018
#########################################################################
#clear system & add package libraries
rm(list=ls())
library(lattice)
library(MASS)
library(dplyr)
library(plyr)
library(lubridate)
library(data.table)
library(xlsx)
library(ggplot2)
library(taxize)
library(Taxonstand)
##########################################################################
# Import Sarah's Ghana plant uses category dataset

# Import data with file name

GhanaUses<-read.csv(file="Ghana.plants.uses.category.new.csv", sep=",",header=TRUE,strip.white=T)


# Basic exploration of dataset
names(GhanaUses)
dim(GhanaUses) # 1030   17 # rows/columns
str(GhanaUses) # Tells you which columns are factors, integers and numeric
head(GhanaUses)
tail(GhanaUses)

# Levels for each species entry
levels(GhanaUses$Species) # 398 species

# Levels for each species entry
levels(GhanaUses$Category) # 8 categoryies

# Explore number of species in relaiton to a category
names(GhanaUses)
count(GhanaUses, c("Category")) # Number of entries in each category - incls duplicate spp
#            Category freq
#1        Agriculture   56
#2       Construction   18
#3            Culture   14
#4             Energy   22
#5 Food and nutrition   33
#6        Health care  862
#7             Social   24
#8 Water purification    1

# Include species in count and can see some species are used for different purpses in each category
count(GhanaUses, c("Category", "Species")) # Number of entries per species in each category

# Convert to dataframe and plot in ggplot
GhanaCount<-count(GhanaUses, c("Category")) 
colnames(GhanaCount)[2]<-"Number_records_per_category"
ggplot(GhanaCount, aes(y=Number_records_per_category, x=Category))+geom_bar(stat = "identity") +theme_classic()
# Most of records are for health care...but this is total number of records not number of species
# Maximum species records ~590 (including issues above)...not >750 spp

# Number of species within each category - need to remove duplicates
# Create a unique code for each species - category combination
GhanaUses$spp_cat<-as.factor(with(GhanaUses, paste(Species,Category, sep="_")))
levels(GhanaUses$spp_cat)

# Nicer way of doing this as losing info...
dim(GhanaUses[duplicated(GhanaUses$spp_cat), ]) #404 duplicates
GhanaUses2<-GhanaUses[!duplicated(GhanaUses$spp_cat), ]
dim(GhanaUses) # 1038 # Full data
dim(GhanaUses2) # 519  # Without duplicate species in each category

# Plot number of records again - without duplicate species entries within each category
GhanaCount2<-count(GhanaUses2, c("Category")) 
#Category freq
#1        Agriculture   42
#2       Construction   18
#3            Culture   13
#4             Energy   21
#5 Food and nutrition   26
#6        Health care  374 # Total 398
#7             Social   24
#8 Water purification    1

# Barplot of species per category
colnames(GhanaCount2)[2]<-"Unique_spp_per_category"
Eplot<-ggplot(GhanaCount2, aes(y=Unique_spp_per_category, x=Category))
Eplot<-Eplot+geom_bar(stat = "identity") +theme_bw()
Eplot
# Now max < 500 - correct...

# Isseue #5 GIThub -
# Number of Gbif records for each ecosystem service category
# Box plots
names(GhanaUses)
GbRec<-ggplot(GhanaUses, aes(y=No..of.occurrences.in.GBIF, x=Category))
GbRec<-GbRec+geom_jitter(size=2) # geom_point()
GbRec<-GbRec+geom_boxplot() 
GbRec<-GbRec+theme_bw()
GbRec # Not unique for species

# Plot the total records per category - without duplicate species
# Box plots
names(GhanaUses2) # No duplicate species in each category
TotalGbifR<-aggregate(No..of.occurrences.in.GBIF~Category,GhanaUses2,sum)

GbTot<-ggplot(TotalGbifR, aes(y=No..of.occurrences.in.GBIF, x=Category))
GbTot<-GbTot+geom_bar(stat = "identity", colour="black",fill=NA) 
#GbTot<-GbTot+scale_y_continuous(trans='log10') # Need to pluse 1 if logging
GbTot<-GbTot+theme_bw()
GbTot # Not unique for species

# Combine plots
#library(grid)
library(egg)
library(ggpubr)
p1<-egg::ggarrange(GbRec,GbTot,ncol=2) #common.legend = T,legend="bottom")
#p1<-ggarrange(GbRec,GbTot,ncol=2)
p1

# Export graphs
ggsave("Number_gbif_occurences.jpeg",width= 26, height = 18,units ="cm",dpi = 600, limitsize = TRUE)

# Exploring species distribution - dominant species
# Plot number gbif records by species in each category # Look for species with larger records
GhanaUses$No..of.occurrences.in.GBIF<-as.numeric(GhanaUses$No..of.occurrences.in.GBIF)
Eplot<-ggplot(GhanaUses2, aes(y=No..of.occurrences.in.GBIF,x=reorder(Species, -No..of.occurrences.in.GBIF)))
Eplot<-Eplot+geom_density(stat="identity")
#Eplot<-Eplot+facet_wrap(~Category) # facet_gird()
Eplot






# Health care outstrips everything else - so perhaps also remove health care and present other categories
GhanaCount2hc<-GhanaCount2[GhanaCount2$Category!="Health care",]
Eplot<-ggplot(GhanaCount2, aes(y=Unique_spp_per_category, x=Category))
Eplot<-Eplot+geom_bar(stat = "identity")
Eplot
# Agriculture second most records for enthnobotanical uses

#############################################################################
#Matching species to GBIF data

#GBIF records
#gbifrecs<-fread('occurrence. Sarah.txt')

#Read directly from zip directory
gbifrecs<-read.delim(unz('GBIFdownload_Oct2018.zip','occurrence.txt'),sep='\t',quote="",dec='.',header=T)

head(gbifrecs)
summary(gbifrecs)

#Use match
match(GhanaUses$Species,gbifrecs$species)
#Use %in% to identify species without GBIF matches
GhanaUses$Species[GhanaUses$Species%in%gbifrecs$species==F]
#List these
unmatchedspp<-levels(droplevels((GhanaUses$Species[GhanaUses$Species%in%gbifrecs$species==F])))
unmatchedspp

#Check matches using the names according to gbif column
GhanaUses$Species.name.in.GBIF[GhanaUses$Species.name.in.GBIF%in%gbifrecs$species==F]
unmatchedspp_gbifname<-levels(droplevels((GhanaUses$Species.name.in.GBIF[GhanaUses$Species.name.in.GBIF%in%gbifrecs$species==F])))
unmatchedspp_gbifname #18 species without GBIF data. Plus many records where there in 'None' writting in GBIF name column

#Look for alternative names for these
#Using Taxonstand rather than taxize as this seems to be more robust for plants
#tnrs(unmatchedspp_gbifname)
TPL(unmatchedspp_gbifname)
taxonstandlookup<-TPL(unmatchedspp)
#Check corrected names
namecheck<-data.frame(cbind(input=taxonstandlookup$Taxon, newname=paste(taxonstandlookup$New.Genus,taxonstandlookup$New.Species)))
namecheck

match(namecheck$newname,gbifrecs$species) # We find a match for many of these. 
#not all species are matched, but some are species that are useful but may not be present in Ghana (natives) like Allium sativum and Aloe vera

#Next, to make a column with the names that match to GBIF including the taxonstand names

#Check all names with TPL
checkallnames<-TPL(GhanaUses$Species)
#Write as a column
GhanaUses$CheckedNames<-paste(checkallnames$New.Genus,checkallnames$New.Species)
GhanaUses[GhanaUses$CheckedNames%in%gbifrecs$species==F,c(1,2,19)]

#Summarize number of records per species in gbifdataset
gbifrecsperspp<-aggregate.data.frame(gbifrecs$species,by=list(gbifrecs$species),length)
names(gbifrecsperspp)<-c('gbifspeciesname','numberrecords')

#Match number of records to species name
GhanaUses$NumberGBIFrecords<-gbifrecsperspp$numberrecords[match(GhanaUses$Species.name.in.GBIF,gbifrecsperspp$gbifspeciesname)]
View(GhanaUses)#This looks good

#Checking the species that did not match
GhanaUses[is.na(GhanaUses$NumberGBIFrecords),]
match(GhanaUses$CheckedNames[is.na(GhanaUses$NumberGBIFrecords)],gbifrecs$species)#Some of these are in GBIF under checked names

GhanaUses$ConfirmedSppNames<-as.character(GhanaUses$Species.name.in.GBIF)
GhanaUses$ConfirmedSppNames[which(is.na(GhanaUses$NumberGBIFrecords))]<-as.character(GhanaUses$CheckedNames[which(is.na(GhanaUses$NumberGBIFrecords))])

GhanaUses$GBIFrecswithconfirmendnames<-gbifrecsperspp$numberrecords[match(GhanaUses$ConfirmedSppNames,gbifrecsperspp$gbifspeciesname)]

#Clean a bit to remove superflous species names and counts
FinalDataSet<-subset(GhanaUses,select=c('ConfirmedSppNames','Category','Use','Group','Location','Authors','Title','Journal','Volume','Pages','GBIFrecswithconfirmendnames'))
write.csv(FinalDataSet,'FinalDataSpeciesNames.csv')

#########################################################################
# END #
#########################################################################
