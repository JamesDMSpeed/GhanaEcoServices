#Ghana plants analysis

#Use Ghana.plant.uses.exploration.R to make Ghana Plant Uses dataset (correcting species names etc)

rm(list=ls())

#Ghana plant uses dataset with names matching to gbif
GhanaPlantUses<-read.csv('FinalDataSpeciesNames.csv',header=T)
head(GhanaPlantUses)
summary(GhanaPlantUses)

#Gbif data
gbifrecs<-fread('occurrence. Sarah.txt') #Citation for this one GBIF.org (07 November 2017) GBIF Occurrence Download https://doi.org/10.15468/dl.n6b02z 
head(gbifrecs)
summary(gbifrecs)
