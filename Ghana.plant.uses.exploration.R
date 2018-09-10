########################################################################
#Exploring Ghana plant species ecosystem service dataset
#Stuart Smith
#21/08/2018
#########################################################################
#clear system & add package libraries
#rm(list=ls())
library(lattice)
library(MASS)
library(dplyr)
library(plyr)
library(lubridate)
library(data.table)
library(xlsx)
library(ggplot2)
##########################################################################
# Import Sarah's Ghana plant uses category dataset

# Importat data with file name
GhanaUses<-read.csv(file="Ghana.plants.uses.category.new.csv", sep=",",header=TRUE,strip.white=T)

# Basic exploration of dataset
names(GhanaUses)
dim(GhanaUses) # 1038   15 # rows/columns
str(GhanaUses) # Tells you which columns are factors, integers and numeric
head(GhanaUses)
tail(GhanaUses)
# Unique levels for each species entry
levels(GhanaUses$Species)
# Currently 519 
#[1] ""                              " Elaeis guineensis "          
#[3] " Euadenia eminens"             " Khaya anthotheca"            
#[5] " Moringa oleifera"             " Psidium guajava"             
#[7] " Vernonia ameygdalina "        "Abelmoschus esculentus"       
#[9] "Acacia gourmaensis"            "Acacia kameruneensis"         
#[11] "Acacia nilotica "              "Acacia senegalensis"          
#[13] "Acalypha ciliata "             "Acanthospermum"               
#[15] "Acanthospermum hispidum"       "Achyranthes aspera "          
#[17] "Adansonia digitata"            "Adansonia digitata "          
#[19] "Adenia cissampeloides"         "Adenia lobata"                
#[21] "Adiatum veneris"               "Afraegle paniculata"          
#[23] "Aframomum melegueta"           "Aframomum melegueta "         
#[25] "Afrostyrax lepidophyllus"      "Afzelia africana"             
#[27] "Afzelia africana "             "Agerantum conyzoides"         
#[29] "Ageratum conyzoides"           "Ageratum conyzoides\xa0"      
#[31] "Alafia multiflora"             "Albizia ferruginea "          
#[33] "Albizia zygia"                 "Alchornea cordifolia"         
#[35] "Alchornea cordifolia "         "Allium cepa"                  
#[37] "Allium cepa "                  "Allium sativum"               
#[39] "Allium sativum "               "Aloe vera"                    
#[41] "Aloe vera "                    "Alstonia boonei"              
#[43] "Alstonia boonei "              "Alternanthera pungens"        
#[45] "Alternanthera sessilis"        "Amaranthus graecizans"        
#[47] "Amaranthus hybridus"           "Amaranthus spinosus"          
#[49] "Amaranthus spinosus "          "Amaranthus viridis"           
#[51] "Amphimas pterocarpoides"       "Anacardium occidentale"       
#[53] "Ananas comosus"                "Ananas comosus "              
#[55] "Aningeria altissima"           "Annona muricata"              
#[57] "Annona reticulata"             "Annona senegalensis"          
#[59] "Annona senegalensis "          "Anogeissus leiocarpa"         
#[61] "Anogeissus leiocarpus"         "Anthocleista nobilis "        
#[63] "Antiaris africana"             "Antiaris toxicaria"           
#[65] "Antrocaryon micraster"         "Arachis hypogaea "            
#[67] "Argemone mexicana"             "Argemone mexicana "           
#[69] "Asparagus _agellaris"          "Aspilia africana"             
#[71] "Astraea lobata"                "Asystasia gangetica"          
#[73] "Azadirachta indica"            "Azadirachta indica "          
#[75] "Balanites aegyptiaca"          "Balanites aegyptiacus"        
#[77] "Balanites aegyptica"           "Bambusa vulgaris"             
#[79] "Bambusa vulgaris "             "Baphia nitida"                
#[81] "Baphia nitida "                "Barleria cristata"            
#[83] "Berlina confusa"               "Berlinia sp. "                
#[85] "Bertiera racemosa"             "Bidens pilosa"                
#[87] "Bidens pilosa "                "Biophytum petersianum "       
#[89] "Blighia sapida"                "Blighia unijugata"            
#[91] "Blighia welwitschii"           "Boerhavia diffusa"            
#.... etc....

# Sarah you need to go through this list and make sure each entry here is an individual species
# There are many examples where these is a space after the species name
# For example [80] and [81] are both Baphia nitida - but one has a space after the nitida...
# Therefore R thinks it is a unique species...

levels(GhanaUses$Category)
#1] ""                   "Agriculture"        "Construction"       "Culture"           
#[5] "Energy"             "Food and nutrition" "Health care"        "Social"            
#[9] "Water purification"

# Explore number of species in relaiton to a category
names(GhanaUses)
count(GhanaUses, c("Category")) # Number of entries in each category
#            Category freq
#1                       8 # Obviously some issues here - some have no category? 
#2        Agriculture   56
#3       Construction   18
#4            Culture   14
#5             Energy   22
#6 Food and nutrition   33
#7        Health care  862
#8             Social   24
#9 Water purification    1

count(GhanaUses, c("Category", "Species")) # Number of entries per species in each category

# Convert to dataframe and plot in ggplot
GhanaCount<-count(GhanaUses, c("Category")) 
colnames(GhanaCount)[2]<-"Number_records"
ggplot(GhanaCount, aes(y=Number_records, x=Category))+geom_bar(stat = "identity")
# Most of records are for health care...but this is total number of records not number of species
# Maximum species records ~590 (including issues above)...not >750 spp

# Number of species within each category - need to remove duplicates
# Create a unique code for each species - category combination
GhanaUses$spp_cat<-as.factor(with(GhanaUses, paste(Species,Category, sep="_")))
levels(GhanaUses$spp_cat)

dim(GhanaUses[duplicated(GhanaUses$spp_cat), ]) #404 duplicates
GhanaUses2<-GhanaUses[!duplicated(GhanaUses$spp_cat), ]
dim(GhanaUses) # 1038   16
dim(GhanaUses2) # 634  16

# Plot number of records again - without duplicate species entries within each category
GhanaCount2<-count(GhanaUses2, c("Category")) 
colnames(GhanaCount2)[2]<-"Number_records_per_spp"
ggplot(GhanaCount2, aes(y=Number_records_per_spp, x=Category))+geom_bar(stat = "identity")
# Now max < 500 - correct...

# Health care outstrips everything else - so perhaps also remove health care and present other categories
GhanaCount2hc<-GhanaCount2[GhanaCount2$Category!="Health care",]
ggplot(GhanaCount2hc, aes(y=Number_records_per_spp, x=Category))+geom_bar(stat = "identity")
# Agriculture second most records for enthnobotanical uses

#########################################################################
# END #
#########################################################################
