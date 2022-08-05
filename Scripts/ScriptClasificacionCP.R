#libraries
library(tidyr)
library(dplyr)

#working directory
setwd("C:/Maestría/Proyecto/ManipulacionBaseDatos")

#read and filter data
cockburn<-read.csv("Cockburn.Taxonomy.csv") #Cockburn
cockburn2<-cockburn %>% select("species","pc.known")        #species with known parental care
cockburn.known<-filter(cockburn2, pc.known != "Unknown" & pc.known != "Unknown ")

jetz<-read.csv("Jetz-Master-Taxonomy.csv", header = T)  #Jetz
jetz2<-jetz %>% select("TipLabel")

#matching
to.rename<-filter(cockburn.known, !(species %in% jetz2$TipLabel))

#search for synonyms on IOC Wolrd Bird List (outside R)

#import list of synonyms
synonyms<-read.csv("EquivalenciasEspecies.csv", header = T)

#keeped species
to.keep<-filter(to.rename, species %in% synonyms$bs4)

#excluded species
to.drop<-filter(to.rename, !(species %in% synonyms$bs4))
write.csv(to.drop, "CockburnNoSynonym.csv")

#removing non matching species from data
cockburn.trimmed<-filter(cockburn.known, !(species %in% to.rename$species))

#renaming
cockburn.renamed<- to.keep
cockburn.renamed<-cockburn.renamed[order(match(cockburn.renamed$species, synonyms$bs4)),]   ###                                 
cockburn.renamed$species<-synonyms$Jetz.Master.Taxonomy

#merge final data frame
cockburn.merged<-rbind(cockburn.trimmed, cockburn.renamed)

#reclassifying
final.data<-cockburn.merged %>%
  mutate(pc.known = replace(pc.known, pc.known == "Female only ", "Female only")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Female only", "F")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Male only", "M")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Cooperation", "C")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Suspected", "C")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Pair", "P")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Group", "P")) %>%  
  mutate(pc.known = replace(pc.known, pc.known == "Occasional ", "Occasional")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Occasional", "P")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Captivity", "P")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Geothermal", "N")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Brood parasite", "N"))

group<-filter(cockburn, pc.known == "Group")
group<-subset(group,pc.inferred == "Female only ")
captivity<-subset(cockburn, pc.known == "Captivity")
captivity<-subset(captivity, pc.inferred == "Cooperation")
final.data$pc.known[final.data$species %in% captivity$species]<-"C"
final.data$pc.known[final.data$species %in% group$species]<-"F"
write.csv(final.data, "ParentalCareData.csv")


#check non conventional species in cockburn
non.conventional<- cockburn.merged
non.conventional$synonym<-rep(NA, length(non.conventional$species))
non.conventional$synonym[which(non.conventional$species %in% synonyms$Jetz.Master.Taxonomy)]<-synonyms$bs4
non.conventional<- non.conventional %>%
  filter(pc.known == "Captivity" | pc.known == "Group" | pc.known == "Occasional" | pc.known == "Occasional " | pc.known == "Suspected") %>%
  group_by(pc.known) %>%
  arrange(.by_group = T)
#export non conventional data to compare with HBW
write.csv(non.conventional, "CockburnNonConventionalCare.csv")

#compare data on HBW (outside R)

#compare unknown species on HBW ("ScriptManipulacionUnknownSpecies.R")

#merge cockburn and HBW data
library(stringr)
library(dplyr)
library(tidyr)

cockburn<-read.csv("ParentalCareData.csv", header = T, sep = ",") %>%
  select("species","pc.known")
hbw<-read.csv("HBW_Reclassified_species.csv", header = T, sep = ",") %>% 
  select("Species","Parental.care.type") %>%
  rename("pc.known" = "Parental.care.type") %>%
  rename("species" = "Species") %>%
  filter(., !is.na(pc.known))%>%
  mutate(pc.known = replace(pc.known, pc.known == "Geothermal", "N")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Female only", "F")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Male only", "M")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Biparental", "P")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Occasional", "P")) %>%
  mutate(pc.known = replace(pc.known, pc.known == "Cooperative", "C")) %>%
  mutate(species = str_replace(species, "[:blank:]","_"))
  
merged<- bind_rows(cockburn,hbw)

#write.csv(merged, "ParentalCare.FullData.csv")

#edit .csv for using on BayesTraits and RevBayes analyses (outside R)

#general dataframe

sequenced<-read.csv("ParentalCare.SeqData.txt", header = F, sep = "") #obtained on BirdTree.org 

general.df<-read.csv("Jetz-Master-Taxonomy.csv", sep = ",") %>%
  filter(., TipLabel %in% merged$species) %>%
  select("TipLabel","BLFamilyLatin","IOCOrder") %>%
  rename("species"="TipLabel", "family"="BLFamilyLatin","order"="IOCOrder") %>%
  mutate(source = if_else(species %in% cockburn$species, "Cockburn", "HBW")) %>%
  mutate(is.sequenced = if_else(species %in% sequenced$V1, "yes", "no")) %>%
  mutate(pc.mode = merged$pc.known[order(match(merged$species,species))])
  
#write.csv(general.df,"GeneralDataFrame.csv") #export




### MISCELANEOUS ###############################################################

#compare with old data
old.data<-read.csv("BaseCuidadoBayes5CP", sep = "", header = F)
colnames(old.data)<-c("species","pc.known")
final.data<-final.data[order(match(final.data$species, old.data$species)),]
all.equal(final.data, old.data)

#matching
to.rename<-filter(care.data, !(species %in% jetz2$TipLabel))
#search for synonyms on IOC Wolrd Bird List (outside R)
#import list of synonyms
synonyms<-read.csv("EquivalenciasEspecies.csv", header = T)
#keeped species
to.keep<-filter(to.rename, species %in% synonyms$bs4)
#excluded species
#to.drop<-filter(to.rename, !(species %in% synonyms$bs4))
#removing non matching species from data
care.data2<-filter(care.data, !(species %in% to.rename$species))
#renaming
cockburn.renamed<- to.keep
cockburn.renamed<-cockburn.renamed[order(match(cockburn.renamed$species, synonyms$bs4)),]   ###                                 
cockburn.renamed$species<-synonyms$Jetz.Master.Taxonomy
#merge final data frame
final.data<-rbind(care.data2, cockburn.renamed)
