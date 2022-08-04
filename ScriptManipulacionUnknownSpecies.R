
### Cockburn Unknown Species Treatment ###

#set working directory

setwd("C:/Maestría/Proyecto/ManipulacionBaseDatos")

#load packages
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(rvest)

#read data
cockburn<-read.csv("Cockburn.Taxonomy.csv", header = T)
jetz<-read.csv("Jetz-Master-Taxonomy.csv", header = T)
birds.world<-read.csv("HBW_Species_URL_list.csv", header = F)

#filter cockburn unknown species
cockburn.unknown<- cockburn %>%
  mutate(., pc.known = ifelse(pc.known == "Unknown ", "Unknown", pc.known)) %>%
  filter(., pc.known == "Unknown")

#filter cockburn unknown species on jetz tree
jetz.unknown<- jetz %>%
  filter(., TipLabel %in% cockburn.unknown$species)

#clean birds of the world URL list
birds.world.tidy<- birds.world %>%
  select(., V1) %>%
  filter(., V1 != "[" & V1 != "]" & V1 != "  [" & V1 != "  ]") 
  
#Create matrix of unknown species in Birds of the World
birds.world.unknown<- matrix(NA,nrow(jetz.unknown),2)
birds.world.unknown[,1]<-jetz.unknown$Scientific
for(i in 1:nrow(jetz.unknown)){
  j<-str_which(birds.world.tidy$V1, jetz.unknown$Scientific[i])
  if(length(j == 1)){
    birds.world.unknown[i,2]<- birds.world.tidy$V1[j+1] #birds.world.unknown[i,2]<- j+1
  } else {
    birds.world.unknown[i,2]<- NA
  }
} 

#create matrix of unknown species with URL adress to wrap
to.scrap<- as.data.frame(birds.world.unknown) %>%
  rename(., URL.adress = V2, Species = V1) %>%
  filter(.,  !is.na(URL.adress)) %>%
  mutate(., URL.repro = paste(URL.adress, "#repro", sep = "")) %>%
  mutate(., URL.breeding = str_replace(URL.adress, "introduction", "breeding")) %>%
  mutate(., breeding.page = NA) %>%
  mutate(., keywords = NA)

#define keywords to search
keywords<-c("male","sexes","adult","parents","pair","co-operative","cooperative","helper",
            "communal","family group","relatives","geothermal","parasite")  #vector of keywords to match
OtherSections<-c('"idsum"','"sys"','"distrib"','"hab"', '"food"', '"sounds"', '"conserv"')  #vector of not "repro" sections

#scraping species loop
for(i in 1:nrow(to.scrap)){
  Sys.sleep(3)
  url.ad<-str_trim(to.scrap$URL.breeding[i])
  pgsession2<-session(url.ad)
  pgform2<-html_form(pgsession2)[[1]]
  filled_form<-html_form_set(pgform2, user="311239438", pass="E-vUR:%8")
  session_submit(pgsession2, filled_form)
  page<-session_jump_to(pgsession2, pgsession2$url)
  sections<- page %>% 
    html_elements("section")
  find.sec<-toString(sections) %>%
    str_detect(OtherSections)
  if(any(find.sec) == T){
    to.scrap$breeding.page[i]<-"#repro"
    find.repro<-grep('"repro"',sections)
    repro<-sections[find.repro] %>%
      html_text()
    key.repro<-rep(NA,length(repro))
    for(j in 1:length(repro)){
      key.repro[j]<-any(str_detect(repro[j], regex(keywords, ignore_case = T)))
    }
    to.scrap$keywords[i]<-any(key.repro)
  } else{
    to.scrap$breeding.page[i]<-"/breeding"
    breeding<- sections %>%
      html_text()
    key.breed<-rep(NA,length(breeding))
    for(k in 1:length(breeding)){
      key.breed[k]<-any(str_detect(breeding[k], regex(keywords, ignore_case = T)))
    }
    to.scrap$keywords[i]<-any(key.breed)
  }
}
Sys.time()

#export data frame
write.csv(to.scrap,"Unknown_Species_Scraped.csv")

#check manually for information on HBW and classify when possible


### Miscelaneous ##########################################################################################################################

#scrapping try with 1 species

#Quiscalus mexicanus
a<-"https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/grtgra/cur/breeding"
#a<-"https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/grtgra/cur/introduction"

a<-"https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/sleshe1/cur/introduction"
pgsession2<-session(a) #inspect object structure
pgform2<-html_form(pgsession2)[[1]]
filled_form<-html_form_set(pgform2, user="311239438", pass="E-vUR:%8")
session_submit(pgsession2, filled_form)
page<-session_jump_to(pgsession2, pgsession2$url)
sections<- page %>% 
  html_elements("sections") %>%
  html_text()

headers<-html_elements(page, "h2")
divs<-html_elements(page, "div")
p.text<-html_elements(page, "p")  #p for paragraph
p.text<-html_text(p.text)#View(p.text)
p.text[11]  #p 11 corresponds to breeding info
p.text[3]
str_detect(p.text[3], "incubation")

#intento con secciones
sections<- page %>% 
  html_elements("section")
index<-which(str_detect(sections, "repro"))
if(length(index) == 0){ #breeding
  page.repro<- page %>%
    html_elements("p") %>%
    html_text()
  str_detect(page.repro, regex(keywords, ignore_case = T))
} else{
  page.repro<-page2[index] %>%
    html_elements("p") %>%
    html_text()
  str_detect(page.repro, regex(keywords, ignore_case = T))
}

#especie /breeding
a<-"https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/empgoo/cur/breeding"
https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/royter1/cur/breeding
https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/leasan/cur/breeding
https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/grests1/cur/breeding
https://birdsoftheworld-org.pbidi.unam.mx:2443/bow/species/purswa3/cur/breeding


#scrapping species tries
keywords<-c("male","sexes","adult","parents","pair","co-operative","cooperative","helper",
            "communal","family group","relatives","geothermal","parasite")  #vector of keywords to match
for(i in 1:nrow(to.scrap)){
  url.ad<-str_trim(to.scrap[i,4])
  pgsession2<-session(url.ad)
  pgform2<-html_form(pgsession2)[[1]]
  filled_form<-html_form_set(pgform2, user="311239438", pass="E-vUR:%8")
  session_submit(pgsession2, filled_form)
  page<-session_jump_to(pgsession2, pgsession2$url)
  p.text<-html_elements(page, "p")
  p.text<-html_text(p.text)
  keywords.repro<-str_detect(p.text[11], regex(keywords, ignore_case = T))  #paragraph 11 correspond to breeding info (#repro)
  if(is.na(p.text[11]) == T){
    to.scrap$breeding.page[i]<-"/breeding"
    keywords.breed<-str_detect(p.text[3], regex(keywords, ignore_case = T)) #paragraph 3 correspond to breeding info (/breeding)
    to.scrap$keywords[i]<-any(keywords.breed)
  } else {
    to.scrap$breeding.page[i]<- "#repro"
    to.scrap$keywords[i]<-any(keywords.repro)
  }
}

#LOOP CORREGIDO
for(i in 1:6){
  url.ad<-str_trim(to.scrap[i,4])
  pgsession2<-session(url.ad)
  pgform2<-html_form(pgsession2)[[1]]
  filled_form<-html_form_set(pgform2, user="311239438", pass="E-vUR:%8")
  session_submit(pgsession2, filled_form)
  page<-session_jump_to(pgsession2, pgsession2$url)
  sections<- page %>% 
    html_elements("section")
  index<-str_detect(sections, '"repro"')
  if(any(index) == F){ #breeding
    page.repro<- page %>%
      html_elements("p") %>%
      html_text()
    to.scrap$breeding.page[i]<-"/breeding"
    keywords.breed<-str_detect(page.repro, regex(keywords, ignore_case = T)) 
    to.scrap$keywords[i]<-any(keywords.breed)
  } else{
    page.repro<-sections[index == T] %>%
      html_elements("p") %>%
      html_text()
    to.scrap$breeding.page[i]<-"#repro"
    keywords.repro<-str_detect(page.repro, regex(keywords, ignore_case = T))
    to.scrap$keywords[i]<-any(keywords.repro)
  }
}


for(i in 1:20){
  url.ad<-str_trim(to.scrap$URL.breeding[i])
  pgsession2<-session(url.ad)
  pgform2<-html_form(pgsession2)[[1]]
  filled_form<-html_form_set(pgform2, user="311239438", pass="E-vUR:%8")
  session_submit(pgsession2, filled_form)
  page<-session_jump_to(pgsession2, pgsession2$url)
  sections<- page %>% 
    html_elements("section")
  find.sec<-toString(sections) %>%
    str_detect(OtherSections)
  if(any(find.sec) == T){
    to.scrap$breeding.page[i]<-"#repro"
    find.repro<-str_detect(sections, '"repro"')
    repro<-sections[find.repro == T] %>%
      html_elements("p") %>%
      html_text()
    keywords.repro<-str_detect(repro, regex(keywords, ignore_case = T))
    to.scrap$keywords[i]<-any(keywords.repro)
  } else{
    to.scrap$breeding.page[i]<-"/breeding"
    breeding<- page %>%
      html_elements("p") %>%
      html_text()
    keywords.breed<-str_detect(breeding, regex(keywords, ignore_case = T)) 
    to.scrap$keywords[i]<-any(keywords.breed)
  }
}

#loop for searching in more than one paragraph
find.paragraph<-rep(NA,length(breeding))
for(i in 1:length(breeding)){
  j<-any(str_detect(breeding[i], regex(keywords, ignore_case = T)))
  find.paragraph[i]<-j
}

#INTENTO MIL CON LOOP
for(i in 671:nrow(to.scrap)){
  Sys.sleep(3)
  url.ad<-str_trim(to.scrap$URL.breeding[i])
  pgsession2<-session(url.ad)
  pgform2<-html_form(pgsession2)[[1]]
  filled_form<-html_form_set(pgform2, user="311239438", pass="E-vUR:%8")
  session_submit(pgsession2, filled_form)
  page<-session_jump_to(pgsession2, pgsession2$url)
  sections<- page %>% 
    html_elements("section")
  find.sec<-toString(sections) %>%
    str_detect(OtherSections)
  if(any(find.sec) == T){
    to.scrap$breeding.page[i]<-"#repro"
    find.repro<-str_detect(sections, '"repro"')
    repro<-sections[find.repro == T] %>%
      html_elements("p") %>%  #eliminar?
      html_text()
    key.repro<-rep(NA,length(repro))
    for(j in 1:length(repro)){
      key.repro[j]<-any(str_detect(repro[j], regex(keywords, ignore_case = T)))
    }
    to.scrap$keywords[i]<-any(key.repro)
  } else{
    to.scrap$breeding.page[i]<-"/breeding"
    breeding<- page %>%
      html_elements("p") %>%
      html_text()
    key.breed<-rep(NA,length(breeding))
    for(k in 1:length(breeding)){
      key.breed[k]<-any(str_detect(breeding[k], regex(keywords, ignore_case = T)))
    }
    to.scrap$keywords[i]<-any(key.breed)
  }
}
Sys.time()

#with `grep` 
for(i in 1:20){
  url.ad<-str_trim(to.scrap$URL.breeding[i])
  pgsession2<-session(url.ad)
  pgform2<-html_form(pgsession2)[[1]]
  filled_form<-html_form_set(pgform2, user="311239438", pass="E-vUR:%8")
  session_submit(pgsession2, filled_form)
  page<-session_jump_to(pgsession2, pgsession2$url)
  sections<- page %>% 
    html_elements("section")
  find.sec<-toString(sections) %>%
    grep(OtherSections,.)
  if(any(find.sec) == T){
    to.scrap$breeding.page[i]<-"#repro"
    find.repro<-grep('"repro"', sections)
    repro<-sections[find.repro] %>%
      html_elements("p") %>%
      html_text()
    keywords.repro<-grep(regex(keywords, ignore_case = T), repro)
    to.scrap$keywords[i]<-any(keywords.repro)
  } else{
    to.scrap$breeding.page[i]<-"/breeding"
    breeding<- page %>%
      html_elements("p") %>%
      html_text()
    keywords.breed<-grep(regex(keywords, ignore_case = T), breeding) 
    to.scrap$keywords[i]<-any(keywords.breed)
  }
}

for(i in 1:nrow(jetz.unknown)){
  birds.world.unknown[i,1]<- grep(jetz.unknown$Scientific[i],a)
}

for(i in 1:nrow(jetz.unknown)){
  if(grep(jetz.unknown$Scientific[i],a) > 0){
    birds.world.unknown[i,1]<- grep(jetz.unknown$Scientific[i],a)
  } else {
    birds.world.unknown[i,1]<- NA
  }
}

for(i in 1:nrow(jetz.unknown)){
  if(which(!is.na(str_match(a, jetz.unknown$Scientific[i]))) == 1){
    birds.world.unknown[i,1]<- "keep"
  } else {
    birds.world.unknown[i,1]<- "drop"
  }
}

#Exploracion especies /Breeding
div.SectionMenu-section:nth-child(1) > ul:nth-child(1) > li:nth-child(10) > a:nth-child(1)  #CSS selector
Breeding  #html interno
<a href="/bow/species/bkhgro/cur/breeding" class="">Breeding</a>  #html externo
/html/body/div[2]/main/div[4]/div/div/div[1]/nav/div/div[1]/ul/li[10]/a #xpath


#Exploracion especies #repro
<section aria-labelledby="repro" class="u-stack-lg u-text-4-loose u-article">
  <div class="SectionHeading  SectionHeading--ruleAbove">
  <div class="SectionHeading-heading">
  <h2 id="repro">Breeding</h2>
  </div>
  </div>
  Apr-Jun. Nest a cup placed 0·6-1·5 m from ground in sapling, thorn bush or bamboo clump. Clutch 3 eggs, pale blue, spotted and streaked with dark red. No other information.</section>