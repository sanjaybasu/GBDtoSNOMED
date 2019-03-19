# GBD 2017 to SNOMED CT US edition 2019
# basus@stanford.edu

install.packages('tidyverse')
install.packages('readxl')
install.packages('httr')
install.packages('icd')
install.packages('devtools')
library(devtools)
devtools::install_github("wtcooper/icdcoder")

library(tidyverse)
library(readxl)
library(httr)
library(icd)
library(icdcoder)
url1<-'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2017_ICD_CAUSE_MAPS.zip'
GET(url1, write_disk(tf <- tempfile(fileext = ".zip")))
unzip(zipfile=tf, files = "IHME_GBD_2017_ICD_CAUSE_MAP_CAUSES_OF_DEATH_Y2018M11D08.XLSX", exdir=".")
gbdtoicd <- read_excel("IHME_GBD_2017_ICD_CAUSE_MAP_CAUSES_OF_DEATH_Y2018M11D08.XLSX")

gbdtoicdsub = gbdtoicd[2:dim(gbdtoicd)[1],1:2]  %>% 
  rename(cause = colnames(gbdtoicd)[1],
         icd = colnames(gbdtoicd)[2]) %>%
  separate(icd,sep=",",into=sprintf("icd%s",seq(1:218)),fill="right") %>%
  gather(index, icd, icd1:icd218) %>%
  separate(icd,sep="-",into=c("starticd","endicd"),fill="right") %>%
  filter(!is.na(starticd),
         str_detect(cause, 'Garbage Code', negate = T)) %>%
  mutate(starticd = decimal_to_short(starticd),
         endicd = decimal_to_short(endicd)) 
gbdtoicdsub$endicd[is.na(gbdtoicdsub$endicd)] = gbdtoicdsub$starticd[is.na(gbdtoicdsub$endicd)]
gbdtoicdsub$starticd=gsub("\\s", "", gbdtoicdsub$starticd) 
gbdtoicdsub$endicd=gsub("\\s", "", gbdtoicdsub$endicd) 

range = matrix(0,dim(gbdtoicdsub)[1],4000)
for (i in 1:dim(gbdtoicdsub)[1]){
temp = getICDRange(as.character(gbdtoicdsub$starticd[i]),as.character(gbdtoicdsub$endicd[i]),"icd10")
singleicd = (gbdtoicdsub$starticd[i])==as.character(gbdtoicdsub$endicd[i])
temp[singleicd=1]=gbdtoicdsub$starticd[i]
tempfill = 4000-length(temp)
range[i,]=c(temp,rep(NA,tempfill))
}
gbdtoicdtot = as.tibble(cbind(gbdtoicdsub$cause,range))
colnames(gbdtoicdtot)=c("cause_name",paste("icd",1:4000,sep="")) 


url2<-'http://ghdx.healthdata.org/sites/default/files/ihme_query_tool/IHME_GBD_2017_CODEBOOK.zip'
GET(url2, write_disk(tf <- tempfile(fileext = ".zip")))
unzip(zipfile=tf, files = "IHME_GBD_2017_CAUSE_HIERARCHY_Y2018M11D18.XLSX", exdir=".")
gbdid <- read_excel("IHME_GBD_2017_CAUSE_HIERARCHY_Y2018M11D18.XLSX")

gbdtoicdlink = inner_join(gbdtoicdtot,gbdid) %>%
  filter(level==4) %>%
  select(cause_id,cause_name,icd1:icd4000)  %>%
  gather(colname, icd, icd1:icd4000,na.rm=T) %>%
  select(cause_id, cause_name, icd) %>%
  rename(mapTarget = icd)


url3<-'https://download.nlm.nih.gov/mlb/utsauth/USExt/SnomedCT_USEditionRF2_PRODUCTION_20190301T120000Z.zip'
GET(url3, write_disk(tf <- tempfile(fileext = ".zip")))
unzip(zipfile=tf, files = "/Documentation/tls_Icd10cmHumanReadableMap_US1000124_20190301.tsv", exdir=".")
snomed <- read_tsv("tls_Icd10cmHumanReadableMap_US1000124_20190301.tsv")

test = snomed %>%
  mutate(mapTarget= decimal_to_short(mapTarget),
         mapTarget = as.character(mapTarget)) %>%
  inner_join(gbdtoicdlink)








