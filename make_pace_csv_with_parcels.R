
library(plyr)
library(dplyr)
library(tidyr)
lac<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRxE0zT4JOEff3HLOF1sauYuDXC2vUTgTIIc1Q6ddpme-FPSQXOWzVKJQjpTiEL-A/pub?gid=1764573151&single=true&output=csv")
lac<-filter(lac,Purpose=="Residential Energy Conservation, Improvement")
lac$text<-NA
lac$parcels<-NA
lac$Issuance.Documents<-lac$Issuance.Documents %>% stringr::str_extract("\\(.+\\)") %>% gsub("\\)|\\(","",.)
for(i in 1:nrow(lac)){
  pdt<-pdftools::pdf_text(lac$Issuance.Documents[i])
  lac$text[i]<-list(pdt)
}

#lac %>% saveRDS("Documents/san_gabriel/resbonds.rds")

lac$parcels<-NA
for(i in 1:nrow(lac)){
  doc1<-lac$text[[i]]
  local_match<-doc1 %>% gsub('--',"-",.,fixed=T) %>% stringr::str_which("[0-9][0-9][0-9][0-9]\\-?[0-9][0-9][0-9]\\-?[0-9][0-9][0-9]")
  bondnames<-doc1[local_match] %>% stringr::str_extract_all("Participating Parcels will secure Bond Number [.\\w\\W]+?\\:")  
  names(bondnames)<-local_match
  bondnames<-bondnames %>% as.character() %>% gsub('character(0)',"",.,fixed=T) 
  bondnames[bondnames==""]<-NA
  bondnames<-data.frame('bond'=bondnames) 
  bondnames<-bondnames %>% fill(bond)
  bondnames$parcels<-doc1[local_match] %>% stringr::str_extract_all("[0-9][0-9][0-9][0-9]\\-?[0-9][0-9][0-9]\\-?[0-9][0-9][0-9]")
  bondnames$bond<-bondnames$bond %>% stringr::str_extract("Bond Number [.\\w\\W]+?\\:") %>% gsub('Bond Number ','',.,fixed=T) %>% gsub(':','',.,fixed=T) %>% gsub(" ","",.)
  if(i==183){bondnames<-filter(bondnames, bond=="R-05A")}
  if(i %in% c(183)==F){
    if(length(table(bondnames$bond))>1){
      bondnames<-bondnames[stringr::str_detect(lac$Project.Name[i],stringr::fixed(gsub('\\n',"",bondnames$bond) %>% gsub("--","-",.))),]
    }}
  lac$parcels[i]<-list(bondnames$parcels %>% unlist())
  rm(bondnames)
  rm(local_match)
  rm(doc1)
  cat(paste0(i,'_'))
}

lac %>% saveRDS("Documents/san_gabriel/resbonds.rds")

which(lac$parcels %>% sapply(.,length)==0)