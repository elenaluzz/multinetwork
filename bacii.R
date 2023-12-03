library(magrittr)
library(dplyr)
setwd("C:/Users/elena/OneDrive/Desktop/BACII")
data_kiss_one <- read.csv("BACI_HS96_Y2018_V202201.csv",
                          sep=",")
list_of_files <- list.files(path = "C:/Users/elena/OneDrive/Desktop/BACII", recursive = TRUE,
                            pattern = "\\.", 
                            full.names = TRUE)
idx_code <-  read.csv("C:/Users/elena/OneDrive/Desktop/BACII/country_codes_V202201.csv",
                      sep=",", header=TRUE)

full_data<-NULL
for (i in 1:2){
  print(i)
  dat<-read.table(list_of_files[i],sep=',', header=TRUE)
  full_data<-rbind(full_data,dat)
}


new_data<-full_data %>%
  select (-c(q))
colnames(new_data)<-c("year","exporter","importer","product","value") 

new_new<-new_data %>% #qua perchè non ti interessa il tipo di prodotto 
  select (-c(product))

data_merge <- new_new %>% #quindi aggreghi 
  group_by(year,exporter,importer) %>% 
  summarise(across(value, ~ sum(.x)))
  

######
data_merge$exporter <- idx_code$iso_3digit_alph[match(as.factor(data_merge$exporter), as.factor(idx_code$country_code))]
data_merge$importer <- idx_code$iso_3digit_alph[match(as.factor(data_merge$importer), as.factor(idx_code$country_code))]

#####
colnames(data_merge) <-c("year","from","to","value")

###########################################################
#per renderli simmetrici...
new_na <- NULL
for (i in unique(data_merge$year)){
  print(i)
  dat<-subset(data_merge, year == i)
  dat<-dat %>%
    group_by(year) %>%
    mutate(across(c(from,to), list(as.character))) %>%
    mutate( V1 = pmin(from, to),
            V2 = pmax(from, to) ) %>%
    group_by(V1, V2) %>%
    summarise(across(value,~ sum(.x)))
  dat$year<-i
  #dat <- mutate(dat, across(everything(), as.factor))
  new_na<-bind_rows(new_na,dat)
}
full_baci<-new_na
colnames(full_baci)<-c("from","to","value","year")
full_baci$value<-full_baci$value*1000
###########################################################

full_baci<-data_merge
colnames(full_baci)<-c("year","from","to","value")
full_baci$value<-full_baci$value*1000

### Subset for actual country
full_data_removed<-full_baci[grepl(paste(iso,collapse="|"), full_baci$from),]
full_data_removed<-full_data_removed[grepl(paste(iso,collapse="|"), full_data_removed$to),]

full_data_removed<-subset(full_data_removed, (as.character(from) != as.character(to)))
new_baci<-full_data_removed

new_baci_corr <- new_baci %>% 
  group_by(year) %>% 
  mutate_if(is.numeric,log_max)

write.csv(new_baci_corr,"C://Users/dadel/OneDrive/Desktop/Data_mutli_net/additional_data/baci_trade.csv", row.names = FALSE)



########### CREATE MERGED ONE 
### Create one with: tourism, CPIS, Trade - 2001-2010
# Stack all of them together

# CPIS-Tourism
full_data_UNWTO$year<-as.character(full_data_UNWTO$year)
prova_prova_two<-full_join(full_data_cpis, full_data_UNWTO, by=c('from', 'to','year'))
colnames(prova_prova_two)<-c("from","to","value_cpis","year","value_unwto")

#Add trade
new_baci$year<-as.character(new_baci$year)
prova_prova_two<-full_join(prova_prova_two, new_baci, by=c('from', 'to','year'))
colnames(prova_prova_two)<-c("from","to","value_cpis","year","value_unwto","value_trade")

#### Subs NA and symmetrize alk
prova_prova_two$value_cpis<-as.numeric(as.character(prova_prova_two$value_cpis))
prova_prova_two$value_unwto<-as.numeric(as.character(prova_prova_two$value_unwto))
prova_prova_two$value_trade<-as.numeric(as.character(prova_prova_two$value_trade))


prova_prova_two[is.na(prova_prova_two)] = 0

new_na <- NULL
for (i in unique(prova_prova_two$year)){
  print(i)
  dat<-subset(prova_prova_two, year == i)
  dat<-dat %>%
    group_by(year) %>%
    mutate_each(funs(as.character), c(from, to)) %>%
    mutate( V1 = pmin(from, to),
            V2 = pmax(from, to) ) %>%
    group_by(V1, V2) %>%
    summarise_each(funs(sum), -c(from,to,year))
  dat$year<-i
  dat <- mutate(dat, across(everything(), as.factor))
  new_na<-bind_rows(new_na,dat)
}

colnames(new_na)<-c("from","to","value_cpis","value_unwto","value_trade","year")
full_data_GOOD_two<-new_na

## Should remove those not present and the self-loops
full_data_removed<-full_data_GOOD_two[grepl(paste(giorgio_iso$iso,collapse="|"), full_data_GOOD_two$from),]
full_data_removed<-full_data_removed[grepl(paste(giorgio_iso$iso,collapse="|"), full_data_removed$to),]

full_data_removed<-subset(full_data_removed, (as.character(from) != as.character(to)))

## remove gcd
tourism_no<-c("BWA","COD","SWZ", "LSO", "LUX","MNE","NAM", "SRB", "SSD", "TLS")

full_data_removed<-full_data_removed[!grepl(paste(tourism_no,collapse="|"), full_data_removed$from),]
full_data_removed<-full_data_removed[!grepl(paste(tourism_no,collapse="|"), full_data_removed$to),]

# Adjust format
full_data_removed$year<-as.factor(full_data_removed$year)
full_data_removed$from <- as.factor(full_data_removed$from)
full_data_removed$to <- as.factor(full_data_removed$to)

prova_prova<-full_data_removed
prova_prova$value_cpis<-as.numeric(as.character(prova_prova$value_cpis))
prova_prova$value_unwto<-as.numeric(as.character(prova_prova$value_unwto))
prova_prova$value_trade<-as.numeric(as.character(prova_prova$value_trade))

## Take only subset 2001-2010
new_new<-prova_prova
new_new$year<-as.numeric(as.character(new_new$year))
new_new<-
  subset(new_new, year>=2001 & year<=2010)
new_new$year<-as.factor(new_new$year)

## Log-normal
new_new$value_cpis[new_new$value_cpis>0 & new_new$value_cpis<1] <- 0

new_data_all <- new_new %>% 
  group_by(year) %>% 
  mutate_if(is.numeric,log_max)
new_data_all[is.na(new_data_all)]  = 0



write.csv(new_data_all,"C://Users/dadel/OneDrive/Desktop/Data_mutli_net/additional_data/tourism_cpis_baci_trade.csv", row.names = FALSE)







## Compare with trade
full_data_removed<-full_data_trade[grepl(paste(giorgio_iso$iso,collapse="|"), full_data_trade$from),]
full_data_removed<-full_data_removed[grepl(paste(giorgio_iso$iso,collapse="|"), full_data_removed$to),]

full_data_removed<-subset(full_data_removed, (as.character(from) != as.character(to)))


new_trade<-full_data_removed

### Merge the two
new_baci$year<-as.character(new_baci$year)
prova_prova_both<-full_join(new_trade, new_baci, by=c('from', 'to','year'))
prova_prova_both[is.na(prova_prova_both)] = 0

new_na <- NULL
for (i in unique(prova_prova_both$year)){
  print(i)
  dat<-subset(prova_prova_both, year == i)
  dat<-dat %>%
    group_by(year) %>%
    mutate_each(funs(as.character), c(from, to)) %>%
    mutate( V1 = pmin(from, to),
            V2 = pmax(from, to) ) %>%
    group_by(V1, V2) %>%
    summarise_each(funs(sum), -c(from,to,year))
  dat$year<-i
  #dat <- mutate(dat, across(everything(), as.factor))
  new_na<-bind_rows(new_na,dat)
}