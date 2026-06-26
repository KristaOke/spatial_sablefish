#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load and manipulate data
#
# Krista, Oct 2025
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)
library(corrplot)

#---------------------------------------------------------

wd<- getwd()

esp <- read.csv(file=paste0(wd, "/data/", "Sablefish_ESP_Indicator_Metadata.csv", sep=""))
#this data is NOT lagged

goadi <- read.csv(file=paste0(wd, "/data/", "GOADI_monthly.csv", sep=""))
goadi <- goadi %>% separate( col = Date, into = c("year", "month"))
goadi$year <- as.numeric(as.character(goadi$year))
goadi$month <- as.numeric(as.character(goadi$month))


ngao <- read.csv(file=paste0(wd, "/data/", "NGAO_monthly.csv", sep=""))
ngao <- ngao %>% separate( col = Date, into = c("year", "month"))
ngao$year <- as.numeric(as.character(ngao$year))
ngao$month <- as.numeric(as.character(ngao$month))

#goadi and ngao are monthly, do we want an annual mean or something else?
#I think likely summer since it's a heatwave linked effect
#let's try by season and annual

#get goadi and ngao season means----

#assign seasons get season means
#goadi
goadi$season <- "NA"
goadi$season[which(goadi$month>10|
                        goadi$month<4)] <- "winter"
goadi$season[which(goadi$month>3 &
                        goadi$month<7)] <- "spring"
goadi$season[which(goadi$month==7 |
                        goadi$month==8)] <- "summer"
goadi$season[which(goadi$month==9 |
                        goadi$month==10)] <- "fall"

goadi$season_year <- goadi$year

i <- 1
for(i in 1:length(goadi$year)){
  temprow <- goadi[i,]
  tempyear <- goadi$year[i]
  if(temprow$month > 10)
  {
    goadi$season_year[i] <- tempyear + 1
  }
}

goadi_season_means <- goadi %>% group_by(season_year, season) %>%
  summarise(seasonal_DW_mean=mean(DW, na.rm=TRUE))

#ngao
ngao$season <- "NA"
ngao$season[which(ngao$month>10|
                    ngao$month<4)] <- "winter"
ngao$season[which(ngao$month>3 &
                     ngao$month<7)] <- "spring"
ngao$season[which(ngao$month==7 |
                    ngao$month==8)] <- "summer"
ngao$season[which(ngao$month==9 |
                     ngao$month==10)] <- "fall"

ngao$season_year <- ngao$year

i <- 1
for(i in 1:length(ngao$year)){
  temprow <- ngao[i,]
  tempyear <- ngao$year[i]
  if(temprow$month > 10)
  {
    ngao$season_year[i] <- tempyear + 1
  }
}

ngao_season_means <- ngao %>% group_by(season_year, season) %>%
  summarise(seasonal_NGAO_mean=mean(NGAO, na.rm=TRUE))


#load more data-----


birds <- read.csv(file=paste0(wd, "/data/", "MDO_age0sablefish_indices_2025.csv", sep=""))

other_birds <- read.csv(file=paste0(wd, "/data/", "FWS_MDO_puffindiets_sablefish_1978-2025_forKristaOke.csv", sep=""))

other_birds <- other_birds %>% separate( col = Date, into = c("year", "month", "day"))
other_birds$year <- as.numeric(as.character(other_birds$year))
other_birds$month <- as.numeric(as.character(other_birds$month))
other_birds$day <- as.numeric(as.character(other_birds$day))

table(other_birds$year, other_birds$Colony)

#rec <- read.csv(file=paste0(wd, "/data/", "spatial_sabie_rec_2024.csv", sep=""))
rec <- read.csv(file=paste0(wd, "/data/", "ts_df.csv", sep=""), row.names=1)
#NOT lagged to brood year, recruitment in 2024 is 2yo fish born in 2022

#look at data-----

ggplot(goadi, aes(year, DW)) + geom_point() + geom_line() + facet_wrap(~month)

ggplot(goadi_season_means, aes(season_year, seasonal_DW_mean)) + geom_point() + geom_line() + facet_wrap(~season)

ggplot(ngao_season_means, aes(season_year, seasonal_NGAO_mean)) + geom_point() + geom_line() + facet_wrap(~season)

ggplot(rec, aes(Year, Rec)) + geom_point() + facet_wrap(~Region)

#match data into one data frame-------

#start matching all indicators together, look at correlations, will sort into regions later

esp_birds <- left_join(esp, birds, by=join_by(Year==year))

summer_ngao <- ngao_season_means[which(ngao_season_means$season=="summer"),]

esp_birds_ngao <- left_join(esp_birds, summer_ngao[,-2], by=join_by(Year==season_year))
esp_birds_ngao <- rename(esp_birds_ngao, c("summer_NGAO"="seasonal_NGAO_mean"))

summer_goadi <- goadi_season_means[which(goadi_season_means$season=="summer"),]

esp_birds_ngao_goadi <- left_join(esp_birds_ngao, summer_goadi[,-2], by=join_by(Year==season_year))
esp_birds_ngao_goadi <- rename(esp_birds_ngao_goadi, c("summer_DW"="seasonal_DW_mean"))

indic_dat <- esp_birds_ngao_goadi

cov.cor <- cor(na.omit(indic_dat), use='pairwise.complete.obs')
#VERY few data once NAs are removed
corrplot(cov.cor,order='AOE',  type = 'lower', method = 'number', number.cex=0.8, tl.cex=0.5, 
         mar = c(0,0,1,0),  number.digits = 2)

#again w/o goa survey (every second yr)
cov.cor2 <- cor(na.omit(indic_dat[,-11]), use='pairwise.complete.obs')
#VERY few data once NAs are removed
corrplot(cov.cor2,order='AOE',  type = 'lower', method = 'number', number.cex=0.8, tl.cex=0.5, 
         mar = c(0,0,1,0),  number.digits = 2)

#sort indicators into regions---------

AI_rec <- rec[which(rec$Region=="AI"),]
BS_rec <- rec[which(rec$Region=="BS"),]
WGOA_rec <- rec[which(rec$Region=="WGOA"),]
CGOA_rec <- rec[which(rec$Region=="CGOA"),]
EGOA_rec <- rec[which(rec$Region=="EGOA"),]

AI_ind <- indic_dat %>% select(c(Year, Annual_Eddy_Kinetic_Energy_Amchitka_Satellite))
AI_df <- left_join(AI_rec, AI_ind)

BS_ind <- indic_dat %>% select(c(Year, Annual_Small_Sablefish_Incidental_Hauls_EBS_Fishery,
                                 Spring_Temperature_Surface_SEBS_Satellite,
                                 Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey)) #,
                               #  Spring_Chlorophylla_Biomass_SEBS_Satellite)) #chlorophyll data missing
BS_df <- left_join(BS_rec, BS_ind)

WGOA_ind <- indic_dat %>% select(c(Year, Spring_Temperature_Surface_GOA_Satellite,
                                  # Spring_Chlorophylla_Biomass_GOA_Satellite,
                                  # Spring_Chlorophylla_Peak_GOA_Satellite,
                                   Annual_Copepod_Community_Size_WGOA_Survey,
                                   Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey,
                                   Summer_Sablefish_CPUE_Juvenile_GOA_Survey,
                                   Summer_Temperature_250m_GOA_Survey,
                                   Summer_Sablefish_Condition_Female_Adult_GOA_Survey,
                                   Annual_Sablefish_Incidental_Catch_Arrowtooth_Target_GOA_Fishery,
                                   summer_NGAO,
                                   summer_DW))
WGOA_df <- left_join(WGOA_rec, WGOA_ind)

CGOA_ind <- indic_dat %>% select(c(Year, Spring_Temperature_Surface_GOA_Satellite,
                                  # Spring_Chlorophylla_Biomass_GOA_Satellite,
                                  # Spring_Chlorophylla_Peak_GOA_Satellite,
                                   Summer_Sablefish_CPUE_Juvenile_Nearshore_GOAAI_Survey,
                                   Summer_Sablefish_CPUE_Juvenile_GOA_Survey,
                                   Summer_Temperature_250m_GOA_Survey,
                                   Summer_Sablefish_Condition_Female_Adult_GOA_Survey,
                                   Annual_Sablefish_Incidental_Catch_Arrowtooth_Target_GOA_Fishery,
                                   summer_NGAO,
                                   summer_DW))
CGOA_df <- left_join(CGOA_rec, CGOA_ind)


EGOA_ind <- indic_dat %>% select(c(Year, Spring_Temperature_Surface_GOA_Satellite,
                                   #Spring_Chlorophylla_Biomass_GOA_Satellite,
                                   #Spring_Chlorophylla_Peak_GOA_Satellite,
                                   Annual_Copepod_Community_Size_EGOA_Survey,
                                   Summer_Sablefish_CPUE_Juvenile_GOA_Survey,
                                   Summer_Temperature_250m_GOA_Survey,
                                   Summer_Sablefish_Condition_Female_Adult_GOA_Survey,
                                   Annual_Sablefish_Incidental_Catch_Arrowtooth_Target_GOA_Fishery,
                                   summer_NGAO,
                                   summer_DW,
                                   n_samples,                                                      
                                   n_sablefish,                                                  
                                   propmass,                                                    
                                   CPUE,                                                       
                                   FO,                                                           
                                   growth_index,                                                   
                                   growth_anomaly,                                                 
                                   pred_len,                                                 
                                   pred_len_se,                                                    
                                   pred_len_anomaly  ))
EGOA_df <- left_join(EGOA_rec, EGOA_ind)




#check ESP package for data=====

pak::pak("atyrell3/AKesp")
library(AKesp)

AKesp::esp_stock_options() #no AI


