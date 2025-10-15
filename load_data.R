#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load and manipulate data
#
# Krista, Oct 2025
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)

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
  summarise(seasonal_BTS_mean=mean(NGAO, na.rm=TRUE))


#load more data-----


birds <- read.csv(file=paste0(wd, "/data/", "MDO_age0sablefish_indices_2025.csv", sep=""))


rec <- read.csv(file=paste0(wd, "/data/", "spatial_sabie_rec_2024.csv", sep=""))
#NOT lagged to brood year, recruitment in 2024 is 2yo fish born in 2022

#look at data-----

ggplot(goadi, aes(year, DW)) + geom_point() + geom_line() + facet_wrap(~month)


ggplot(rec, aes(Year, Rec)) + geom_point() + facet_wrap(~Region)








