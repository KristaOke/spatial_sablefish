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
goadi$Date <- as.Date(goadi$Date)
goadi <- goadi %>% mutate(dates = as.Date(Date), 
                          month = month(dates), year = year(dates))

ngao <- read.csv(file=paste0(wd, "/data/", "NGAO_monthly.csv", sep=""))

#goadi and ngao are monthly, do we want an annual mean or something else?

birds <- read.csv(file=paste0(wd, "/data/", "MDO_age0sablefish_indices_2025.csv", sep=""))


rec <- read.csv(file=paste0(wd, "/data/", "spatial_sabie_rec_2024.csv", sep=""))
#NOT lagged to brood year, recruitment in 2024 is 2yo fish born in 2022

#look at data-----

ggplot(goadi, aes(Date, DW)) + geom_point() + geom_line()











