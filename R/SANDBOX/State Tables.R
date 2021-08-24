rm(list=ls())
library(tidyverse)
library(tidycensus)
library(scales)
library(cowplot)
library(BAMMtools)
library(hablar)

# downloading a shapefile for US counties. H0010001 is just a generic variable that is not used.
shape <- get_decennial("county",
                       variables = "H001001",
                       year = 2010,
                       sumfile = "sf1",
                       geometry = TRUE,
                       shift_geo = TRUE)

# downloading a shapefile for US states H0010001 is just a generic variable that is not used.
states <- get_decennial("state",
                        variables = "H001001",
                        year = 2010,
                        sumfile = "sf1",
                        geometry = TRUE,
                        shift_geo = TRUE) %>%
  mutate(STATE = substr(GEOID,1,2))


dat <- read_csv("./R/DATA-RAW/co-est2020-alldata.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  dplyr::select(GEOID, STATE:CTYNAME,
                POPESTIMATE2010:POPESTIMATE2020,
                BIRTHS2010:DEATHS2020,
                INTERNATIONALMIG2010:DOMESTICMIG2020)


## Making a Births dataset
births <- dat %>%
  dplyr::select(GEOID:CTYNAME, # Selecting the ID variables
                BIRTHS2010:BIRTHS2020) %>% # selecting only the births
  pivot_longer(cols = c(BIRTHS2010:BIRTHS2020), # Need to go from wide to tall.
               names_to = "birthyears", # making the name "birthyears"
               values_to = "births") %>% # making the # of births called births
  mutate(year = substr(birthyears,7,11)) %>% # birthyears is "BIRTHS2010" so getting only the year.
  filter(COUNTY != "000", # We need to filter out the state sums to get a US total
         year != "2010") %>% # 2010 is only a partial year. So we'll drop it
  group_by(year, STATE, STNAME) %>% # Grouping by County, County name, and Year
  summarise(births = sum(births)) %>% # Getting the total number of births by year.
  group_by(STATE, STNAME) %>% # Same grouping above but dropping year
  mutate(perdrop =  (births - lag(births))/abs(lag(births))) %>%
  # mutate(perdrop = if_else(lag(births)<0,abs(perdrop), perdrop)) %>%
  I()

births$perdrop[is.nan(births$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
births[is.na(births)] <- 0 # we set all NA values to = 1.0