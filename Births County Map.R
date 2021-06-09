rm(list=ls())
library(tidyverse)
library(tidycensus)
library(scales)
library(cowplot)
library(BAMMtools)

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
  group_by(year, GEOID, STATE, COUNTY, STNAME, CTYNAME) %>% # Grouping by County, County name, and Year
  summarise(births = sum(births)) %>% # Getting the total number of births by year.
  group_by(GEOID, CTYNAME) %>% # Same grouping above but dropping year
  mutate(perdrop = births/lag(births))
births$perdrop[is.nan(births$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
births[is.na(births)] <- 1 # we set all NA values to = 1.0

births <- births %>%
  filter(year == 2020) %>% # we only want the 2020 change
  dplyr::select(GEOID, perdrop) %>% # we select just our county ID and the percentage drop
  mutate(groups_perdrop = case_when( # we classify our percentage drops into given categories
    perdrop <= 0.85 ~ "< 0.85",
    perdrop < 1 ~ "< 1",
    perdrop < 1.15 ~ "< 1.15",
    perdrop < 1.5 ~ "< 1.5",
    perdrop <= 2 ~ "> 1.5"
  )) %>%
  I()
# We need to convert the categories into a levelled factor. If we don't do this, the order is wrong.
births$groups_perdrop = factor(births$groups_perdrop,
                               levels = c("< 0.85", "< 1", "< 1.15", "< 1.5", "> 1.5"))
# Using colorbrewer, we create an RGB color scheme.
births$rgb <- "#999999" # we have to initialize the variable first.
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[1])] <- "#d7191c"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[2])] <- "#fdae61"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[3])] <- "#ffffbf"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[4])] <- "#abdda4"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[5])] <- "#2b83ba"

# Joining our birth data with our shapefile
countydat <- left_join(shape, births)

# Making our map
map_births <- 
  ggplot() +
  geom_sf(data = countydat, fill = countydat$rgb, color = NA) + # we set the fill to equal the raw color code
  geom_sf(data = states, fill=NA, color = "black") +
  theme_bw() +
  coord_sf(datum=NA) +
  theme(legend.position = "right")

countydat %>%
  ggplot(aes(fill = rgb)) +
  geom_sf(color = NA)


