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
  mutate(perdrop =  (births - lag(births))/abs(lag(births))) %>%
  # mutate(perdrop = if_else(lag(births)<0,abs(perdrop), perdrop)) %>%
  I()
  
births$perdrop[is.nan(births$perdrop)] <- NA # some values are 0/0 or 0/1 or 1/0. We set those to NA
births[is.na(births)] <- 0 # we set all NA values to = 1.0

jenks_births <-  births %>%
  filter(year == 2020)

# Some values are Inf and -Inf. We drop them for the Jenks calculations.
jenks_births <- jenks_births[!is.infinite(jenks_births$perdrop),]
getJenksBreaks(jenks_births$perdrop, 5)
births <- births %>%
  filter(year == 2020) # we only want the 2020 change

z <- births[which(births$GEOID == "01001"),] %>% 
  filter(GEOID == "01001")
getJenksBreaks(jenks_births$perdrop, 5)
births <- births %>%
  filter(year == 2020) %>% # we only want the 2020 change
  dplyr::select(GEOID, perdrop) %>% # we select just our county ID and the percentage drop
  mutate(groups_perdrop = case_when( # we classify our percentage drops into given categories
    perdrop <= -1 ~ "< -100%",
    perdrop < -0.25 ~ "< -25%",
    perdrop < 0 ~ "< 0%",
    perdrop < 0.5 ~ "< 50%",
    perdrop <= 1000 ~ "> 50%"
  )) %>%
  I()
# We need to convert the categories into a leveled factor. If we don't do this, the order is wrong.
births$groups_perdrop = factor(births$groups_perdrop,
                               levels = c("< -100%", "< -25%", "< 0%", "< 50%", "> 50%"))
# Using colorbrewer, we create an RGB color scheme.
births$rgb <- "#999999" # we have to initialize the variable first.
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[1])] <- "#c51b7d"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[2])] <- "#e9a3c9"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[3])] <- "#fde0ef"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[4])] <- "#b8e186"
births$rgb[which(births$groups_perdrop == levels(births$groups_perdrop)[5])] <- "#4dac26"


# Joining our birth data with our shapefile
countydat <- left_join(shape, births)

pal2 <- c( "#c51b7d",  "#e9a3c9", "#fde0ef",  "#b8e186", "#4dac26")

# Making our map
map_births <- 
  ggplot(data = countydat) +
  geom_sf(aes(fill = groups_perdrop), color = NA) + # we set the fill to equal the raw color code
  geom_sf(data = states, fill=NA, color = "black") +
  scale_fill_manual(values = pal2, na.value = "#999999") +
  theme_bw() +
  coord_sf(datum=NA) +
  theme(legend.position = "right") +
  labs(fill = "% Change in Births")

countydat %>%
  ggplot(aes(fill = rgb)) +
  geom_sf(color = NA)

#CHANGE b8e186 TO GREENISH COLOR SINCE IT'S POSITIVE


