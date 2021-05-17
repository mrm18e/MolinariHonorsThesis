rm(list=ls())
library(tidyverse)
library(scales)
library(cowplot)
dat <- read_csv("./R/DATA-RAW/co-est2020-alldata.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  dplyr::select(GEOID, STATE:CTYNAME,
                POPESTIMATE2010:POPESTIMATE2020,
                BIRTHS2010:DEATHS2020,
                INTERNATIONALMIG2010:DOMESTICMIG2020)

quantile(dat$INTERNATIONALMIG2016, 0.5)  

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
  group_by(year) %>% # Grouping by County, County name, and Year
  summarise(births = sum(births)) %>% # Getting the total number of births by year.
  # group_by(GEOID, CTYNAME) %>% # Same grouping above but dropping year
  # mutate(perdrop = births/lag(births)) # calculating the % drop in births.
  I()
  
## Make a ggplot of the # of births between 2010-2020 for the US.
fig_births <- ggplot(births, aes(x= year, y = births, group =1)) +
  geom_line(lwd=1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw() +
  labs(title = "Births",
       x = "",
       y = "")

## Making a Deaths dataset
deaths <- dat %>%
  dplyr::select(GEOID:CTYNAME,
                DEATHS2010:DEATHS2020) %>%
  pivot_longer(cols = c(DEATHS2010:DEATHS2020),
               names_to = "deathyears",
               values_to = "deaths") %>%
  mutate(year = substr(deathyears,7,11)) %>%
  filter(COUNTY == "000",
         year != "2010") %>%
  group_by( year) %>%
  summarise(deaths = sum(deaths)) %>%
  # group_by(GEOID, CTYNAME) %>%
  # mutate(perdrop = deaths/lag(deaths))
  I()

## Make a ggplot of the # of deaths between 2010-2020 for the US.
fig_deaths <- ggplot(deaths, aes(x= year, y = deaths, group =1)) +
  geom_line(lwd=1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw() +
  labs(title = "Deaths",
       x = "",
       y = "")

## Making a Domestic Migration dataset
domesticmig <- dat %>%
dplyr::select(GEOID:CTYNAME,
              DOMESTICMIG2010:DOMESTICMIG2020) %>%
  pivot_longer(cols = c(DOMESTICMIG2010:DOMESTICMIG2020),
               names_to = "domesticmigyears",
               values_to = "domesticmig") %>%
  mutate(year = substr(domesticmigyears,12,15)) %>%
  filter(COUNTY == "000",
         year != "2010") %>%
  group_by(year) %>%
  summarise(domesticmig = sum(abs(domesticmig))) %>%
  #group_by(GEOID, CTYNAME) %>%
  #mutate(perdrop = domesticmigs/lag(domesticmigs))
  I()

## Make a ggplot of the # of domestic migrations between 2010-2020 for the US.
fig_dommig <- ggplot(domesticmig, aes(x= year, y = domesticmig, group =1)) +
  geom_line(lwd=1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw() +
  labs(title = "Domestic Migration",
       x = "",
       y = "")

## Making an International Migration dataset
internationalmig <- dat %>%
  dplyr::select(GEOID:CTYNAME,
                INTERNATIONALMIG2010:INTERNATIONALMIG2020) %>%
  pivot_longer(cols = c(INTERNATIONALMIG2010:INTERNATIONALMIG2020),
               names_to = "internationalmigyears",
               values_to = "internationalmig") %>%
  mutate(year = substr(internationalmigyears,17,20)) %>%
  filter(COUNTY == "000",
         year != "2010") %>%
  group_by(year) %>%
  summarise(internationalmig = sum(internationalmig)) %>%
  #group_by(GEOID, CTYNAME) %>%
  #mutate(perdrop = internationalmig/lag(internationalmig))
  I()

## Make a ggplot of the # of international migrations between 2010-2020 for the US.
fig_intmig <- ggplot(internationalmig, aes(x= year, y = internationalmig, group =1)) +
  geom_line(lwd=1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw() +
  labs(title = "International Migration",
       x = "",
       y = "")


fig_nat <- plot_grid(fig_births, fig_deaths,
          fig_dommig, fig_intmig,
          ncol = 2)

ggsave("./FIGURES/fig_nat.png")