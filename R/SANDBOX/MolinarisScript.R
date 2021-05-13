library(tidyverse)
dat <- read_csv("./R/DATA-RAW/co-est2019-alldata.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  dplyr::select(GEOID, STATE:CTYNAME,
                POPESTIMATE2010:POPESTIMATE2019,
                BIRTHS2010:DEATHS2019,
                INTERNATIONALMIG2010:DOMESTICMIG2019) 
quantile(dat$INTERNATIONALMIG2016, 0.5) 


## Making a Births dataset
births <- dat %>%
  dplyr::select(GEOID:CTYNAME, # Selecting the ID variables
                BIRTHS2010:BIRTHS2019) %>% # selecting only the births
  pivot_longer(cols = c(BIRTHS2010:BIRTHS2019), # Need to go from wide to tall.
    names_to = "birthyears", # making the name "birthyears"
               values_to = "births") %>% # making the # of births called births
  mutate(year = substr(birthyears,7,11)) %>% # birthyears is "BIRTHS2010" so getting only the year.
  filter(COUNTY == "000", # We need to filter out the state sums to get a US total
         year != "2010") %>% # 2010 is only a partial year. So we'll drop it
  group_by(birthyears) %>%
  summarise(births = sum(births)) # Getting the total number of births by year.

## Make a ggplot of the # of births between 2010-2019 for the US.
ggplot(births, aes(x= birthyears, y = births, group =1)) +
  geom_line(lwd=1)

## Making a Deaths dataset
deaths <- dat %>%
  dplyr::select(GEOID:CTYNAME,
                DEATHS2010:DEATHS2019) %>%
  pivot_longer(cols = c(DEATHS2010:DEATHS2019),
               names_to = "deathyears",
               values_to = "deaths") %>%
  mutate(year = substr(deathyears,7,11)) %>%
  filter(COUNTY == "000",
         year != "2010") %>%
  group_by(deathyears) %>%
  summarise(deaths = sum(deaths))

## Make a ggplot of the # of deaths between 2010-2019 for the US.
ggplot(deaths, aes(x= deathyears, y = deaths, group =1)) +
  geom_line(lwd=1)

## Making a Domestic Migration dataset
domesticmig <- dat %>%
dplyr::select(GEOID:CTYNAME,
              DOMESTICMIG2010:DOMESTICMIG2019) %>%
  pivot_longer(cols = c(DOMESTICMIG2010:DOMESTICMIG2019),
               names_to = "domesticmigyears",
               values_to = "domesticmig") %>%
  mutate(year = substr(domesticmigyears,7,11)) %>%
  filter(COUNTY == "000",
         year != "2010") %>%
  group_by(domesticmigyears) %>%
  summarise(domesticmig = sum(domesticmig))

## Make a ggplot of the # of domestic migrations between 2010-2019 for the US.
ggplot(domesticmig, aes(x= domesticmigyears, y = domesticmig, group =1)) +
  geom_line(lwd=1)

## Making an International Migration dataset
internationalmig <- dat %>%
  dplyr::select(GEOID:CTYNAME,
                INTERNATIONALMIG2010:INTERNATIONALMIG2019) %>%
  pivot_longer(cols = c(INTERNATIONALMIG2010:INTERNATIONALMIG2019),
               names_to = "internationalmigyears",
               values_to = "internationalmig") %>%
  mutate(year = substr(internationalmigyears,7,11)) %>%
  filter(COUNTY == "000",
         year != "2010") %>%
  group_by(internationalmigyears) %>%
  summarise(internationalmig = sum(internationalmig))

## Make a ggplot of the # of international migrations between 2010-2019 for the US.
ggplot(internationalmig, aes(x= internationalmigyears, y = internationalmig, group =1)) +
  geom_line(lwd=1)