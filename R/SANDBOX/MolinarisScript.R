library(tidyverse)
dat <- read_csv("./R/DATA-RAW/co-est2019-alldata.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  dplyr::select(GEOID, STATE:CTYNAME,
                POPESTIMATE2010:POPESTIMATE2019,
                BIRTHS2010:DEATHS2019,
                INTERNATIONALMIG2010:DOMESTICMIG2019) 
quantile(dat$INTERNATIONALMIG2016, 0.75) 


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

## This is a test! -- Matt Hauer

