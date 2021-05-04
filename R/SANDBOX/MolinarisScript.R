library(tidyverse)
dat <- read_csv("./R/DATA-RAW/co-est2019-alldata.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  dplyr::select(STATE:CTYNAME,
                POPESTIMATE2010:POPESTIMATE2019,
                BIRTHS2010:DEATHS2019,
                INTERNATIONALMIG2010:DOMESTICMIG2019)
quantile(dat$INTERNATIONALMIG2016, 0.5) 

