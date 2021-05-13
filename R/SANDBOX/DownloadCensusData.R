
## This is how you download a file from a website. This will download Census Estimates for 2010-2020 by demographic
## components of change for all counties. 
download.file("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv",
              "./R/DATA-RAW/co-est2020-alldata.csv")

library(tidyverse)
dat <- read_csv("./R/DATA-RAW/co-est2020-alldata.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  dplyr::select(STATE:CTYNAME,
                POPESTIMATE2010:POPESTIMATE2020,
                BIRTHS2010:DEATHS2020,
                INTERNATIONALMIG2010:DOMESTICMIG2020)

quantile(dat$INTERNATIONALMIG2016, 0.5)  


