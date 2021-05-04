
## This is how you download a file from a website. This will download Census Estimates for 2010-2019 by demographic
## components of change for all counties. 
download.file("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
              "./R/DATA-RAW/co-est2019-alldata.csv")


dat <- read_csv("./R/DATA-RAW/co-est2019-alldata.csv") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  dplyr::select(STATE:CTYNAME,
                POPESTIMATE2010:POPESTIMATE2019,
                BIRTHS2010:DEATHS2019,
                INTERNATIONALMIG2010:DOMESTICMIG2019)

quantile(dat$INTERNATIONALMIG2016, 0.5)  

