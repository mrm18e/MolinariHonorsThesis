
## This is how you download a file from a website. This will download Census Estimates for 2010-2019 by demographic
## components of change for all counties. 
download.file("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
              "./R/DATA-RAW/co-est2019-alldata.csv")