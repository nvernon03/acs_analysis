# download and merge 2016 5 year PUMS
library(rvest)
library(dplyr)
library(data.table)
library(bit64)

setwd(Sys.getenv("ACS_DIR")) # set working directory
source("pull_acs_function.R") # read in function

dt <- acs_pull(base="https://www2.census.gov/programs-surveys/acs/data/pums/",
               level="p",
               jurisdiction="us",
               year="2016",
               span="1",
               delete=TRUE)

file <- acs_file_name(folder="D:\\", level="p",
              jurisdiction="us",
              year="2016",
              span="1")

fwrite(dt, file)
