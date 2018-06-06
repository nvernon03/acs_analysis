# download and merge 2016 5 year PUMS
library(rvest)
library(stringr)
library(dplyr)
library(data.table)
library(bit64)

setwd(Sys.getenv("ACS_DIR")) # set working directory

# scrape the abbreviations for individual states
webpage <- read_html("https://www2.census.gov/programs-surveys/acs/data/pums/2016/5-Year/")
st_abr <- webpage %>% 
  html_nodes("table") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>%
  str_subset(pattern="csv_h") %>% 
  str_replace("csv_h", "") %>%
  str_replace(".zip", "")

st_abr <- st_abr[!str_detect(st_abr,pattern="us")]

# FUNCTION TO PULL DATA FOR THE ACS WEBSITE #
# inputs #
## base = the base URL for the dataset
## level = household (h) or personal (p)
## jurisdiction = state (abbreviation) or country (us)
## year = calender year
## span = time period for data (1 or 5)

# example url for 2016, 5 year average, household alaska
## "https://www2.census.gov/programs-surveys/acs/data/pums/2016/5-Year/csv_hak.zip"
acs_pull <- function(base, level, jurisdiction, year, span) {
  full_url <- paste0(base, year, "/", span, "-Year/", "csv_", level, jurisdiction, ".zip") # url for the zip file
  temp <- tempfile() # name of temp file
  tempd <- tempdir() # name of temp directory
  
  download.file(full_url, temp) # downloads zip file
  unzip(temp, exdir=tempd) # unzip the tempfile that is found in the temp directory
  tempfiles <- list.files(path=tempd, pattern=".csv") %>% # filter so that the list on includes csv for that jurisdiction 
    str_subset(pattern=paste0(substr(year, 3, 4), level, jurisdiction))
  
  temp_dt_list <- list() # list to hold dts with all csvs
  for (f in tempfiles){ # loop through all files
    temp_dt_list[[f]] <- fread(paste0(tempd, "/", f)) # converts csv to a dataframe
  }
  
  unlink(temp) # delete the temp zip file
  do.call(file.remove, list(list.files(tempd, pattern=".csv", full.names=TRUE))) # delete all csvs
  do.call(file.remove, list(list.files(tempd, pattern=".pdf", full.names=TRUE))) # delete all pdfs
  
  temp_dt <- rbindlist(temp_dt_list) # create a master dt
  return(temp_dt)
}

# pull all data for the US
dt_list <- list()  ## create empty list
for (s in st_abr[1:3]){
  dt_list[[s]] <- acs_pull(base="https://www2.census.gov/programs-surveys/acs/data/pums/",
                           level="p",
                           jurisdiction=s,
                           year="2016",
                           span="5")
  print(s)
}

# bind dataframes together
dt <- rbindlist(dt_list)
rm(dt_list) # remove dataframe list

# save data - create folder if needed
dir.create(file.path("data"), showWarnings = FALSE)
write.csv(dt, "data/p_2016_5yr.csv")