# download and merge 2016 5 year PUMS
library(rvest)
library(dplyr)
library(data.table)
library(bit64)

setwd(Sys.getenv("ACS_DIR")) # set working directory
source("pull_acs_function.R") # read in function

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

# pull all data for the US
dt_list <- list()  ## create empty list
for (s in st_abr){ # loop through all states to create datatable
  dt_list[[s]] <- acs_pull(base="https://www2.census.gov/programs-surveys/acs/data/pums/",
                           level="p",
                           jurisdiction=s,
                           year="2016",
                           span="5",
                           delete=TRUE)
  print(s)
}

# bind dataframes together
dt <- rbindlist(dt_list)
rm(dt_list) # remove dataframe list

# save data - create folder if needed
file <- acs_file_name(folder="D:\\", level="p",
              jurisdiction="states",
              year="2016",
              span="5")

fwrite(dt, file)
