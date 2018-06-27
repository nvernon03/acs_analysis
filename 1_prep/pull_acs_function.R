# download and merge 2016 5 year PUMS
library(rvest)
library(dplyr)
library(data.table)
library(bit64)

# FUNCTION TO PULL DATA FOR THE ACS WEBSITE #
# inputs #
## base = the base URL for the dataset
## level = household (h) or personal (p)
## jurisdiction = state (abbreviation) or country (us)
## year = calender year
## span = time period for data (1 or 5)

# example url for 2016, 5 year average, household alaska
## "https://www2.census.gov/programs-surveys/acs/data/pums/2016/5-Year/csv_hak.zip"
acs_pull <- function(base, level, jurisdiction, year, span, delete) {
  full_url <- paste0(base, year, "/", span, "-Year/", "csv_", level, jurisdiction, ".zip") # url for the zip file
  temp <- tempfile() # name of temp file
  tempd <- tempdir() # name of temp directory
  
  download.file(full_url, temp) # downloads zip file
  unzip(temp, exdir=tempd) # unzip the tempfile that is found in the temp directory
  tempfiles <- list.files(path=tempd, pattern=".csv") # filter so that the list on includes csv for that jurisdiction 
    
  temp_dt_list <- list() # list to hold dts with all csvs
  for (f in tempfiles){ # loop through all files
    gc() # optimizes memory usage
    temp_dt_list[[f]] <- fread(paste0(tempd, "/", f)) # converts csv to a dataframe
    
    if (delete==TRUE){ # delete columns is delete is included
      to_delete <- append(grep('PWGTP', names(temp_dt_list[[f]]), value=TRUE),
                          names(temp_dt_list[[f]])[startsWith(names(temp_dt_list[[f]]), "F") & endsWith(names(temp_dt_list[[f]]), "P")])
      temp_dt_list[[f]] <- temp_dt_list[[f]][, {to_delete} := NULL]
    }
    file.remove(paste0(tempd, "\\", f)) # delete all csvs
  }
  
  unlink(temp) # delete the temp zip file
  do.call(file.remove, list(list.files(tempd, pattern=".csv", full.names=TRUE))) # delete all csvs
  do.call(file.remove, list(list.files(tempd, pattern=".pdf", full.names=TRUE))) # delete all pdfs
  
  gc() # optimizes memory usage
  temp_dt <- rbindlist(temp_dt_list) # create a master dt
  return(temp_dt)
}

# FUNCTION TO CREATE FILE NAME FOR THE DATASET
acs_file_name <- function(folder, level, jurisdiction, year, span){
  file_name <- paste0(folder,"level-",level,"_jur-",jurisdiction,"_yr-",year,"_span-",span,".csv")
  return(file_name)
}
