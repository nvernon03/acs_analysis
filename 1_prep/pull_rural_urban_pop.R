# Clean RStudio and load packages
rm(list=ls()) # clear environment
library(rvest)
library(dplyr)
library(data.table)
library(reshape2)

# Inputs
file_folder <- "data"
file_name <- "urban_rural"
url <- "http://mcdc.missouri.edu/tmpscratch/02JUL1001289.geocorr14/geocorr14.csv"
wd <- Sys.getenv("ACS_DIR") # working directory location

# Scrape webpage
dt_raw <- as.data.table(read.csv(url, sep=",", header=TRUE))
setDT(dt_raw)

# Clean data table
dt <- dt_raw %>%
  select(puma12, ur, pop14) %>%
  slice(2:n()) %>%
  mutate_at("pop14", funs(as.numeric)) %>%
  mutate_at(c("puma12", "ur"), funs(as.character))

dt <- dt[!duplicated(dt), ]

dt_wide <- dcast(dt, puma12 ~ ur, value.var = "pop14", sum) %>%
  rename(rural = R) %>%
  rename(urban = U) %>%
  mutate(urban_perc = urban/(rural + urban)) %>%
  select(puma12, urban_perc)

# Save data table
setwd(wd)
fwrite(dt_wide, paste0(file_folder, "/", file_name, ".csv"))
