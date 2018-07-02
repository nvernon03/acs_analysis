#' ---
#' title: "Regressions"
#' author: "Nate & Matt"
#' date: "June 25, 2018"
#' output: html_document
#' ---

#' ### Clean RStudio and load packages
#+ echo=TRUE, message=FALSE, warning=FALSE
rm(list=ls()) # clear environment
library(dplyr)
library(data.table)
library(ggplot2)
library(knitr)
library(car)

#' ### Inputs
#+ echo=TRUE, message=FALSE, warning=FALSE
file_name <- "processed_level-p_jur-us_yr-2016_span-1"
file_folder <- "data"
wd <- Sys.getenv("ACS_DIR") # working directory location
file_name_function <- "2_analysis/regression_function"
endogenous_var <- "log_hr_wagp"

#' ### Read in the data
#+ echo=TRUE, message=FALSE, warning=FALSE
setwd(wd) # set working directory
opts_knit$set(root.dir = wd)
dt <- fread(paste0(file_folder, "/", file_name, ".csv")) # select dataset to use
source(paste0(file_name_function, ".R")) # read in functions


#' ### Naive regression
#+ echo=TRUE, message=FALSE, warning=FALSE
naive_reg_rhs <- c("race_east_asian","race_white","race_afr_am")

reg_analysis(dt, endogenous_var, naive_reg_rhs,
             "race_east_asian", "race_white",
             "race", 100000)

#' ### Baseline regression
#+ echo=TRUE, message=FALSE, warning=FALSE
bl_reg_rhs <- c("race_east_asian","race_white","agep","exp","exp2","nativity","sex",
                names(dt)[grep("deg_", names(dt))][-1], 
                names(dt)[grep("state_", names(dt))][-1])

reg_analysis(dt, endogenous_var, bl_reg_rhs,
             "race_east_asian", "race_white",
             "race", 100000)


#' ### Baseline regression - California
#+ echo=TRUE, message=FALSE, warning=FALSE
dt_cali <- filter(dt, st == 06)
setDT(dt_cali)

bl_cali_reg_rhs <- c("race_east_asian","race_white","agep","exp","exp2","nativity","sex",
                names(dt_cali)[grep("deg_", names(dt_cali))][-1])


reg_analysis(dt_cali, endogenous_var, bl_cali_reg_rhs,
             "race_east_asian", "race_white",
             "race", 100000)