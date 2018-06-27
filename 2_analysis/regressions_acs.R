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
library(psych)
library(data.table)
library(lazyeval)
library(ggplot2)

#' ### Inputs
#+ echo=TRUE, message=FALSE, warning=FALSE
file_name <- "processed_level-p_jur-us_yr-2016_span-1"
file_folder <- "data"

#' ### Read in the data
#+ echo=TRUE, message=FALSE, warning=FALSE
dt <- fread(paste0(file_folder, "/", file_name, ".csv")) # select dataset to use

#' ### Read in the data
#+ echo=TRUE, message=FALSE, warning=FALSE
# baseline regression
bl_reg_rhs <- c("race_east_asian","race_white","agep","exp","exp2","nativity","sex",
                names(dt)[grep("deg_", names(dt))])
bl_reg <- lm(as.formula(paste0("wagp~", paste(bl_reg_rhs, collapse = "+"))), dt)
summary(bl_reg)