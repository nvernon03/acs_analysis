#' ---
#' title: "Summary Statistics"
#' author: "Nate & Matt"
#' date: "June 25, 2018"
#' output: html_document
#' ---


#' ### Clean RStudio and load packages
#+ echo=TRUE, message=FALSE, warning=FALSE
rm(list=ls()) # clear environment
library(data.table)
library(knitr)


#' ### Inputs
#+ echo=TRUE, message=FALSE, warning=FALSE
file_name_data <- "data/processed_level-p_jur-us_yr-2016_span-1"
file_name_function <- "2_analysis/sum_stat_function"
cont_vars <- c("log_wagp", "log_pincp", 
               "wkhp", "agep", "exp", "schl_yrs") # continuous variables to analyze
wd <- Sys.getenv("ACS_DIR") # working directory location

#' ### Read in the data and functions selected above
#+ echo=TRUE, message=FALSE, warning=FALSE
setwd(wd) # set working directory
opts_knit$set(root.dir = wd)
dt <- fread(paste0(file_name_data, ".csv")) # select dataset to use
source(paste0(file_name_function, ".R")) # read in functions


#' ### tables and graphs for continuous variables
#+ echo=TRUE, message=FALSE, warning=FALSE
for (v in cont_vars){
  print(paste0("summary stats: ", v))
  print(summary_func(dt, "race", v, "sex"))
  
  print(paste0("box plot: ", v))
  print(boxplot_func(dt, "race", v, "sex"))
}


#' ### tables and graphs for continuous variables
#+ echo=TRUE, message=FALSE, warning=FALSE
corr_func(data=dt, vars=cont_vars)

#' ### tables and graphs for continuous variables
#+ echo=TRUE, message=FALSE, warning=FALSE
combos <- combn(cont_vars, 2)
for (cmb in 1:ncol(combos)){
  print(scatter_func(x_var=combos[,cmb][1],
                     y_var=combos[,cmb][2],
                     by_var="race"))
}