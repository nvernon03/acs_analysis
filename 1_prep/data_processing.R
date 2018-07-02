#' ---
#' title: "Data Processing"
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
library(kableExtra)
library(assertthat)
library(knitr)

#' ### Inputs
#+ echo=TRUE, message=FALSE, warning=FALSE
file_name <- "level-p_jur-us_yr-2016_span-1"
file_folder <- "data"
hrs_wk <- 20 # number of hours per week to exclude in analysis
wd <- Sys.getenv("ACS_DIR") # working directory location

#' ### Read in the data selected above
#+ echo=TRUE, message=FALSE, warning=FALSE
opts_knit$set(root.dir = wd)
setwd(wd)
dt_raw <- fread(paste0(file_folder, "/", file_name, ".csv")) # select dataset to use


#' ### Format to Stata variable names; drop some variables
#+ echo=TRUE, message=FALSE, warning=FALSE
names(dt_raw) <- tolower(names(dt_raw)) ## lower case to match stata
dt_raw <- dt_raw[, {grep('pwgtp', names(dt_raw), value=TRUE)} := NULL] ## drop unneeded variables that start with pwgtp --- must happen before "tolower" command


#' ### Data processing
#' #### Ensure that correct obs. are dropped 
#+ echo=TRUE, message=FALSE, warning=FALSE
dt <- dt_raw %>% 
  filter(wkhp >= hrs_wk) %>% # drop if work less than hrs_wk hours a week
  filter(wagp != 0) # drop if wagp is 0
  
assert_that(min(dt$wkhp) >= hrs_wk) ## ensure that the above drop command worked correctly


#' ### Print head of data
#+ echo=TRUE, message=FALSE, warning=FALSE
kable(head(dt)) %>%
kable_styling() %>%
scroll_box(width = "100%")


#' #### All other data processing
#+ echo=TRUE, message=FALSE, warning=FALSE
## dummy for east asian country and other asian
east_asia_code <- c(5,7,8)
dt <- mutate(dt, easn = as.numeric(rac3p %in% east_asia_code)) %>%
  mutate(asn = ifelse(easn == 0 & rac1p == 6, 1, 0))

## dummy for white
dt <- mutate(dt, wht = ifelse(dt$racwht == 1 & 
                                ((dt$anc1p < 400 | dt$anc1p > 499) | 
                                   (dt$anc2p < 400 | dt$anc2p > 499)), 1, 0))
## new simplified race variable
dt <- mutate(dt, race = ifelse(wht == 1, "white",
                               ifelse(easn == 1, "east_asian", 
                                      ifelse(rac1p == 2, "afr_am", "other"))))

race_coded <- as.data.table(dummy.code(dt$race))
names(race_coded) <- paste("race", names(race_coded), sep = "_")
dt <- cbind(dt, race_coded)


#+ echo=TRUE, message=FALSE, warning=FALSE
table(dt$race) # show frequency of variable


#+ echo=TRUE, message=FALSE, warning=FALSE
## school coded variable
dt <- mutate(dt, deg = ifelse(dt$schl <= 15, "no_hs",
                              ifelse(dt$schl <= 19, "hs",
                                     ifelse(dt$schl <= 20, "associate",
                                            ifelse(dt$schl <= 21, "bach",
                                                   ifelse(dt$schl <= 23, "master",
                                                          ifelse(dt$schl <= 24, "doctorate")))))))

deg_coded <- as.data.table(dummy.code(dt$deg))
names(deg_coded) <- paste("deg", names(deg_coded), sep = "_")
dt <- cbind(dt, deg_coded)


#+ echo=TRUE, message=FALSE, warning=FALSE
table(dt$deg) # show frequency of variable


#+ echo=TRUE, message=FALSE, warning=FALSE
## create experience variable
dt <- mutate(dt, schl_yrs = ifelse(dt$schl <= 3, 0,
                                   ifelse(dt$schl <= 15, dt$schl - 3,
                                          ifelse(dt$schl <= 18, 12,
                                                 ifelse(dt$schl <= 20, 14,
                                                        ifelse(dt$schl <= 21, 16,
                                                               ifelse(dt$schl <= 23, 18,
                                                                      ifelse(dt$schl <= 24, 23))))))))
dt <- mutate(dt, exp = agep - 5 - schl_yrs) %>%
  mutate(exp2 = exp*exp)


#+ echo=TRUE, message=FALSE, warning=FALSE
kable(describe(dt[, c('exp', 'schl_yrs')]), digits=3) %>% # show summary stats of variables
  kable_styling("striped", full_width = F) %>% # do not print all decimals
  scroll_box(width = "100%")


#+ echo=TRUE, message=FALSE, warning=FALSE
## create married dummy
dt <- mutate(dt, married = as.numeric(mar==1))

## create military dummy
dt <- mutate(dt, military = as.numeric(mil != 4))

## recode sex dummy
dt$sex <- ifelse(dt$sex==1, 0, 1)

## natural log of wage variables
log_cols <- c("intp", "wagp", "oip", "pap", "semp", "pincp")
dt <- mutate_at(dt, setNames(log_cols, paste0("log_", log_cols)), log)

## wage per hours worked
### absolute value - no log
per_hr <- dt[log_cols] / dt$wkhp
names(per_hr) <- paste("hr", names(per_hr), sep = "_")
dt <- cbind(dt, per_hr)

### logs
dt <- mutate_at(dt, setNames(names(per_hr), paste0("log_", names(per_hr))), log)

## state level dummies
state_coded <- as.data.table(dummy.code(dt$st))
names(state_coded) <- paste("state", names(state_coded), sep = "_")
dt <- cbind(dt, state_coded)

#' ### Print variable names
#+ echo=TRUE, message=FALSE, warning=TRUE
print(names(dt))

kable(head(dt)) %>%
  kable_styling() %>%
  scroll_box(width = "100%")



#' ### Save dataset
#+ echo=TRUE, message=FALSE, warning=TRUE
opts_knit$set(root.dir = wd)
setwd(wd)
fwrite(dt, paste0(file_folder, "/processed_", file_name, ".csv"))
