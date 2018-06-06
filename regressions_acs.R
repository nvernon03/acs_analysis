# regressions to determine relationship between race and wage 
library(rvest)
library(stringr)
library(dplyr)

setwd(Sys.getenv("ACS_DIR")) # set working directory

# read in acs data
dt <- fread("data/p_2016_5yr.csv")

# data processing
## lower case to match stata
names(dt) <- tolower(names(dt)) 

## dummy for east asian country
east_asia_code <- c(5,7,8)
dt$easn <- (dt$rac3p %in% east_asia_code) * 1

## school coded variable
dt$deg <- ifelse(dt$schl <= 15, "no_hs",
       ifelse(dt$schl <= 19, "hs",
              ifelse(dt$schl <= 20, "associate",
                     ifelse(dt$schl <= 21, "bach",
                            ifelse(dt$schl <= 23, "master",
                                   ifelse(dt$schl <= 24, "doctorate"))))))

deg_coded <- as.data.table(dummy.code(dt$deg))
names(deg_coded) <- paste("deg", names(deg_coded), sep = "_")
dt <- cbind(dt, deg_coded)

# baseline regression
bl_reg_rhs <- c("easn","racwht","agep",names(dt)[grep("deg_", names(dt))])
bl_reg <- lm(as.formula(paste0("wagp~", paste(bl_reg_rhs, collapse = "+"))), dt)
summary(bl_reg)