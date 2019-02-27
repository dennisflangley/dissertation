## Title -----------------------------------------------------------------------
##
## name: 01imputedata.R
## author: Dennis F Langley
## date: 2018-06-04
## what: Impute missingness in 2016 CCES, 2016 ANES, 
##               and ANES cumulative files
##       
####

## Preamble --------------------------------------------------------------------
rm(list = ls())
dev.off()
options(scipen = 999)
setwd("~/Documents/Dissertation/Data")

library(mice)
library(gdata)

source("~/Documents/Dissertation/Analysis/00customfunctions.R")

load("~/Documents/Dissertation/Analysis/00cleaneddata.RData")

## Imputation ------------------------------------------------------------------


apply(d.cces,2,percent_missing)
apply(d.anes,2,percent_missing)
apply(d.anes.p00,2,percent_missing)
apply(d.anes.p02,2,percent_missing)
apply(d.anes.p04,2,percent_missing)

tempdata.cces <- suppressWarnings(mice(data=d.cces, m = 5, maxit = 5, 
                                meth = "rf", seed = 32308, print = FALSE))

tempdata.anes <- suppressWarnings(mice(data=d.anes, m = 5, maxit = 5, 
                                meth = "rf", seed = 32308, print = FALSE))

tempdata.anes.p00 <- suppressWarnings(mice(data=d.anes.p00, m = 5, maxit = 5, 
                                meth = "rf", seed = 32308, print = FALSE))

tempdata.anes.p02 <- suppressWarnings(mice(data=d.anes.p02, m = 5, maxit = 5, 
                                meth = "rf", seed = 32308, print = FALSE))

tempdata.anes.p04 <- suppressWarnings(mice(data=d.anes.p04, m = 5, maxit = 5, 
                                meth = "rf", seed = 32308, print = FALSE))


d.cces.old <- d.cces
d.cces <- complete(tempdata.cces)
d.anes <- complete(tempdata.anes)
#d.anes.ts <- complete(tempdata.anes.ts)
d.anes.p00 <- complete(tempdata.anes.p00)
d.anes.p02 <- complete(tempdata.anes.p02)
d.anes.p04 <- complete(tempdata.anes.p04)

keep(d.cces.old, d.cces, d.anes, 
     d.anes.p00, d.anes.p02, d.anes.p04, sure = TRUE)

save.image("~/Documents/Dissertation/Analysis/01imputeddata.RData")

