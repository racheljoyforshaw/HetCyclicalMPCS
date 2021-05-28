# ActiveSaving Master

# clear
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/Research/ActiveSaving/")

# packages
library(survey)    # load survey package (analyzes complex design surveys)   
library(plyr)      # load package needed to use 'join_all'
library(tidyr)     # load package needed to go from wide to long data
library(reshape2)  # load package needed to go from wide to long format
library(xtable) # pretty latex tables
library(data.table) # easy subsetting
library(gdata) # remove.vars
library(ggplot2) # plotting
library(dplyr) # summarise data 
library(srvyr) # dplyr for survey
library(magrittr) # pipes
library(jtools)
library(quantreg) # quantile regression
library(modelr) # add residuals to dataset
library(AER) # for iv regression
library(ivpack) # for iv ci
library(gtools) # for significance stars in reg
library(systemfit) # for SUR
library(stargazer) # for ivreg latex output
library("ggpubr") # pretty plots
library("plm")
library(survey)    # load survey package (analyzes complex design surveys)   


# option for survey - if lonely psu the stratum contribution to the variance is taken to be the average of all the strata with more than one PSU
options(survey.lonely.psu="average")

# import data and clean
source('DoFiles/CleanData.R')

# calculate after-tax income
source('DoFiles/TAXSIM.R')

# calculate active savings
#source('DoFiles/CalcActiveSavings.R')

# select sample
source('DoFiles/SampleSelection.R')

# compare PSID and CEX variables
#source('DoFiles/compareConsumptionAgg.R')

# DSZ way of getting MPS, MPC
#source('DoFiles/DSZ.R')

# active saving components
#source('DoFiles/AScomponents.R')

#KV
#source('DoFiles/KV_IV_w_impS.R')
#source('DoFiles/KV_IV_w_AS.R')
#source('DoFiles/KV_IV_w_impC.R')
#source('DoFiles/KV_IV_w_C.R')

#source('DoFiles/KV_unweightedIV.R')
#source('DoFiles/KV_test.R')
#source('DoFiles/KV_unweighted.R')
#source('DoFiles/KV_capChange.R')
#source('DoFiles/KV_testAS2.R')
#source('DoFiles/KV_balanced_2.R')

# IVQR
source('DoFiles/IVQR.R')

