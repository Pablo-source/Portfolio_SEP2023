# R Script: 01 PRED MODEL LIVE task DEC 2019

# 01 PRED MODEL LIVE SERVERtask.R | Schedule model run daily at 14H
# Tasckschedule R (README)
# https://cran.r-project.org/web/packages/taskscheduleR/readme/README.html
# TaskcheduleR
# https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html
# Load required libraries
library("rlang", lib.loc="C:/R/R-3.4.4/library")
# 1 Install taskschedulerR package
install.packages("taskscheduleR")
# 2 Load library
library("taskscheduleR")
# 3 OPEN THE ADD IN
taskscheduleR:::taskschedulerAddin()
# IMPORTANT DON'T LEAVE ANY EMPTY SPACES ON .R script saved in \extdata folder
# R Script should be "PredmodelATTADM.R"
# Setup script to schedule task
###########################################################################################
# SCHEDULE MODEL RUN DAILY AT 12:05H
# 1) Task: PREDMODEL1205
# 2) R Script@ PredmodelATTADMALL.R
# 3) r script: PREDMODEL <- system.file("extdata", "PredmodelATTADM.R", package = "taskscheduleR")
PREDMODEL <- system.file("extdata", "PredmodelATTADMALL.R", package = "taskscheduleR")
taskscheduler_create(taskname = "PREDMODEL1205", rscript = PREDMODEL,
                     schedule = "DAILY", starttime = "12:05")
##----------------------------------------------------------------------------------###
# ONE OFF SCHEDULLED RUN
# PREDMODELoneoff <- system.file("extdata", "PredmodelATTADMALLONEOFF.R", package = "taskscheduleR")
#
#taskscheduler_create(taskname = "PREDMODELoneoff", rscript = PREDMODELoneoff,
#
# schedule = "ONCE", starttime = format(Sys.time() + 60, "%H:%M"))
##----------------------------------------------------------------------------------###
## NEW PLOTS TO COMPARE ACTUAL VS PREDICTED AND HITORIC VALUES
# 1) Task: PLOTSCHK
# 2) Scheduled to run daily at 17:20h
PLOTSCHK <- system.file("extdata", "PLOTSACTFORECASTHISV8.R", package = "taskscheduleR")
taskscheduler_create(taskname = "PLOTSCHK", rscript = PLOTSCHK,
                     schedule = "DAILY", starttime = "17:20")
## plots one off
PLOTSCHKONCE <- system.file("extdata", "PLOTSACTFORECASTHISV8.R", package = "taskscheduleR")
taskscheduler_create(taskname = "PLOTSCHKONCE", rscript = PLOTSCHK,
                     schedule = "ONCE", starttime = format(Sys.time() + 60, "%H:%M"))
##----------------------------------------------------------------------------------###
## delete the tasks
# The above code refers to the taskname= "ModelTimeSTMPonce" line craated in
taskscheduler_delete(taskname = "PREDMODEL1220")
taskscheduler_delete(taskname = "PLOTSCHK")
## get a data.frame of all tasks
tasks <- taskscheduler_ls()
str(tasks)
# Remove old objects
rm(PREDMODELtest)
rm(TEST01)