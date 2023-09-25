# R Script : 01 Histopathology_weekly_report_V01.R 
# Created on: 29/06/2022 (PLR)
# Updated on: 26/10/2022 (PLR) 


# AIM
# Input a set of Histopathology Excel reports created for each System and select
# specific fields and indicators for each reporting system. 

# OUTPUT 
# Output three Excel file in LONG format with the specific indicators formatted 
# and tailored for each system

# Each system output files naming convention : 
# High_level_Summary_SYSTEM_REPORT_DATE.xlsx
# Routine_Histopathology_SYSTEM_REPORT_DATE.xlsx
# Urgent_Histopathology_SYSTEM_REPORT_DATE.xlsx

# 1. Ensure Pacman is installed and loaded to proceed with the installation of required packages 
packages <- c("pacman") 
# Install packages not yet installed 
installed_packages <- packages %in% rownames(installed.packages()) if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) } 
# Packages loading 
invisible(lapply(packages, library, character.only = TRUE))

# Load required packages at once (readxl,dplyr,here,writexl,tidyverse)
# Libraries (readxl,dplyr,here,writexl,tidyverse)
pacman::p_load(readxl,dplyr,here,writexl,tidyverse)

# Check project folder location
here()

# 1. List files on "Input_files" folder
list.files('Input_files')

# INSTRUCTIONS> Select and run script up to line 210 to load three required functions


# Input files are a series of different individual files that each organisation submits on a weekly basis.
# Total number of input files in the /Input_files folder changes from week to week depending on the number of organisations reporting

# # Function section Tab 01-03: HLS_diagnostics_report()
# Gets data from Excel input files TAB 01-03: TAB01 HLS. High Level Summary report
# # Function section Tab 02-03: UH_diagnostics_report()
# Gets data from Excel input files TAB 02-03: TAB02. Urgent Histopathology
# # Function section Tab 03-03: RH_diagnostics_report()
# Gets data from Excel input files TAB 03-03: TAB 03. Routine Histopathology

# PROGRAM OVERALL STRUCTURE

## SECTION 01: Define function to collect specific data from original excel file
## SECTION 02: Loop through each file in the input folder applying each function to individual files. 


# Load functions in global environment

## SECTION 01: Define function to collect specific data from original excel file

# TAB01 HLS. High Level Summary report
# Part 01-03
HLS_diagnostics_report <-function(DataA,NetworkA,WeekA) {  
    
    # TAB 01/03: High Level Summary tab
    #[.................................]
             
    # Tab0103OUTPUT > HLS tab formatted output  
                
    # 1. Import required tables from Excel file  
    Tab10202in <- read_excel(here("Input_files",DataA),sheet = 1,range = "C10:F18",skip = 1,na = "")
              
    Tab0103<- Tab10202in %>% pivot_longer(names_to = "metric", cols = 2:ncol(Tab10202in))
       
    # 2. Add network name column to output table
    # B. Provide Excel file name for InfileB HLS_tab function argument  
    Network_name_prep <- read_excel(here("Input_files",NetworkA),sheet = 1,range = "C3:D4",skip = 1,na = "")
              
    Network_name_prep1 <-  Network_name_prep %>% select(starts_with("...2")) 
    Network_name_prep2 <- Network_name_prep %>% select(Network_name = "...2") 
 
              # 3. Add week ending column to output table             
              # C. Provide EXcel file name for InfileC HLS_tab function argument 
              Week_ending_prep <- read_excel(here("Input_files",WeekA),sheet = 1,range = "C7:D8",skip = 1,na = "")

              Week_ending <-  Week_ending_prep %>% 
                rename(exclude = 1,Week_ending =2) %>% 
                # Turn  POSIXct date into date format in R
                mutate(week_ending = as.Date(Week_ending)) %>% 
                select(week_ending)
              # Now we merge it with original table
              Tab0103OUTPUT <- cbind.data.frame(Tab0103,Network_name_prep2,Week_ending)
              # OUTPUT DYNAMIC FILE NAME
              # NEW script OUTPUT file name using input dynamic file name
              # High Level Summary
              File_name <- basename(here("Input_files",DataA))
              Ouptut_Name <-paste0("High_level_Summary_",File_name)
              write_xlsx(Tab0103OUTPUT,here("Output_files",Ouptut_Name))
              
}    

# TAB02 UH
# Part 02-03
UH_diagnostics_report <-function(DataB,NetworkB,WeekB) {
  
  # TAB 02/03: Urgent Histopathology
  #[................................]
  
  # 1. Import required tables from Excel file
  TAB2input <- read_excel(here("Input_files",DataB),sheet = 2,range = "C4:H254",skip = 1,na = "") # Specify NA values
  
  TAB2input_subset <- TAB2input[ , c("Speciality")]    
  
  # …and then we can apply the complete cases function to exclude all rows of our original data based on this subset:
  TAB2notna <- TAB2input[complete.cases(TAB2input_subset),]
  
  # Replicate calculations to obtain the share of "Reported within 7 days" and *Reported beyond 7 days" as in the templae
  # Total reported
  TAB2calc_prep <- TAB2notna %>% 
    mutate(
      Total_Rported = `Reported beyond 7 days`+ `Reported within 7 days`,
      Reported_within_7D = (`Reported within 7 days`/ Total_Rported),
      Reported_beyond_7D = (`Reported beyond 7 days`/ Total_Rported)
    )
  
  
  Tab0203 <-TAB2calc_prep %>% select("Speciality","site", "Reported within 7 days" ,"Reported beyond 7 days",
                                     "Total Unreported",  "Total_Rported" , "Reported_within_7D","Reported_beyond_7D")
  
  # 2. Add network name column to output table 
  # B. Provide EXcel file name for InfileB HLS_tab function argument  
  
  Network_name_prep <- read_excel(here("Input_files",             
                                       NetworkB),sheet = 1,range = "C3:D4",skip = 1,na = "")
  
  Network_name_prep1 <-  Network_name_prep %>% select(starts_with("...2")) 
  Network_name_prep2 <- Network_name_prep %>%  select(Network_name = "...2") 
  
  # 3. Add week ending column to output table
  # C. Provide EXcel file name for InfileC HLS_tab function argument 
  Week_ending_prep <- read_excel(here("Input_files",             
                                      WeekB),
                                 sheet = 1,range = "C7:D8",skip = 1,na = "")
  
  
  # Format the date and column name to merge it with original table  
  Week_ending <-  Week_ending_prep %>% 
    rename(exclude = 1,Week_ending =2) %>% 
    # Turn  POSIXct date into date format in R
    mutate(week_ending = as.Date(Week_ending)) %>% 
    select(week_ending)
  Week_ending
  
  # Now we merge new columns with initial table
  Tab0203OUTPUT<- cbind.data.frame(Tab0203,Network_name_prep2,Week_ending)
  
  # Output HLS_tab output 
  # Urgent Histopathology
  File_name <- basename(here("Input_files",DataB))
  Ouptut_Name <-paste0("Urgent Histopathology_",File_name)
  write_xlsx(Tab0203OUTPUT,here("Output_files",Ouptut_Name))
  
}

# TAB03 RH
# Part 03-03
RH_diagnostics_report <-function(DataC,NetworkC,WeekC) {            
  
  # TAB 03/03: Routine Histopathology
  #[..........................]
  
  # 1 BUILD MAIN TABLE
  
  # A. Provide EXcel file name for InfileA HLS_tab function argument 
  TAB3input <- read_excel(here("Input_files",DataC),sheet = 3,range = "C4:H254",skip = 1,na = "") # Specify NA values
  TAB3input
  
  TAB3input_subset <- TAB3input[ , c("Speciality")]      
  
  # …and then we can apply the complete cases function to exclude all rows of our original data based on this subset:
  TAB3notna <- TAB3input[complete.cases(TAB3input_subset),]
  
  # Replicate calculations to obtain the share of "Reported within 7 days" and *Reported beyond 7 days" as in the templae
  # Total reported
  TAB3calc_prep <- TAB3notna %>% 
    mutate(
      Total_Rported = `Reported beyond 10 days`+ `Reported within 10 days`,
      Reported_within_10D = (`Reported within 10 days`/ Total_Rported),
      Reported_beyond_10D = (`Reported beyond 10 days`/ Total_Rported)
    )
  
  names(TAB3calc_prep)
  
  TAB3calc <-TAB3calc_prep %>%  select("Speciality","site", "Reported within 10 days" ,"Reported beyond 10 days",
                                       "Total Unreported",  "Total_Rported" , "Reported_within_10D","Reported_beyond_10D")
  
  
  # Add network name column to output table 
  # B. Provide EXcel file name for InfileB HLS_tab function argument 
  Network_name_prep <- read_excel(here("Input_files",NetworkC),sheet = 1,range = "C3:D4",skip = 1,na = "")
  
  Network_name_prep1 <-  Network_name_prep %>%  select(starts_with("...2")) 
  Network_name_prep2 <- Network_name_prep %>%   select(Network_name = "...2") 
  
  # 3 Add week ending column to output table
  
  # C. Provide EXcel file name for InfileC HLS_tab function argument 
  Week_ending_prep <- read_excel(here("Input_files",WeekC),sheet = 1,range = "C7:D8",skip = 1,na = "")
  
  
  Week_ending <-  Week_ending_prep %>% 
    rename(exclude = 1,Week_ending =2) %>% 
    # Turn  POSIXct date into date format in R
    mutate(week_ending = as.Date(Week_ending)) %>% 
    select(week_ending)
  Week_ending
  
  # Now we merge new columns with initial table
  TABLE0303OUTPUT  <- cbind.data.frame(TAB3calc,Network_name_prep2,Week_ending)
  TABLE0303OUTPUT
  
  # Output RH_tab output 
  # Routine Histopathology
  File_name <- basename(here("Input_files",DataC))
  Ouptut_Name <-paste0("Routine_Histopathology_",File_name)
  write_xlsx(TABLE0303OUTPUT,here("Output_files",Ouptut_Name))
  
  
}  

# Run up to this line to load ALL THREE required functions to produce final output


## SECTION 02: Loop through each file in the input folder applying each function to individual files. 

# INVOKE FUNCTIONS

# Assign total number in Input_files folder to an object
Input_files <- list.files('Input_files')
Input_files

# NOW WE SACAN "Input_files" folder content to apply HLS_fiagnostics_report()
# function to each of the individual files in the folder.
# No need to input manually each file name from Input_files folder: 

# FUNCTION 01-03 
# HLS_diagnostics_report()
for (i in (1:length(Input_files))){
  HLS_diagnostics_report(Input_files[i],Input_files[i],Input_files[i])
  print(Input_files[i])
}

# FUNCTION 02-03 
# UH_diagnostics_report()
for (i in (1:length(Input_files))){
  UH_diagnostics_report(Input_files[i],Input_files[i],Input_files[i])
  print(Input_files[i])
}

# FUNCTION 03-03 
# RH_diagnostics_report()
for (i in (1:length(Input_files))){
  RH_diagnostics_report(Input_files[i],Input_files[i],Input_files[i])
  print(Input_files[i])
}


### BONUS
# Run line below using source() function on a new r Script to trigger the whole report and output creation:
# source("01 Histopathology_weekly_report_V01.R")




















