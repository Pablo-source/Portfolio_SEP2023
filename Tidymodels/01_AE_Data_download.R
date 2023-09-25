# Script: 01 A_E_data_download.R


# 1. Ensure Pacman is installed and loaded to proceed with the installation of required packages 
packages <- c("pacman") 
# Install packages not yet installed 
installed_packages <- packages %in% rownames(installed.packages()) 
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) } 

# Packages loading 
invisible(lapply(packages, library, character.only = TRUE))

# AE_Attendances_charts.R
# Plot NHS Statistics 
pacman::p_load(renv,readxl,here,dplyr,janitor) 

# 1. Download AE data from NHS England website

AE_data <- function() {
  if(!dir.exists("data")){dir.create("data")}
  # NHS England. A&E Attendances and Emergency Admissions statistics
  # https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/
  # England-level time series
  # Download Excel file to a Project sub-folder called "data"
  xlsFile = "AE_England_data.xls"
  
  download.file(
    url = 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Timeseries-monthly-Unadjusted-9kidr.xls',
    destfile = here("data",xlsFile),
    mode ="wb"
  )
  
}
# Download A&E data function (no arguments)
AE_data()

# 1. Load in data

# From file  AE_England_data.xls
AE_data_subset<- read_excel(
  here("data", "AE_England_data.xls"), 
  sheet = 1, skip =17) %>% 
  clean_names() %>% 
  select(
    "x1",                                                                      
    "period",                                                                  
    "type_1_departments_major_a_e",                                            
    "type_2_departments_single_specialty",                                     
    "type_3_departments_other_a_e_minor_injury_unit",                          
    "total_attendances" 
  )
AE_data_subset

AE_data_analysis <- AE_data_subset %>% 
  select(-x1)
AE_data_analysis

# 2. Then we rename remaining variables to shorten their names

AE_data_analysis_prep <- AE_data_analysis %>% 
  select(
    period,
    type_1_Major_att = type_1_departments_major_a_e,
    type_2_Single_esp_att = type_2_departments_single_specialty,
    type_3_other_att = type_3_departments_other_a_e_minor_injury_unit,
    total_att = total_attendances
    
  ) 
AE_data_analysis_prep

# 3. Subset Type 1 Attendances data to model TS using parsnip package

AEATT_data <- AE_data_analysis_prep %>% select(period, type_1_Major_att,type_2_Single_esp_att,type_3_other_att) 
AEATT_data
