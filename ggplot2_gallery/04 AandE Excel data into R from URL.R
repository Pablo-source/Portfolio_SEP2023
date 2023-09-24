# 04 Import A&E Excel data into R from URL 

#Searching for the href HTML tag we can find the corresponding .xls file to import it into R
#<p><a href="https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Timeseries-monthly-Unadjusted-9kidr.xls">Unadjusted: Monthly A&amp;E Time series April 2019 (XLS, 364K)</a><br />


# This is an .xls file extension, Excel 97-Excel 2003 Workbook , The Excel 97 - Excel 2003 Binary file format (BIFF8).
# We can import both .xls and .xlsx file using download.file() function from readxl package


# 1. Ensure Pacman is installed and loaded to proceed with the installation of required packages 
packages <- c("pacman") 
# Install packages not yet installed 
installed_packages <- packages %in% rownames(installed.packages()) if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) } 
# Packages loading 
invisible(lapply(packages, library, character.only = TRUE))

# Load 
pacman::p_load(readxl,here,dplyr,janitor) 

# 1. Import data into R
AE_data <- function() {
  
  if(!dir.exists("data")){dir.create("data")}
  
  # England-level time series
  # Download Excel file to a Project sub-folder called "data"
  # Created previously using an adhoc project structure function
  
  xlsFile = "AE_England_data.xls"
  
  download.file(
    url = 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Timeseries-monthly-Unadjusted-9kidr.xls',
    destfile = here("data",xlsFile),
    mode ="wb"
  )
  
}
# Download A&E data function (no arguments)
AE_data()