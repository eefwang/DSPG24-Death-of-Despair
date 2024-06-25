#Master Do File
library(here)

##############
#Data Cleaning 
##############

#Import Data
source(here("Programs/cleaning", "1cl_01_Data_import.R"))

#Retrieve ACS data for 2018-2022
source(here("Programs/cleaning", "1cl_02_ACS_Retrieval.R"))

#Merge ACS, health rank, Death data, ruc codes for VA and Appalachian
source(here("Programs/cleaning", "1cl_03_Merge_VA_APP_Data.R"))

#Merge Data with Map
source(here("Programs/cleaning", "1cl_04_Merged_Map_Data.R"))

#Merge Data with Map
rmarkdown::render(here("Programs/cleaning", "1cl_05_Bartik_1822.Rmd"))

###################
#Data Visualization
###################





####################
#Regression Analysis
####################

# IV Regression