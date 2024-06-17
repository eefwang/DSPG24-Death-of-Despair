#Master Do File
library(here)

##############
#Data Cleaning 
##############

#Import Data
rmarkdown::render(here("Programs", "1cl_01_Data_import.Rmd"))

#Retrieve ACS data for 2018-2022
rmarkdown::render(here("Programs", "1cl_02_ACS_Retrieval.Rmd"))

#Merge ACS, health rank, Death data, ruc codes for VA and Appalachian
rmarkdown::render(here("Programs", "1cl_03_Merge_VA_APP_Data.Rmd"))

#Merge Data with Map
#rmarkdown::render(here("Programs", "1cl_04_Merged_Map_Data.Rmd"))

#Merge Data with Map
rmarkdown::render(here("Programs", "1cl_05_Bartik_1822.Rmd"))

###################
#Data Visualization
###################






####################
#Regression Analysis
####################

# IV Regression