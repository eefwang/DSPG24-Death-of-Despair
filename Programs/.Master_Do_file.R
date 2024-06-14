#Master Do File
library(here)

##############
#Data Cleaning 
##############

#Import Data
rmarkdown::render(here("Programs", "00cl_Data_import.Rmd"))

#Retrieve ACS data for 2018-2022
rmarkdown::render(here("Programs", "01cl_ACS_Retrieval.Rmd"))

#Merge ACS, health rank, Death data, ruc codes for VA and Appalachian
rmarkdown::render(here("Programs", "02cl_Merge_VA_APP_Data.Rmd"))

#Merge Data with Map
#rmarkdown::render(here("Programs", "03cl_Merged_Map_Data.Rmd"))

#Merge Data with Map
rmarkdown::render(here("Programs", "04cl_Bartik_1822.Rmd"))

###################
#Data Visualization
###################






####################
#Regression Analysis
####################