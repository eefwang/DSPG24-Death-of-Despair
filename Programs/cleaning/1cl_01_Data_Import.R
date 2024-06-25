library(here)

#load in all data sets
#virginia specific datasets
va_health_rank = read.csv(here("Data/raw_data", "health_rank_data.csv"), sep = ",", header = TRUE)

vdh_opioid_deaths = read.csv(here("Data/raw_data","vdh-pud-overdose_deaths_by-fips_20240202.csv"))

#virginia and appalachia
ruc_codes = read.csv(here("Data/raw_data", "Ruralurbancontinuumcodes2023.csv"))

mcod_total_DOD = read.delim(here("Data/raw_data/MOCD 0922", "DOD.txt"), sep = ",", header = TRUE)

mcod_total_deaths = read.delim(here("Data/raw_data/MOCD 0922", "all_death.txt"), sep = ",", header = TRUE)

#appalachia specific datasets
appalachia_regions = read.csv(here("Data/raw_data","Subregions-in-Appalachia_2021_Data.csv"))