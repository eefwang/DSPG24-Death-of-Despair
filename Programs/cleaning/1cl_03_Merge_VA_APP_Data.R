library(dplyr)
library(here)


#Import ACS Data
appalachia_acs_2015_2022 = read.csv(here("Data/merged_data", "appalachia_acs_2015_2022.csv"), sep = ",", header = TRUE)

#rename acs variables
appalachia_acs_2015_2022 <- appalachia_acs_2015_2022 %>%
  rename(FIPS = County.Code)

appalachia_acs_2015_2022$FIPS = as.integer(appalachia_acs_2015_2022$FIPS)

#Filter out 2018-2022
appalachia_acs_2018_2022 <-appalachia_acs_2015_2022 %>%
  filter(Year >= 2018 & Year <= 2022)

write.csv(appalachia_acs_2018_2022, 
          file = here("Data/merged_data", "appalachia_acs_2018_2022.csv"),
          row.names = FALSE) 

######Virginia data sets

#renamed all year and FIPS variables
vdh_opioid_deaths <- vdh_opioid_deaths %>%
  rename(Year = Death.Year,
         FIPS = VA.FIPS,
         VDH_death_rate = Death.Rate)

#select relevent variables and restrict sample from 2018-2022
mcod_total_DOD_1822 <- mcod_total_DOD %>%
  select(Year, FIPS, DOD_death_rate)%>%
  filter(Year >= 2018 & Year <=2022)

mcod_total_deaths_1822 <- mcod_total_deaths %>%
  select(Year, FIPS, total_death_rate) %>%
  filter(Year >= 2018 & Year <=2022)
#same number of observations (5496)

#select relevent variables and restrict sample from 2015-2019
mcod_total_DOD_1519 <- mcod_total_DOD %>%
  select(Year, FIPS, DOD_death_rate)%>%
  filter(Year >= 2015 & Year <=2019)

mcod_total_deaths_1519 <- mcod_total_deaths %>%
  select(Year, FIPS, total_death_rate) %>%
  filter(Year >= 2015 & Year <=2019)
#same number of observations (5500)


#rename and select virginia health rank data
va_health_rank <- va_health_rank %>%
  rename(Mental_health_providers = X..Mental.Health.Providers..count.,
         Primary_care_physicians = Primary.Care.Physicians,
         MHP_ratio = MHP.Ratio,
         PCP_ratio = PCP.Ratio,
         Median_household_income = Median.Household.Income,
         Avg_mentally_unhealthy_days = Average.Number.of.Mentally.Unhealthy.in.30.Days, 
         Poor_physical_health_days = Poor.Physical.Health.Days,
         Alcohol_impaired_driving_deaths = Alcohol.impaired.driving.deaths....,
         Excessive_drinking = Excessive.Binge.Drinking,
         Teen_birth_rate = Teen.Births.Rate,
         Poor_fair_health = Poor.or.Fair.health....,
         Adult_smoking = Adult.Smoking...Smokers.,
         Children_in_poverty = Children.in.Poverty,
         Severe_housing_problems = Severe.Housing.Problems...,
         Health_rank_population = Population) %>%
  select(Year, FIPS, Mental_health_providers, Primary_care_physicians, MHP_ratio, PCP_ratio, Median_household_income, Avg_mentally_unhealthy_days, Alcohol_impaired_driving_deaths, Excessive_drinking, Teen_birth_rate, Poor_fair_health, Adult_smoking, Children_in_poverty, Severe_housing_problems, Health_rank_population)


#get acs data in VA
virginia_acs_2018_2022 <- appalachia_acs_2018_2022 %>%
  filter(State_fip == 51) 

#merged VA data
virginia_2018_2022 <-virginia_acs_2018_2022 %>% #ACS VA data
  inner_join(mcod_total_deaths_1822, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD all death data
  inner_join(mcod_total_DOD_1822, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD DOD death data
  inner_join(va_health_rank, by = c("FIPS", "Year"), multiple = "all") %>% # merge to va health rank
  inner_join(vdh_opioid_deaths, by = c("FIPS", "Year"), multiple = "all") %>% # merge to vdh opioid deaths
  inner_join(ruc_codes, by = c("FIPS"), multiple = "all") # merge to ruc codes

#665 observations, all counties in VA is preserved


#get acs data in VA from 2015-2019
virginia_acs_2015_2022 <- appalachia_acs_2015_2022 %>%
  filter(State_fip == 51) 

#merged VA data
virginia_2015_2019 <-virginia_acs_2015_2022 %>% #ACS VA data
  inner_join(mcod_total_deaths_1519, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD all death data
  inner_join(mcod_total_DOD_1519, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD DOD death data
  inner_join(va_health_rank, by = c("FIPS", "Year"), multiple = "all") %>% # merge to va health rank
  inner_join(ruc_codes, by = c("FIPS"), multiple = "all") # merge to ruc codes

#665 observations, all counties in VA is preserved




####Appalachian data sets######

#merge death rates
appalachia_2018_2022 <- appalachia_acs_2018_2022 %>% #Appalachian States ACS data
  inner_join(appalachia_regions, by = c("FIPS"), multiple = "all") %>% #filter all counties in Appalachian
  inner_join(mcod_total_deaths_1822, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD all death data
  inner_join(mcod_total_DOD_1822, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD DOD death data
  inner_join(ruc_codes, by = c("FIPS"), multiple = "all") 

#2115 observations, all counties in Appalachian is preserved

#merge death rates
appalachia_2015_2019 <- appalachia_acs_2015_2022 %>% #Appalachian States ACS data
  inner_join(appalachia_regions, by = c("FIPS"), multiple = "all") %>% #filter all counties in Appalachian
  inner_join(mcod_total_deaths_1519, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD all death data
  inner_join(mcod_total_DOD_1519, by = c("FIPS", "Year"), multiple = "all") %>% #MCOD DOD death data
  inner_join(ruc_codes, by = c("FIPS"), multiple = "all") 

#2115 observations, all counties in Appalachian is preserved


#Export data sets

#write.csv(appalachia_2018_2022, file = here("Data/merged_data", "appalachia_2018_2022.csv"),row.names = FALSE) 
#write.csv(appalachia_2015_2019, file = here("Data/merged_data", "appalachia_2015_2019.csv"),row.names = FALSE) 
#write.csv(virginia_2018_2022,file = here("Data/merged_data", "virginia_2018_2022.csv"), row.names = FALSE) 
#write.csv(virginia_2015_2019,file = here("Data/merged_data", "virginia_2015_2019.csv"), row.names = FALSE) 

