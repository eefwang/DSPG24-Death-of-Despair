# Load necessary libraries
library(dplyr)
library(corrplot)
library(viridis)
library(here)

virginia_2018_2022 = read.csv(here("Data/merged_data", "virginia_2018_2022.csv"), sep = ",", header = TRUE)

# Assuming your data frame is called appalachia_2018_2022
# Ensure the dataset is clean and filter the necessary columns
corr_data <- virginia_2018_2022 %>%
  mutate(
    public_admin = as.numeric(as.character(public_admin)),
    other_services = as.numeric(as.character(other_services)),
    #arts_ent_rec = as.numeric(as.character(arts_ent_rec)),
    edu_health_social = as.numeric(as.character(edu_health_social)),
    #prof_scientific = as.numeric(as.character(prof_scientific)),
    finance_insurance = as.numeric(as.character(finance_insurance)),
    #information = as.numeric(as.character(information)),
    transportation = as.numeric(as.character(transportation)),
    retail_trade = as.numeric(as.character(retail_trade)),
    wholesale_trade = as.numeric(as.character(wholesale_trade)),
    manufacturing = as.numeric(as.character(manufacturing)),
    construction = as.numeric(as.character(construction)),
    agriculture_mining = as.numeric(as.character(agriculture_mining)),
    #production = as.numeric(as.character(production)),
    #natural_resources = as.numeric(as.character(natural_resources)),
    #sales = as.numeric(as.character(sales)),
    #service = as.numeric(as.character(service)),
    #management = as.numeric(as.character(management)),
    #armed_forces = as.numeric(as.character(armed_forces)),
    VDH_death_rate = as.numeric(as.character(VDH_death_rate)),
    DOD_death_rate = as.numeric(as.character(DOD_death_rate))
  ) %>%
  select(public_admin, other_services, edu_health_social,
         finance_insurance, transportation, retail_trade, wholesale_trade,
         manufacturing, construction, agriculture_mining,VDH_death_rate, DOD_death_rate)

# Remove rows with NA values
corr_data <- na.omit(corr_data)

# Update variable names for better readability
colnames(corr_data) <- c("Public Admin", "Other Services", "Education, Health, Social",
                         "Finance, Insurance", "Transportation",
                         "Retail Trade", "Wholesale Trade", "Manufacturing", "Construction", "Agriculture, Mining","Opioid Death Rate", "DOD Death Rate")

# Scatterplot matrix
pairs(corr_data, pch = 18, upper.panel = NULL, main = "Virginia Counties Scatterplot Matrix")

# Calculate the correlation matrix
corr_matrix <- cor(corr_data)

# Define breaks for the correlation scale
breaks <- seq(-1, 1, by = 0.25)

# Define custom color palette with viridis
corr_colors <- viridis::viridis(length(breaks) - 1)

# Plot the correlation matrix with customized color legend
corrplot(corr_matrix, method = "color", type = "lower", col = corr_colors, 
         title = "Virginia Occupations Correlation Plot", mar = c(0, 2, 2, 4),  # Increase right margin
         cl.length = length(breaks), 
         cl.cex = 0.8, cl.align.text = "r", cl.ratio = 0.2, 
         tl.cex = 0.9, 
         tl.col = "black", tl.srt = 30, # Set text rotation to 30 degrees
         addCoef.col = "black", number.cex = 0.6, cl.offset = 0.5, # Adjust the size of the numbers inside boxes and the legend
         addgrid.col = "black")
