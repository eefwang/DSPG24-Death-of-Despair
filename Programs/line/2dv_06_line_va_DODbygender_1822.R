# Load necessary libraries
library(ggplot2)
library(dplyr)
library(here)
library(viridis)

#Data Import
VA_DOD_SEX_1822 <- read.csv(here("Data/raw_data/MCOD VA 1822", "VA.DOD.SEX.1822.txt"), sep = "\t", header = TRUE)

#Prepare data
VA_DOD_SEX_1822 <- VA_DOD_SEX_1822 %>%
  select(Year,Gender,Crude.Rate) %>%
  na.omit()

# Create the line plot
ggplot(data = VA_DOD_SEX_1822, aes(x = Year, y = Crude.Rate, color = Gender, group = Gender)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Virginia DOD Rates by Gender (2018-2022)",
       x = "Year",
       y = "Death Rate (per 100,000)",
       color = "Gender",
       caption = "Data obtained by National Center for Health Statistics (CDC)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 9))+
  scale_color_viridis(discrete = TRUE) 
