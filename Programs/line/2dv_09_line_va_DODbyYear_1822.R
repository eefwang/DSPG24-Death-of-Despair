# Load necessary libraries
library(ggplot2)
library(dplyr)
library(here)

#Data Import
VA_DOD_1822 <- read.csv(here("Data/raw_data/MCOD VA 1822", "VA.DOD.1822.txt"), sep = "\t", header = TRUE)

#Prepare data
VA_DOD_1822 <- VA_DOD_1822 %>%
  select(Year,Crude.Rate) %>%
  na.omit()

# Create the line plot
ggplot(data = VA_DOD_1822, aes(x = Year, y = Crude.Rate)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Virginia Deaths of Despair (2018-2022)",
       x = "Year",
       y = "Death Rate (per 100,000)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_viridis(discrete = TRUE) 
