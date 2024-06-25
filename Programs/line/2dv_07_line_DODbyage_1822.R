# Load necessary libraries
library(ggplot2)
library(dplyr)
library(here)

#Data Import
VA_DOD_AGE_1822<- read.csv(here("Data/raw_data/MCOD VA 1822", "VA.DOD.AGE.1822.txt"), sep = "\t", header = TRUE)

#Prepare data
VA_DOD_AGE_1822 <- VA_DOD_AGE_1822 %>%
  select(Year,Ten.Year.Age.Groups,Crude.Rate) %>%
  na.omit()

# Create the line plot
ggplot(data = VA_DOD_AGE_1822, aes(x = Year, y = Crude.Rate, color = Ten.Year.Age.Groups, group = Ten.Year.Age.Groups)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Virginia DOD Rate by Age Group (2018-2022)",
       x = "Year",
       y = "DOD Rate (per 100,000)",
       color = "Age Group",
       caption = "Data obtained by National Center of Health Statistics (CDC)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.7),
        plot.caption = element_text(hjust = 0.5, size = 9))+
  scale_color_viridis(discrete = TRUE) 
