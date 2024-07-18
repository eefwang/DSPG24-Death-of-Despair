# Load necessary libraries
library(ggplot2)
library(dplyr)
library(here)
library(viridis)
library(here)

#Data Import
VA_DOD_SEX_1822 <- read.csv(here("Data/raw_data/MCOD VA 1822", "VA.DOD.SEX.1822.txt"), sep = "\t", header = TRUE)

#Prepare data
VA_DOD_SEX_1822 <- VA_DOD_SEX_1822 %>%
  select(Year,Gender,Crude.Rate) %>%
  na.omit()

# Create the line plot
ggplot(data = VA_DOD_SEX_1822, aes(x = Year, y = Crude.Rate, color = Gender, group = Gender)) +
  geom_line(size = 1.5) +  # Line thickness
  geom_point(size = 2) +  # Point size
  labs(x = "Year",
       y = "DOD Rate (per 100,000)",
       color = "Sex",
       caption = "Data obtained by National Center for Health Statistics (CDC)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5, size = 9),
        guides(color = guide_legend(override.aes = list(shape = NA))))+
  scale_color_viridis(discrete = TRUE) 
