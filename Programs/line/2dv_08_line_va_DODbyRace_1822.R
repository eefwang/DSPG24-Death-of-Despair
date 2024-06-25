
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(here)

#Data Import
VA_DOD_RACE_1822<- read.csv(here("Data/raw_data/MCOD VA 1822", "VA.DOD.RACE.1822.txt"), sep = "\t", header = TRUE)

#Prepare data
VA_DOD_RACE_1822 <- VA_DOD_RACE_1822 %>%
  select(Year,Single.Race.6,Crude.Rate) %>%
  na.omit()


# Clean and preprocess the data
VA_DOD_RACE_1822_clean <- VA_DOD_RACE_1822 %>%
  filter(!Crude.Rate %in% c("Suppressed", "Unreliable")) %>% # Remove suppressed and unreliable values
  mutate(Crude.Rate = as.numeric(Crude.Rate),                # Convert Crude Rate to numeric
         Year = as.integer(Year))                            # Convert Year to integer

# Plotting the line graph
ggplot(VA_DOD_RACE_1822_clean, aes(x = Year, y = Crude.Rate, color = `Single.Race.6`)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, show.legend = FALSE) +
  labs(title = "Virginia DOD Rate by Race (2018-2022)",
       x = "Year",
       y = "DOD Rate (per 100,000)",
       color = "Race",
       caption = "Data obtained by Natioanl Center for Health Statistics (CDC)\nNote: The data for Pacific Islanders
and American Natives is suppressed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.caption = element_text(hjust = 0.5, size = 9),
        guides(color = guide_legend(override.aes = list(shape = NA))))+
  scale_color_viridis(discrete = TRUE) 
