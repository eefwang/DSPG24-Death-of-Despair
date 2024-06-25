
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Load the dataset
VA.RACE.DOD <- read.delim("C:/Users/mutaalf/Desktop/DOD Code/VA plots/VA.DOD.RACE.1822.txt", sep = "\t", header = TRUE)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming the data is already loaded into VA.DOD.RACE.1822
# Read the data if not already done
# VA.DOD.RACE.1822 <- read.csv("/path/to/VA.DOD.RACE.1822.csv")

# Convert the data to a tibble for easier manipulation
#VA.DOD.RACE.1822 <- as_tibble(VA.RACE.DOD)

# View the first few rows of the data
head(VA.DOD.RACE.1822)

# Clean and preprocess the data
VA.DOD.RACE.1822_clean <- VA.DOD.RACE.1822 %>%
  filter(!Crude.Rate %in% c("Suppressed", "Unreliable")) %>% # Remove suppressed and unreliable values
  mutate(Crude.Rate = as.numeric(Crude.Rate),                # Convert Crude Rate to numeric
         Year = as.integer(Year))                            # Convert Year to integer

# Plotting the line graph
ggplot(VA.DOD.RACE.1822_clean, aes(x = Year, y = Crude.Rate, color = `Single.Race.6`)) +
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
        guides(color = guide_legend(override.aes = list(shape = NA))))
