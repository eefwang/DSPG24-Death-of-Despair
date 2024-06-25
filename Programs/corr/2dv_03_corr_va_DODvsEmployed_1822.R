# Load necessary libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(here)

virginia_2018_2022 = read.csv(here("Data/merged_data", "virginia_2018_2022.csv"), sep = ",", header = TRUE)

# Ensure the dataset is clean and filter the necessary columns
corr_data <- virginia_2018_2022 %>%
  mutate(
    employed_pop_pe = as.numeric(as.character(employed_pop_pe)),
  ) %>%
  select(employed_pop_pe, DOD_death_rate)

# Remove rows with NA values
corr_data <- na.omit(corr_data)

# Create a scatterplot with a regression line using viridis color palette
ggplot(corr_data, aes(x = employed_pop_pe, y = DOD_death_rate)) +
  geom_point(color = viridis(256)[50], size = 2, alpha = 0.7) +  # Use a consistent viridis color for points with transparency
  geom_smooth(method = "lm", color = viridis(256)[200], size = 1.5, se = FALSE) +  # Add a prominent regression line
  labs(title = "Virginia Employed population vs. DOD Rate (2018-2022)",
       x = "Employed population",
       y = "DOD Rate (per 100,000)",
       caption = "Data obtained from the American Community Survey and National Center for Health Statistics (CDC)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # Remove the legend
  )
