# Load necessary libraries
library(dplyr)
library(ggplot2)
library(viridis)

# Ensure the dataset is clean and filter the necessary columns
corr_data <- virginia_2018_2022 %>%
  mutate(
    unemployed_pop = as.numeric(as.character(unemployed_pop_pe)),
    DOD_death_rate = (as.numeric(as.character(DOD_death_rate))*100)
  ) %>%
  select(unemployed_pop_pe, DOD_death_rate)

# Remove rows with NA values
corr_data <- na.omit(corr_data)

# Create a scatterplot with a regression line using viridis color palette
ggplot(corr_data, aes(x = unemployed_pop_pe, y = DOD_death_rate)) +
  geom_point(color = viridis(256)[50], size = 2, alpha = 0.7) +  # Use a consistent viridis color for points with transparency
  geom_smooth(method = "lm", color = viridis(256)[200], size = 1.5, se = FALSE) +  # Add a prominent regression line
  labs(title = "Unemployed Population and DOD Rate",
       x = "Unemployed Population",
       y = "DOD Rate",
       caption = "Data obtained from the American Community Survey and Center for Disease Control and Prevention") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # Remove the legend
  )