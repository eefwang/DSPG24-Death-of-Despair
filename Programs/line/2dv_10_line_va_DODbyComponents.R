library(readr)
library(dplyr)
library(viridis)
library(ggplot2)
library(readxl)
library(here)

# Import dataset
merged_state_death <- read_excel(here("Data/raw_data/MCOD 18-22 BY STATE", "merged_state_death.xlsx"))

# Create summary data frame
merged_state_death_1822 <- merged_state_death %>%
  filter(State == "Virginia") %>%
  mutate(
    alcohol_deaths = as.numeric(alcohol_deaths),
    drugs_deaths = as.numeric(drugs_deaths),
    suicide_deaths = as.numeric(suicide_deaths),
    Population = as.numeric(Population)
  ) %>%
  mutate(alcohol_death_rate = alcohol_deaths / Population * 100000,
         drugs_death_rate = drugs_deaths / Population * 100000,
         suicide_death_rate = suicide_deaths / Population * 100000)

ggplot(merged_state_death_1822, aes(x = Year)) +
  geom_line(aes(y = alcohol_death_rate, color = "Alcohol"), size = 1.5) +
  geom_line(aes(y = drugs_death_rate, color = "Drugs"), size = 1.5) +
  geom_line(aes(y = suicide_death_rate, color = "Suicide"), size = 1.5) +
  scale_color_viridis_d() +  # Use discrete colors from viridis
  labs(
    title = "Virginia Deaths of Despair by Components 2018-2022",
    x = "Year",
    y = "Death Rate (per 100,000)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )