library(readr)
library(dplyr)
library(viridis)
library(ggplot2)

setwd("/Users/yifanwang/Desktop/MCOD 18-22 BY STATE")
#import dataset
year.state.all_death <- read.delim("~/Desktop/MCOD 18-22 BY STATE/year-state-all_death.txt")
year.state.DOD <- read.delim("~/Desktop/MCOD 18-22 BY STATE/year-state-DOD.txt")
year.state.alcohol <- read.delim("~/Desktop/MCOD 18-22 BY STATE/year-state-alcohol.txt")
year.state.drugs <- read.delim("~/Desktop/MCOD 18-22 BY STATE/year-state-drugs.txt")
year.state.suicide <- read.delim("~/Desktop/MCOD 18-22 BY STATE/year-state-suicide.txt")

#aggregate 5 years all death data (you can cahnge for other datasets)
agg_alldeath <- year.state.all_death %>%
  group_by(State, State.Code) %>%  # Group by both State and State.Code
  summarise(
    Total_Deaths = sum(Deaths),
    Total_Population = sum(Population),
    all_death_rate = (Total_Deaths / Total_Population) * 100000,  # Death rate per 100,000 population
    .groups = "drop"  # Optional: drop the grouping once summarisation is done
  )%>%
  na.omit(agg_alldeath)

###########
#Things you can do before you have the merged data:
#1. merge the aggregate data with shepfile and do a map visualization on all death/DOD
#2. draw bar plots: DOD appalachia vs. non-appalachian states (why DOD is an important topic for appalachia area), 
#3. use the file merged_state_death in Teams, draw bar plots on what % of DOD is drug/alcohol/suicide (what is the main casue of DOD)
###########


# Create summary data frame
merged_state_death_2022 <- merged_state_death %>%
  filter(State == "Virginia")%>%
  mutate(
    alcohol_deaths = as.numeric(alcohol_deaths),
    drugs_deaths = as.numeric(drugs_deaths),
    suicide_deaths = as.numeric(suicide_deaths),
    Population = as.numeric(Population))%>%
  mutate(alcohol_death_rate = alcohol_deaths/Population,
         drugs_death_rate = drugs_deaths/Population,
         suicide_death_rate = suicide_deaths/Population)

ggplot(merged_state_death_2022, aes(x = Year, y = alcohol_death_rate, color = "Alcohol")) +
  geom_line() +
  geom_line(aes(y = drugs_death_rate, color = "Drugs")) +
  geom_line(aes(y = suicide_death_rate, color = "Suicide")) +
  scale_color_viridis_d() +  # Use discrete colors from viridis
  labs(
    title = "Virginia Deaths of Despair by Causes 2018-2022",
    x = "Year",
    y = "Death Rate"
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
