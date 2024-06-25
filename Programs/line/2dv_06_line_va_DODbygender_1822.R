# Load necessary libraries
library(ggplot2)
library(dplyr)


# Data inspection
head(VA_DOD_SEX_1822)
str(VA_DOD_SEX_1822)

# Rename columns to remove spaces
colnames(VA_DOD_SEX_1822) <- make.names(colnames(VA_DOD_SEX_1822))

# Check the column names
colnames(VA_DOD_SEX_1822)

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
        plot.caption = element_text(hjust = 0.5, size = 9))