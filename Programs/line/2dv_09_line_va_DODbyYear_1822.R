# Load necessary libraries
library(ggplot2)
library(dplyr)


# Data inspection
print(head(VA_DOD_1822))
print(str(VA_DOD_1822))

# Rename columns to remove spaces
colnames(VA_DOD_1822) <- make.names(colnames(VA_DOD_1822))

# Check the column names
print(colnames(VA_DOD_1822))

# Create the line plot
ggplot(data = VA_DOD_1822, aes(x = Year, y = Crude.Rate)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Virginia Deaths of Despair (2018-2022)",
       x = "Year",
       y = "Death Rate (per 100,000)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
