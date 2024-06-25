# Load necessary libraries
library(ggplot2)
library(dplyr)

# Data inspection
print(head(VA_DOD_AGE_1822))
print(str(VA_DOD_AGE_1822))

# Rename columns to remove spaces
colnames(VA_DOD_AGE_1822) <- make.names(colnames(VA_DOD_AGE_1822))

# Check the column names
print(colnames(VA_DOD_AGE_1822))

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
        plot.caption = element_text(hjust = 0.5, size = 9))
