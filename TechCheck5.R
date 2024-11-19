#######################################################
## TECH CHECK 5
######################################################
library(nycflights13)
library(dplyr)

head(weather)

## Question 1
##Group the temperature in ascending order by month using the IQR values to represent variability:

weather_summary <- weather %>%
  group_by(month) %>%
  summarize(IQR_temp = IQR(temp, na.rm = TRUE)) %>%
  arrange(IQR_temp)  

print(weather_summary)

## Question 2
# Join flights and weather data
combined_data <- inner_join(flights, weather, by = c("year", "month", "day", "hour", "origin"))

# Calculations
result <- combined_data %>%
  group_by(month) %>%
  summarize(
    Median_Gain = round(median((dep_delay - arr_delay) / (air_time / 60), na.rm = TRUE), 6),
    Mean_Temp_Celsius = round(mean((temp - 32) / 1.8, na.rm = TRUE), 6),
    Missing_Temp = sum(is.na(temp))  
  ) %>%
  arrange(Median_Gain)  

# Use sprintf to ensure six decimal places
result$Median_Gain <- sprintf("%.6f", result$Median_Gain)
result$Mean_Temp_Celsius <- sprintf("%.6f", result$Mean_Temp_Celsius)

# Display the result
print(result)