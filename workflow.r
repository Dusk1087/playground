# library(tidyverse)
# library(nycflights13)

short_flights <- flights |>
  filter(air_time < 60)

flights |>
  group_by(dest) |>
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |>
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 1,
    se = FALSE, #confidence internal
    color = "white",
    linewidth = 1
  ) +
  geom_point()

# Filter out rows with missing values in speed or distance
summary_data <- flights |> 
  group_by(dest) |> 
  summarize(
    distance = mean(distance, na.rm = TRUE),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  filter(!is.na(distance) & !is.na(speed))  # Ensure no missing values

# Fit the LOESS model
loess_fit <- loess(speed ~ distance, data = summary_data, span = .5)
loess_fit
# Add LOESS-smoothed values to the dataset
summary_data <- summary_data |> 
  mutate(smoothed_speed = predict(loess_fit))

# Display the first few rows of the dataset with smoothed values
print(head(summary_data))
ggplot(summary_data, aes(x = distance, y = speed)) +
  geom_point() +  # Original points
  geom_line(aes(y = smoothed_speed), color = "blue", size = 1)  # LOESS-smoothed curve

