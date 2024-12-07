library(tidyverse)
library(nycflights13)

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
    span = 0.5,
    se = FALSE, #confidence internal
    color = "white", 
    linewidth = 4
  ) +
  geom_point()
