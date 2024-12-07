library(tidyverse)
library(nycflights13)

short_flights <- flights |> filter(air_time < 60)
