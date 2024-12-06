library(tidyverse)
library(janitor)
library(nycflights13)

#group_by and summarize are used in order to calculate the average delay for each month.
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )

#prop is used to calculate the 10% of the total number of flights in each month.
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    prop = .1
  )

#Slice is used to select the row with the smallest value of dep_delay in each group.
#with_ties = FALSE is used to break ties.
flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n=1, with_ties = FALSE) |>
  relocate(dest)

# slice_head(n = 1) takes the first row from each group.
# slice_tail(n = 1) takes the last row in each group.
# slice_min(x, n = 1) takes the row with the smallest value of column x.
# slice_max(x, n = 1) takes the row with the largest value of column x.
# slice_sample(n = 1) takes one random row.

daily <- flights |> 
  group_by(year, month, day)
daily

#grouping data with multiple variables leads to issues and we use .group
daily_flight <- daily |> 
  summarize(
    n = n(),
    .group = 'keep'
    )
daily_flight

#ungrouping the data allows us to summarize the data without the grouping variables.
daily |> 
  ungroup() |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )

#.by allows grouping within a single operation
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  )

#.by multiple variables grouping within a single operation
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  )

#EXAMPLES
#which carrier has the worst average delays.  Challenge: can you disentangle the effects of bad airports vs. bad carriers?
flights |> 
  group_by(carrier) |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |> 
  slice_max(delay, n=1, with_ties = FALSE) |>
  relocate(carrier)
  
#which carrier has the worst average delays.  
flights |> 
  group_by(carrier) |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |> 
  slice_max(delay, n=1, with_ties = FALSE) |> 
  relocate(carrier)

#Challenge: can you disentangle the effects of bad airports vs. bad carriers?
flights |> 
  group_by(carrier, origin) |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n=n()
  )  |> 
  slice_max(delay, n=1, with_ties = FALSE) |> 
  relocate(origin, carrier)

#find the flights that are most delayed upon departure from each destination
flights |> 
  group_by(dest) |> 
  slice_max(dep_delay) |> 
  relocate(dest, carrier)
