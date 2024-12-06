library(tidyverse)
library(janitor)
library(nycflights13)
library(friends)

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
  slice_max(dep_delay, n = 2) |> 
  relocate(dest, flight,dep_delay)

#How do delays vary over the course of the day? Illustrate your answer with a plot.
delays_by_hour <- flights |> 
  group_by(hour) |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE)
  )
delays_by_hour |>   
  ggplot(aes(x = hour, y = delay)) +
  geom_point(na.rm = TRUE)+
  geom_line(na.rm = TRUE) +
  labs(
    title = "Average flight delay per hour",
    x = "hour of the day",
    y = "average delay (min)"
  ) +
  theme(
    plot.title = element_text(hjust =.5)
  )
  
#What happens if you supply a negative n to slice_min() and friends?  Everything but the first item is reported
flights |> 
  slice_min(dep_delay, n = -1) |> 
  relocate(carrier)

#Explain what count() does in terms of the dplyr verbs you just learned. 
#What does the sort argument to count() do?
#riends |> 
  group_by(speaker) |> 
  slice_min(speaker, n = -1)ent of count
flights |> 
  group_by(carrier) |> 
  summarize(n=n())

#Alternatively, using the count() function
flights |> 
  count(carrier)

df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

#Write down what you think the output will look like, then check if you were correct, and describe what group_by() does
df |> 
  group_by(y)
  
#Write down what you think the output will look like, then check if you were correct, and describe what arrange() does. 
#Also, comment on how it’s different from the group_by() in part (a).
df |> 
  arrange(y)

df |> 
  group_by(z) |> 
 # slice_min(x, n=1) |> 
  summarize(mean = mean(x),
            n = n()
  )

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")


df |>
  group_by(y, z) |>
  summarize(count = n(), .groups = "drop_last") |> 
  summarise(maxx=max(count),.group = 'drop')

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))
