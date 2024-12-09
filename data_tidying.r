library(tidyverse)
# Compute rate per 10,000
table1 |>
  mutate(rate = cases / population * 10000)


# Compute total cases per year
table1 |> 
  group_by(year) |> 
  summarize(total_cases = sum(cases))
#> # A tibble: 2 × 2

# Visualize changes over time
ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000)) # x-axis breaks at 1999 and 2000

ggplot(table1, aes(x = year, y = cases)) +
  geom_point(aes(color = country, shape = country)) +
  geom_line(aes(group = country), color = "grey50") +
  scale_x_continuous(breaks = c(1999, 2000)) # x-axis breaks at 1999 and 2000

view(table2)
table2 <- table2
table1 <- table1
table3 <- table3
view(table1)
view(table3)

# pivot longer introduction -----------------------------------------------

view(billboard)
billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = 'week',
    values_to = 'rank',
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )


billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()

billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_x_reverse()


# pivot example -----------------------------------------------------------

df <- tribble(
  ~id, ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)
  
view(df)

df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = 'measurement',
    values_to = 'value'
  )


# pivot longer with complex column names ------------------------------------------------------------


view(who2)
who2 <- who2
who2 |> 
  glimpse()

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c('diagnosis', 'gender', 'age'),
    names_sep = '_',
    values_to = 'count'
  )


# pivot longer with column names including variable values and var --------

household |> 
  pivot_longer(
    cols = !family,
    names_to = c('.value', 'child'),
    names_sep = '_',
    values_drop_na = TRUE
  )
