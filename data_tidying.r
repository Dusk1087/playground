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


# my client data_test the pivot wider technique ---------------------------


client_data <- data.frame(
  name = c('email1', 'email1', 'email1', 'email2', 'email2'),
  file_name = c('file1email1', 'file2email1', 'file3email1', 'file1email2', 'file2email2')
)
view(client_data)

client_data |> 
  pivot_wider(
    names_from
  )

cms_patient_experience |> 
  distinct(measure_cd, measure_title)

cms_patient_experience

cms_patient_experience |> 
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )

cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with('org'),
    names_from = measure_cd,
    values_from = prf_rate
  )


# Example data
data <- data.frame(
  email = c("user1@example.com", "user1@example.com", "user1@example.com", "user2@example.com", "user2@example.com"),
  file_name = c("email1file1.pdf", "email1file2.pdf", "email1file3.pdf","email2file1.pdf", "email2file2.pdf")
)


print(data)

add_file_counter <- data |> 
  group_by(email) |> 
  mutate(file_number = paste0("file_",row_number())) |> 
  ungroup()# Create a unique number for each file per email
print(add_file_counter)
add_file_counter |> 
  distinct(file_number)

# Transform the data
result <- add_file_counter |> 
  pivot_wider(
    names_from = file_number, 
    values_from = file_name,
    names_prefix = "file_"
  )
print(result)

df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)
df
df |> 
  pivot_wider(
    id_cols = id,
    names_from = measurement,
    values_from = value
  )

df |> 
  distinct(measurement) |> 
  pull()

df |> 
  distinct(measurement)

df |> 
  select(-measurement, -value) |> 
  distinct() 

df |> 
  select(-measurement, -value) |> 
  distinct() |> 
  mutate(x = NA, y = NA, z = NA)


df23 <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "A",        "bp1",    102,
  "A",        "bp2",    120,
  "B",        "bp1",    140, 
  "B",        "bp2",    115
)
df23 |> 
  pivot_wider(
    id_cols = id,
    names_from = measurement,
    values_from = value
  )

df23 |> 
  group_by(id, measurement) |> 
  summarize(n = n(), .groups = "drop") |> 
  filter(n > 0)

