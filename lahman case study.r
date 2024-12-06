library(tidyverse)
library(Lahman)
batters_original <- Lahman::Batting
batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(performance = sum(H, na.rm=TRUE) / sum(AB, na.rm = TRUE),
            n = sum(AB, na.rm = TRUE)
  )
#plot a graph of baseball Hit to At Bat performance
batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE)

