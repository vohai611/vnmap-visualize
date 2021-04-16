library(tidyverse)
library(plotly)
vnmap <- read_rds("cleanded-data/vnmap.rds")

gg_vnmap <- vnmap %>%
  ggplot(aes(long, lat, group = group, fill = id, text = id))+
  geom_polygon(color = "white", size = 0.3)+
  theme_minimal()+
  theme(legend.position = "none")

ggplotly(gg_vnmap, tooltip = "text") %>%
  config(displayModeBar = FALSE)
