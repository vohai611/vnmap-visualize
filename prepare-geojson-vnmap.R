library(tidyverse)
# dowload shapefile from Raster
country <- raster::getData("GADM", country = "Vietnam", level = 1)

## standardize name from raster to name in VN statistic data
standard_name <- readRDS("cleanded-data/education/highschool_graduated.rds") %>%
  distinct(clean_name, dia_phuong) %>%
  arrange(dia_phuong)


new_name <- tibble(name1 = country$NAME_1, clean_name = janitor::make_clean_names(name1) ) %>%
  left_join(standard_name) %>%
  mutate(dia_phuong = if_else(is.na(dia_phuong), "TP.Hồ Chí Minh", dia_phuong)) %>%
  pull(dia_phuong)

# clean name
country$name <- new_name

# simplyfy map data
vn_small <- rmapshaper::ms_simplify(country, keep = 0.05)

# convert to geojson
geojson_vn <- geojsonio::geojson_list(vn_small)

## write to rds
readr::write_rds(geojson_vn, "cleanded-data/geojson_vnmap.rds")








