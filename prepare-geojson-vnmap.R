# dowload shapefile from Raster
country <- raster::getData("GADM", country = "Vietnam", level = 1)

# clean name
country$name <- janitor::make_clean_names(country$NAME_1)


# convert to geojson
geojson_map <- geojsonio::geojson_list(country)

## write to rds
readr::write_rds(geojson_map, "cleanded-data/geojson_vnmap.rds")








