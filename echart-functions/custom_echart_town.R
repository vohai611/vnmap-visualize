### map for chosen town
custom_echart_town <- function(country = "Vietnam", town){
  library(purrr)
  ## raster data
  country <- raster::getData("GADM", country = country, level = 2)
  
  ## filter the location
  town <- country[country$NAME_1 %in% town,]
  
  ## rename
  names(town)[7]<- "name"
  
  ## convert to json map
  geojson_map <- geojsonio::geojson_list(town)
  
  ## show the choosen town 
  chosen_region <- map_chr(seq_along(geojson_map$features), ~pluck(geojson_map, "features", .x, "properties", "name"))
  
  ## return result
  list(map = geojson_map, region = chosen_region)
  
  
}
