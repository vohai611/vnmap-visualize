### map for chosen district
custom_echart_district <- function(country = "Vietnam", district){
  
  library(purrr)
  ## raster data
  country <- raster::getData("GADM", country = country, level = 3)
  
  ## filter the location
  district <- country[country$NAME_2 %in% district,]
  
  ## rename to name
  names(district)[which(names(district)== "VARNAME_3")]<- "name"
  
  # Match polygon ID
  rownames(district@data) <- map_chr(seq_len(nrow(district@data)), ~district@polygons[[.x]]@ID)
  
  
  ## convert to json map
  geojson_map <- geojsonio::geojson_list(district)
  
  ## show the choosen town 
  chosen_region <- map_chr(seq_along(geojson_map$features), ~pluck(geojson_map, "features", .x, "properties", "name"))
  
  ## return result
  list(map = geojson_map, region = chosen_region)
  
}

