## get the
vnmap_dt <- raster::getData("GADM", country = "Vietnam", level = 1, path = "raw-data/")
tidy_vn <- broom::tidy(vnmap_dt,region = "NAME_1")

write_rds(tidy_vn, "cleanded-data/vnmap.rds")
