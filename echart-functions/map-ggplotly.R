

ggplotly_population_vn <- function() {
  # read data
  data <- read_rds("cleanded-data/area-population.rds")

  vnmap <- read_rds("cleanded-data/vnmap.rds") %>%
    arrange(id) %>%
    group_by(id) %>%
    mutate(town_id = cur_group_id())

  area_population_2019 <- data %>%
    filter(year == 2019) %>%
    arrange(dia_phuong) %>%
    mutate(town_id = row_number())

  vnmap_join <- vnmap %>%
    left_join(area_population_2019) %>%
    mutate(danso = dan_so_trung_binh_nghin_nguoi / 1000)

  gg_vnmap <-   ggplot(vnmap_join,
                       aes(
                         long,
                         lat,
                         group = group,
                         fill = danso,
                         text = glue(
                           "Town: {id}
                area: {scales::comma(dien_tich_km2)} km2
                population: {round(danso, 1)} mil
                pop/km2: {mat_do_dan_so_nguoi_km2}"
                         )
                       )) +
    geom_polygon(color = "black", size = 0.1) +
    #scale_fill_distiller(palette = 4, direction = -1) +
    scale_fill_gradient(low = "#fed976",
                        high = "#bd0026",
                        n.breaks = 6) +
    labs(title = "Vietnam Mainland",
         fill = "Population (mil)") +
    theme_minimal() +
    theme(axis.title = element_blank())

  ggplotly(gg_vnmap, tooltip = "text") %>%
    config(displayModeBar = FALSE)

}
