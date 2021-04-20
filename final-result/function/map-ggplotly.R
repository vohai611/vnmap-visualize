
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
    left_join(area_population_2019)

  gg_vnmap <-   ggplot(
    vnmap_join,
    aes(
      long,
      lat,
      group = group,
      fill = dan_so_trung_binh_nghin_nguoi,
      text = glue(
        "Town: {id}
                area: {dien_tich_km2} km2
                population: {dan_so_trung_binh_nghin_nguoi}
                pop/km2: {mat_do_dan_so_nguoi_km2}"
      )
    )
  ) +
    geom_polygon(color = "white", size = 0.3) +
    scale_fill_continuous(type = "viridis") +
    labs(title = "Vietnam Mainland",
         fill = "Population (mil)") +
    theme_light()+
    theme(axis.title = element_blank())

  ggplotly(gg_vnmap, tooltip = "text") %>%
    config(displayModeBar = FALSE)


}
