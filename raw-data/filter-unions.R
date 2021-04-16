## clean data fucntion
filter_union <- function(tbl){
  filter_unions <- readRDS("raw-data/filter_unions.rds")

  tbl %>%
  filter(!(tinh_thanh_pho %in% filter_unions)) %>%
    pivot_longer(-tinh_thanh_pho, names_to = "year") %>%
    mutate(year = parse_number(year))
}
