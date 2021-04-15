## clean data
library(tidyverse)

dtich_danso <- read_csv("raw-data/dientich_danso.csv") %>%
  janitor::clean_names()


# area and population -----------------------------------------------------


# remove the union regions
union_regions <- dtich_danso %>%
  arrange(desc(x2011_dien_tich_km2)) %>%
  slice(1:7) %>%
  pull(dia_phuong)


dtich_danso <- dtich_danso %>%
  filter(!(dia_phuong %in% union_regions)) %>%
  pivot_longer(-dia_phuong, names_to = c("year", ".value"), names_pattern = "x(\\d{4})_(.*)") %>%
  mutate(dia_phuong = str_to_title(dia_phuong))

#write_rds(dtich_danso, "cleanded-data/area-population.rds")

# sex  --------------------------------------------------------------------

gioi_tinh <- read_csv("raw-data/gioitinh.csv") %>%
  janitor::clean_names()

gioi_tinh <- gioi_tinh %>% filter(!(tinh_thanh_pho %in% union_regions)) %>%
  filter(!tinh_thanh_pho %in% c("Ðồng bằng sông Hồng","Ðông Nam Bộ", "Ðồng bằng sông Cửu Long")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-tinh_thanh_pho, names_to = c("category", "year"), names_pattern = "(.*)_(\\d{4})",
               names_repair = "minimal") %>%
  mutate(category = case_when(str_detect(category, "nam")~ "male",
                              str_detect(category, "nu")~  "female",
                              str_detect(category, "nong_thon")~ "rural",
                              str_detect(category, "thanh_thi")~ "urban",
                              TRUE ~ "total"))

#write_rds(gioi_tinh, "cleanded-data/sex.rds")

# labor -------------------------------------------------------------------
laodong <-  read_csv("raw-data/laodong.csv") %>%
  janitor::clean_names()
laodong <- laodong %>%
  filter(!(tinh_thanh_pho %in% union_regions)) %>%
  filter(!tinh_thanh_pho %in% c("Ðồng bằng sông Hồng","Ðông Nam Bộ", "Ðồng bằng sông Cửu Long")) %>%
  pivot_longer(-tinh_thanh_pho, names_to = "year") %>%
  mutate(year = parse_number(year))

#write_rds(laodong, "cleanded-data/labor.rds")
