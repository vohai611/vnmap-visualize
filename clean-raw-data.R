## clean data
library(tidyverse)
setwd(here::here())
source("raw-data/filter-unions.R")

dtich_danso <- read_csv("raw-data/dientich_danso.csv") %>%
  janitor::clean_names()


# area and population -----------------------------------------------------

# add clean name from janitor
clean_name <- dtich_danso %>%
  distinct(dia_phuong) %>%
  mutate(clean_name = janitor::make_clean_names(dia_phuong))

# remove the union regions
union_regions <- dtich_danso %>%
  arrange(desc(x2011_dien_tich_km2)) %>%
  slice(1:7) %>%
  pull(dia_phuong) %>%
  c(c("Ðồng bằng sông Hồng","Ðông Nam Bộ", "Ðồng bằng sông Cửu Long"))

#write_rds(union_regions, "raw-data/filter_unions.rds")
union_regions <- read_rds("raw-data/filter_unions.rds")

dtich_danso <- dtich_danso %>%
  filter(!(dia_phuong %in% union_regions)) %>%
  pivot_longer(-dia_phuong, names_to = c("year", ".value"), names_pattern = "x(\\d{4})_(.*)") %>%
  mutate(dia_phuong = str_to_title(dia_phuong),
         year = as_factor(year)) %>%
  mutate(dia_phuong = if_else(dia_phuong == "Tp.hồ Chí Minh", "Hồ Chí Minh", dia_phuong)) %>%
  inner_join(clean_name)


write_rds(dtich_danso, "cleanded-data/area-population.rds")

# sex  --------------------------------------------------------------------

gioi_tinh <- read_csv("raw-data/gioitinh.csv") %>%
  janitor::clean_names()

gioi_tinh <- gioi_tinh %>%
  filter(!(tinh_thanh_pho %in% union_regions)) %>%
  filter(!tinh_thanh_pho %in% c("Ðồng bằng sông Hồng","Ðông Nam Bộ", "Ðồng bằng sông Cửu Long")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-tinh_thanh_pho, names_to = c("category", "year"), names_pattern = "(.*)_(\\d{4})",
               names_repair = "minimal") %>%
  mutate(value = as.double(value)) %>%
  mutate(category = case_when(str_detect(category, "nam")~ "male",
                              str_detect(category, "nu")~  "female",
                              str_detect(category, "nong_thon")~ "rural",
                              str_detect(category, "thanh_thi")~ "urban",
                              TRUE ~ "total")) %>%
  mutate(year = as_factor(year)) %>%
  inner_join(clean_name, by =c("tinh_thanh_pho" = "dia_phuong"))

write_rds(gioi_tinh, "cleanded-data/sex.rds")

# labor -------------------------------------------------------------------

laodong <-  read_csv("raw-data/laodong.csv") %>%
  janitor::clean_names() %>%
  filter_union() %>%
  mutate(year = as_factor(year))

laodong_clean_name <- laodong %>%
  distinct(tinh_thanh_pho)%>%
  mutate(clean_name = janitor::make_clean_names(tinh_thanh_pho))

laodong %>%
  inner_join(laodong_clean_name) %>%
write_rds( "cleanded-data/labor.rds")

# company -----------------------
dn1 <- read_csv("raw-data/so_doanhnghiep.csv") %>%
  janitor::clean_names() %>%
  filter_union() %>%
  rename(n_company = value)
dn2 <- read_csv("raw-data/loinhuan_dn.csv") %>%
  janitor::clean_names() %>%
  filter_union() %>%
  rename(company_revenue = value)

dn <- dn1 %>%
  full_join(dn2) %>%
  mutate(year = as_factor(year))

dn_clean_name <- dn %>% distinct(tinh_thanh_pho)%>%
  mutate(clean_name = janitor::make_clean_names(tinh_thanh_pho))

dn %>%
  inner_join(dn_clean_name) %>%
  write_rds("cleanded-data/company.rds")


read_csv("raw-data/labor-by-age.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(-nhom_tuoi) %>%
  mutate(nhom_tuoi = if_else(nhom_tuoi =='TỔNG SỐ', "total", nhom_tuoi)) %>%
  transmute(nhom_tuoi, year = parse_number(name), n_labor = value) %>%
  write_rds("cleanded-data/labor-by-age.rds")



