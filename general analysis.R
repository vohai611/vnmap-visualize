theme_set(theme_light())



# doanh nghiep ------------------------------------------------------------
# total revenue
company <- read_rds("cleanded-data/company.rds")
# total_revenue change
company %>%
  ggplot(aes(year, company_revenue, group = tinh_thanh_pho, color = tinh_thanh_pho))+
  geom_line(show.legend = FALSE)+
  geom_label(aes(label= tinh_thanh_pho), show.legend = FALSE)
## some go up, almost all keep unchange
company %>%
  filter(year %in% c(2010, 2015)) %>%
  select(-n_company) %>%
  pivot_wider(names_from = year, values_from = company_revenue , names_prefix = "x") %>%
  mutate(change = x2015 - x2010) %>%
  arrange(-abs(change))

## why Bà Rịa - Vũng Tàu go down so much?
 company %>%
   filter(tinh_thanh_pho == "Bà Rịa - Vũng Tàu" )

# trung binh doanh thu



# what town change the most ?
company %>%
  mutate(avg_revenue = company_revenue/ n_company) %>%
  filter(year %in% c(2017, 2018), !is.na(avg_revenue)) %>%
  select(-n_company, -company_revenue) %>%
  pivot_wider(names_from = year, values_from = avg_revenue, names_prefix = "x") %>%
  mutate(change = x2018- x2017) %>%
  arrange(-(abs(change)))


# danso  ------------------------------------------------------------------
labor <- read_rds("cleanded-data/labor.rds") %>%
  filter(tinh_thanh_pho != "Bắc Trung Bộ và duyên hải miền Trung")


labor %>%
  filter(year == 2017) %>%
  mutate(tinh_thanh_pho = fct_lump(tinh_thanh_pho, w= value, n= 15)) %>%
  group_by(tinh_thanh_pho) %>%
  summarise(n_labor = mean(value),.groups = "drop") %>%
  mutate(tinh_thanh_pho = fct_reorder(tinh_thanh_pho, n_labor)) %>%
  ggplot(aes(n_labor,tinh_thanh_pho))+
  geom_col()+
  scale_x_continuous(labels = comma)



labor %>%
  ggplot(aes(year, value ,color = tinh_thanh_pho ,group =tinh_thanh_pho))+
  geom_line(show.legend = FALSE)+
  scale_y_continuous(labels = comma)

labor %>%
  group_by(year) %>%
  summarise(total = sum(value))

company %>%
  group_by(year) %>%
  summarise(total_revenue = sum(company_revenue),
            n_company = sum(n_company, na.rm = TRUE))
### 2010 - 2015 change
# what change ?
labor %>%
  filter(year %in% c(2010, 2015)) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "x") %>%
  mutate(change = x2015 - x2010) %>%
  arrange(change)


# changing in labor force




