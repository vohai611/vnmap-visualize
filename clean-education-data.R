library(tidyverse)

#  clean function ----
basic_clean <- function(tbl, values_to) {
  remove_region <- c("ca_nuoc",
                     "tay_nguyen",
                     "dong_nam_bo",
                     "dong_bang_song_hong",
                     "bac_trung_bo_va_duyen_hai_mien_trung",
                     "trung_du_va_mien_nui_phia_bac",
                     "dong_bang_song_cuu_long")
  tbl %>%
    janitor::clean_names() %>%
    mutate(clean_name = janitor::make_clean_names(dia_phuong),.before = 1) %>%
    pivot_longer(-c(clean_name,dia_phuong), names_to = "year", values_to = values_to) %>%
    filter(!clean_name %in% remove_region)
}

clean_2 <- function(tbl, values_to) {
  tbl %>%
    basic_clean(values_to) %>%
    extract(year,
            into = c("year", "education_level"),
            regex = "x(\\d{4})_(.*)") %>%
    mutate(
      education_level = case_when(
        education_level == "tieu_hoc" ~ "cap1",
        education_level %in% c("trung_hoc", "trung_hoc_co_so") ~ "cap2",
        education_level %in% c("pho_thong_co_so", "trung_hoc_pho_thong") ~ "cap3",
        TRUE ~ education_level
      )
    ) %>%
    filter(education_level != "tong_so")
}


# load data ----
highschool_graduated <- read_csv("raw-data/Education/highschool-graduated.csv") %>%
  mutate(across(-1, .fns = as.double))

university_student <- read_csv("raw-data/Education/student-university.csv") %>%
  mutate(across(-1, .fns = as.double))

school_student <- read_csv("raw-data/Education/n-student_high-school.csv") %>%
  mutate(across(-1, .fns = as.double))

teacher <- read_csv("raw-data/Education/number-of-teachers.csv") %>%
  mutate(across(-1, .fns = as.double))

school <- read_csv("raw-data/Education/number-schools.csv") %>%
  mutate(across(-1, .fns = as.double))

n_class <- read_csv("raw-data/Education/n_class.csv") %>%
  mutate(across(-1, .fns = as.double))

female_teacher <- read_csv("raw-data/Education/female-teacher.csv") %>%
  mutate(across(-1, .fns = as.double))

femalte_student <-  read_csv("raw-data/Education/female-student.csv") %>%
  mutate(across(-1, .fns = as.double))
#dir.create("cleanded-data/education")
# clean data ----

## highschool graduate pct,  university student ----------------------------

highschool_graduated %>%
  basic_clean(values_to = "pct_graduated")%>%
  mutate(year= as.factor(str_sub(year, 2,5))) %>%
  write_rds("cleanded-data/education/highschool_graduated.rds")


university_student %>%
  basic_clean(values_to ="n_Ustudent") %>%
  mutate(year = as.factor(parse_number(year))) %>%
  write_rds("cleanded-data/education/university_student.rds")



##   student - teacher and school ------------------------------------------

school_student <- school_student %>%
  clean_2("n_student")


teacher <- teacher %>%
  clean_2("n_teacher") %>%
  select(-dia_phuong)

school <- school %>%
  clean_2("n_school") %>%
  group_by(clean_name, dia_phuong, year, education_level) %>%
  summarise(n_school = sum(n_school, na.rm = TRUE),.groups = "drop") %>%
  select(-dia_phuong)

n_class <- n_class %>%
  clean_2("n_class") %>%
  select(-dia_phuong)


school_student %>%
  full_join(teacher) %>%
  full_join(school) %>%
  full_join(n_class) %>%
  filter(clean_name != "ha_tay",
         !clean_name %in% c("lop_hoc_pho_thong_trong_cac_truong_dai_hoc",
                            "hoc_sinh_pho_thong_trong_cac_truong_dai_hoc")) %>%
  write_rds("cleanded-data/education/n_school-student-teacher.rds")


female_teacher %>%
  clean_2(values_to = "female_teacher") %>%
  left_join(teacher) %>%
  mutate(male_teacher = n_teacher - female_teacher) %>%
  filter(male_teacher > 0) %>%
  select(-n_teacher) %>%
  pivot_longer(c(female_teacher, male_teacher), "gender", "value") %>%
  write_rds("cleanded-data/education/female_teacher.rds")


femalte_student %>%
  clean_2("female_student") %>%
  left_join(school_student) %>%
  mutate(male_student = n_student - female_student) %>%
  select(-n_student) %>%
  pivot_longer(c(female_student, male_student), "gender", "value") %>%
  write_rds("cleanded-data/education/female_student.rds")

# extract religion --------------------------------------------------------
read_csv("raw-data/Education/n_class.csv") %>%
  mutate(across(-1, .fns = as.double)) %>%
  janitor::clean_names() %>%
  select(dia_phuong) %>%
  mutate(clean_name = janitor::make_clean_names(dia_phuong)) %>%
  filter(clean_name != "ca_nuoc",
         clean_name != "lop_hoc_pho_thong_trong_cac_truong_dai_hoc") %>%
  mutate(region = case_when(clean_name == "tay_nguyen"   ~ clean_name,
                            clean_name == "dong_nam_bo" ~ clean_name,
                            clean_name == "dong_bang_song_hong" ~ clean_name,
                            clean_name == "bac_trung_bo_va_duyen_hai_mien_trung" ~ clean_name,
                            clean_name == "trung_du_va_mien_nui_phia_bac" ~ clean_name,
                            clean_name == "dong_bang_song_cuu_long" ~ clean_name,
                            TRUE ~ NA_character_)) %>%
  fill(region, .direction = "down") %>%
  write_rds('cleanded-data/education/religion.rds')

