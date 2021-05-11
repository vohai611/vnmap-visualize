library(tidyverse)
library(shiny)
library(echarts4r)
library(DT)
library(janitor)
library(shinydashboard)
library(glue)
library(plotly)
library(scales)
library(here)

# styling theme and color --------------------------------------------------
theme_facet <- theme_light() +
  theme(
    strip.text = element_text(
      face = "bold",
      color = "chocolate3",
      hjust = .5,
      size = 11
    ),
    plot.title = element_text(margin = margin(b =10)),
    text = element_text(family = "Montserrat"),
    strip.background = element_rect(fill = "gray90", color = "transparent"),
    panel.grid.major.y = element_line(colour = "white", linetype = 3),
    plot.background = element_rect(fill = "#E7D5BD"),
    panel.background = element_rect(fill = "grey90")
  )

my_pal<- c("#62959c", "#c19277", "#e1bc91", "#e3d0b9","#9dad7f", "#726a95",
           "#d6b0b1", "#8b5e83")

# prepare data -------------------------------------------------------------

## rename education level to english-----
rename_education <- . %>%
  mutate(education_level = case_when(education_level == "cap1" ~ "Elementary school",
                                    education_level == "cap2" ~ "Middle school",
                                    education_level == "cap3" ~ "High school"),
         education_level = factor(education_level,
                                  levels = c("Elementary school", "Middle school", "High school"))
         )

## Load the data ----
area <- read_rds(here("cleanded-data/area-population.rds")) %>%
  filter(year == 2018)

company <- read_rds(here("cleanded-data/company.rds"))
labor <- read_rds(here("cleanded-data/labor.rds"))

pop <- read_rds(here("cleanded-data/sex.rds"))

school_stat <- read_rds(here("cleanded-data/education/n_school-student-teacher.rds")) %>%
  rename_education()

hs_graduated <- read_rds(here("cleanded-data/education/highschool_graduated.rds"))
student_gender <- read_rds(here("cleanded-data/education/female_student.rds")) %>%
  rename_education()
teacher_gender <- read_rds(here("cleanded-data/education/female_teacher.rds")) %>%
  rename_education()

# match display UI and server name
equal_name <- school_stat %>%
  distinct(clean_name, dia_phuong)

# load map for echart
geojson_vn <- read_rds(here("cleanded-data/geojson_vnmap.rds"))

# Server ------------------------------------------------------------------
server <- function(input, output, session){

  ## render echart map ----
  output$vnmap <- renderEcharts4r({
    hs_graduated %>%
      filter(year == 2016) %>%
      e_charts(dia_phuong) %>%
      e_map_register("vn", geojson_vn) %>%
      e_map(pct_graduated, map = "vn") %>%
      #e_visual_map(pct_graduated) %>%
      # e_theme("infographic") %>%
      e_title(
        #text ="% Graduated from highschool (2016)",
        subtext = glue("{input$vnmap_clicked_data$name} is now selected
                       Click on map to choose the province"))
  })


  ### selected from echart ----
  province_selected <- reactive({
    if (is.null(input$vnmap_clicked_data$name)) ("ha_noi")
    else (make_clean_names(input$vnmap_clicked_data$name))

  })
  ## check event
  observeEvent(input$vnmap_clicked_data, {print(input$vnmap_clicked_data$name)})

## Tab1: Education ----

  ### render valuebox -------
  edu_selected <- reactive({
    area %>% filter(year == 2018,
                    clean_name == province_selected())
  })
  output$tab1_info1 <- renderValueBox(
    valueBox(comma(edu_selected()$dien_tich_km2,
                   suffix = " km2"),
             subtitle = "Area",
             icon = icon("chart-area")))

  output$tab1_info2 <- renderValueBox(valueBox(edu_selected()$mat_do_dan_so_nguoi_km2,
                                               subtitle = "People per km2",
                                               icon = icon("male")))


### render subtab1: general information ----
    output$p1 <- renderPlotly({
      df <- school_stat %>%
        mutate(year = as.factor(year)) %>%
        filter(clean_name == province_selected()) %>%
        pivot_longer(c(n_student,n_teacher, n_school, n_class)) %>%
        mutate(name = case_when(name == "n_class" ~  "Number of classes",
                                name == "n_student" ~  "Number of students",
                                name == "n_teacher" ~  "Number of teachers",
                                name == "n_school" ~ "Number of schools"))
      plot <-  df %>%
         ggplot(aes(year, value, color = education_level, group = education_level,
                    text = glue("Year: {year}
                         value: {comma(round(value,-1))}")))+
         geom_line(size = .4, alpha = .6)+
         geom_point(size = .8)+
         labs(title = glue("{df$dia_phuong[[1]]} Province"),
              subtitle = " ",
              x = NULL,
              y= NULL,
              color = NULL)+
         scale_color_manual(values = my_pal[1:3],
                            labels = c("Elementary", "Middle school", "High school"))+
        scale_x_discrete(breaks = seq(2002, 2019, 3))+
         scale_y_continuous(labels = comma)+
         facet_wrap(~name, scale = "free")+
         theme_facet

      ggplotly(plot, tooltip = "text") %>%
        layout(hovermode = "x")
    })

#### render student plot subtab2 ----
    output$p2 <- renderPlotly({
      p2 <- student_gender %>%
        filter(clean_name == province_selected()) %>%
        ggplot(aes(year, value, color = gender, group = gender,
                   text = glue("{gender}
                                Year: {year}
                                value: {comma(round(value,-1))}")))+
        geom_point(size = .8)+
        geom_line(size = .6)+
        scale_color_manual(values = my_pal[c(2,8)],
                           labels = c("Female", "Male"))+
        scale_x_discrete(breaks = seq(2002, 2019, 3))+
        scale_y_continuous(labels = comma)+
        facet_wrap(~education_level)+
        labs(title = "Student by gender",
             x= NULL,
             color = NULL)+
        theme_facet

      ggplotly(p2, tooltip = "text") %>%
        layout(hovermode = "x")
    })

#### render teacher plot subtab2 ----
    output$p3 <- renderPlotly({
      p3 <- teacher_gender %>%
        filter(clean_name == province_selected()) %>%
        ggplot(aes(year, value, color = gender, group = gender,
                   text = glue("{gender}
                                Year: {year}
                                value: {comma(round(value,-1))}")))+
        geom_point(size = .8)+
        geom_line(size = .6)+
        scale_color_manual(values = my_pal[c(2,8)],
                           labels = c("Female", "Male"))+
        scale_x_discrete(breaks = seq(2002, 2019, 3))+
        scale_y_continuous(labels = comma)+
        facet_wrap(~education_level)+
        labs(title = "Teacher by gender",
             x= NULL,
             color = NULL)+
        theme_facet
        ggplotly(p3) %>%
          layout(hovermode= "x")
    })

### render subtab3: Data in table ----

    output$table1 <- renderDT({
      school_stat %>%
        filter(clean_name == province_selected()) %>%
        select(-clean_name) %>%
        datatable(extensions = c("Buttons"),
                  options = list(
                    dom = "Bfrtip",
                    buttons = list("pageLength",
                                   list(extend = "collection",
                                   buttons = c("csv", "excel", "pdf"),
                                   text ="Download"))))
    })

## Tab2: Economic ----
  ## eco of selected province in 2018
  inf_box_dt <- reactive({
      company %>%
      filter(clean_name == province_selected(),
                         year == 2018)
    })
  ### valuebox ----
  output$tab3_info1 <- renderValueBox({
    valueBox(value = comma(inf_box_dt()$company_revenue,suffix = " billions"),
             subtitle = "VND Total company revenue",
             color = ifelse(inf_box_dt()$company_revenue >=0, "blue", "red"),
             icon = icon("search-dollar"))
  })

  output$tab3_info2 <- renderValueBox({
    valueBox(value = comma(inf_box_dt()$n_company),
             subtitle = "Companies",
             icon = icon("building"))
  })

  output$tab3_info3 <- renderValueBox({
    labor_val <- labor %>% filter(clean_name == province_selected(), year == 2018) %>% pull(value)
    valueBox(value = comma(labor_val),
             subtitle = "Labors",
             icon = icon("male"))
  })
## Tab3: Population ----
  #### female-male / rural-urban visualize ----
  output$p4 <- renderPlotly({

    pop <- pop %>%
      filter(clean_name == province_selected()) %>%
      mutate(year = as.double(as.character(year)))

    total_pop <- pop %>% filter(category == "total") %>% select(-category)

    (pop %>% filter(category != "total") %>%
        ggplot(aes(year, value, fill = category, group = category, text = glue("{year}
                                                                               {category}
                                                                               {value}")))+
        geom_area()+
        geom_area(data= total_pop,
                  aes(year, value, group = clean_name, text = glue("Total
                                                                   Population :{value}")),
                  fill = "grey80",alpha = .6, inherit.aes = F)+
        facet_wrap(~category, scale = "free")+
        scale_x_continuous(n.breaks = 5)+
        scale_y_continuous(labels = comma)+
        labs(title = "",
             x= NULL,
             y= NULL,
             fill = NULL)+
        theme_facet) %>%
      plotly::ggplotly(tooltip = "text") %>%
      layout(hovermode = "x")
  })
  ####  data on table----
  output$table2 <- renderDT({
    pop %>%
      filter(clean_name == province_selected()) %>%
      select(-clean_name) %>%
      datatable(extensions = c("Buttons"),
                options = list(
                  dom = "Bfrtip",
                  buttons = list("pageLength",
                                 list(extend = "collection",
                                      buttons = c("csv", "excel", "pdf"),
                                      text ="Download"))))
  })

}





