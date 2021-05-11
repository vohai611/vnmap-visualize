library(here)
source(here("education-dashboard/UI.R"))
source(here("education-dashboard/sever.R"))
shiny::shinyApp(ui, server)
