# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  title = "Vietnam statistic",
  dashboardHeader(title = "Vietnam statistic",
                  dropdownMenu(
                    notificationItem("Github", icon = icon("github"), href = "https://github.com/vohai611/vnmap-visualize")
                  )),
  dashboardSidebar(
    width = 330,
    sidebarMenu(
      menuItem("Census", tabName = "tab1", icon = icon("users")),
      menuItem("Education",tabName = "tab2",icon = icon("school")),
      menuItem("Economic", tabName = "tab3", icon = icon("hand-holding-usd")),
      echarts4rOutput("vnmap", height = "600px")
    )),
  dashboardBody(
    dashboardthemes::shinyDashboardThemes("grey_light"),
    tabItems(
      tabItem("tab1",
              fluidRow(
                valueBoxOutput(width = 3,"tab1_info1"),
                valueBoxOutput(width = 3, "tab1_info2")
              ),
              fluidRow(
                tabBox(width = 12,
                       tabPanel("Population",
                                plotlyOutput("p4", height = "600px"),
                                icon = icon("globe")),
                       tabPanel("Data on table",
                                DTOutput("table2"),
                                icon = icon("table")
                       ))
              )),
      tabItem("tab2",
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("General information",
                           plotlyOutput("p1", height = "600px"),
                           icon = icon("globe")),
                  tabPanel("Student and teacher by gender",
                           plotlyOutput("p2", height = "300px"),
                           plotlyOutput("p3", height = "300px"),
                           icon = icon("venus-mars")),
                  tabPanel("Data on table", DTOutput("table1"),
                           icon = icon("table"))
                )
              )),
      tabItem("tab3",
              fluidRow(
                valueBoxOutput(width = 5,"tab3_info1"),
                valueBoxOutput(width = 3, "tab3_info2"),
                valueBoxOutput(width = 3, "tab3_info3")),
              fluidRow(tabBox(
                width = 12,
                tabPanel("General statistic",
                         plotlyOutput("eco_p1")),
                tabPanel("Data on table", DTOutput("table3"), icon = icon("table"))
              ))
      ))
  ))



