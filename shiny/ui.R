#--- ui file shiny app Italy Covid-19

ui <- dashboardPage(
  
  # page
  title = "Covid-19 in Italy",
  skin = "black",
  
  # header
  header = dashboardHeader(
    titleWidth='10%',
    title = span(
      tags$img(src="corona.png", width = '120%', align = "left"))
  ),
  
  # sidebar
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Italy", tabName = "italy"),
      menuItem("Regions", tabName = "regions"),
      conditionalPanel("input.tabs == 'regions'",
        selectInput("region", "Select Region", choices = unique(regions$region))
      )
    )
  ),
  
  # body
  body = dashboardBody(
    fluidRow(
      column(9, htmlOutput("update")),
      column(3, img(src='pc.png', align = "right", width = "80px", height = "40px")),
      br(),br(),br(),
      tabItems(
        # italy
        tabItem(
          tabName = "italy",
          box(
            title = "Numbers", width = 12,
            solidHeader = T, collapsible = F, status = "danger",
            valueBoxOutput("actual", width = 3),
            valueBoxOutput("healed", width = 3),
            valueBoxOutput("death", width = 3),
            valueBoxOutput("total", width = 3)
          ),
          box(
            title = "Distribution", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("pie_cases") %>% withSpinner(color = "red")
          ),
          box(
            title = "Tested", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("tested") %>% withSpinner(color = "red")
          ),
          box(
            title = "New Cases", width = 12, height = "650px",
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("new_cases", height = "600px") %>% withSpinner(color = "red")
          ),
          box(
            title = "Dead", width = 12, height = "650px",
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("dead_cases", height = "600px") %>% withSpinner(color = "red")
          ),
          box(
            title = "Total Cases", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("tot_cases") %>% withSpinner(color = "red")
          ),
          box(
            title = "Mortality", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("mortality") %>% withSpinner(color = "red")
          ),
          box(
            title = "Regions", width = 12,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("regions_bar") %>% withSpinner(color = "red")
          )
        ),
        # regions
        tabItem(
          tabName = "regions",
          box(
            title = "Numbers", width = 12,
            solidHeader = T, collapsible = F, status = "danger",
            valueBoxOutput("actual_reg", width = 3),
            valueBoxOutput("healed_reg", width = 3),
            valueBoxOutput("death_reg", width = 3),
            valueBoxOutput("total_reg", width = 3)
          ),
          box(
            title = "Distribution", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("pie_cases_reg") %>% withSpinner(color = "red")
          ),
          box(
            title = "Tested", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("tested_reg") %>% withSpinner(color = "red")
          ),
          box(
            title = "New Cases", width = 12, height = "650px",
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("new_cases_reg",  height = "600px") %>% withSpinner(color = "red")
          ),
          box(
            title = "Dead", width = 12, height = "650px",
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("dead_cases_reg",  height = "600px") %>% withSpinner(color = "red")
          ),
          box(
            title = "Total Cases", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("tot_cases_reg") %>% withSpinner(color = "red")
          ),
          box(
            title = "Mortality", width = 6,
            solidHeader = T, collapsible = F, status = "danger",
            highchartOutput("mortality_reg") %>% withSpinner(color = "red")
          )
        )
      )
    )
  )
)
