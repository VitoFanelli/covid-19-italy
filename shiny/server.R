#--- server file shiny app Italy Covid-19

server <- function(input, output, session){
  
  updateTabItems(session, inputId = "tabs", selected = "italy")
  
  output$update <- renderText({
    HTML(
      paste("<a href=https://github.com/pcm-dpc/COVID-19>
            Update to ", format(max(italy$date), "%d/%m/%Y"), 
            " from https://github.com/pcm-dpc/COVID-19</a>", sep = "")
    )
  })
  
  #--------------------------- italy -----------------------------------------#
  
  #--- actual
  output$actual <- renderValueBox({
    valueBox(
      format(italy$actual_cases[italy$date == max(italy$date)],
        big.mark = ".", decimal.mark = ","
      ),
      "Actual Cases",
      icon = icon("plus-circle"),
      color = "red"
    )
  })
  
  #--- healed
  output$healed <- renderValueBox({
    valueBox(
      format(italy$healed[italy$date == max(italy$date)],
        big.mark = ".", decimal.mark = ","
      ),
      "Healed",
      icon = icon("check-circle"),
      color = "red"
    )
  })
  
  #--- dead
  output$death <- renderValueBox({
    valueBox(
      format(italy$dead[italy$date == max(italy$date)],
        big.mark = ".", decimal.mark = ","
      ),
      "Dead",
      icon = icon("times-circle"),
      color = "red"
    )
  })
  
  #--- total
  output$total <- renderValueBox({
    valueBox(
      format(italy$total_cases[italy$date == max(italy$date)],
        big.mark = ".", decimal.mark = ","
      ),
      "Total Cases",
      icon = icon("users"),
      color = "red"
    )
  })
  
  #--- pie total cases
  output$pie_cases <- renderHighchart({
    data <- data.frame(
      Actual = italy$actual_cases[italy$date == max(italy$date)],
      Healed = italy$healed[italy$date == max(italy$date)],
      Dead = italy$dead[italy$date == max(italy$date)]
    )
    data <- gather(data, Type, Value)
    data$color = c("#0000FF","#00FF00","#FF0000")
    highchart(type = "chart") %>% 
      hc_add_series(data = data, type = "pie", 
        hcaes(x = Type, y = Value, color = color)) %>% 
      hc_title(text = "Total Cases Distribution")
  })
  
  #--- bar tested
  output$tested <- renderHighchart({
    data <- italy[(nrow(italy)-4):nrow(italy), c(1,7)]
    highchart(type = "chart") %>% 
      hc_add_series(data = data, type = "column", 
                    hcaes(x = as.character(date), y = tested), name = "Tested") %>% 
      hc_title(text = "Tested Cases - Last 5 days") %>% 
      hc_xAxis(categories = as.character(format(data$date, "%d/%m/%Y")))
  })
  
  #--- new cases trend
  output$new_cases <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_yAxis(title = list(text = "Value")) %>% 
      hc_title(text = "New Cases Daily Trend") %>% 
      hc_subtitle(text = "D.P.C.M. = Decree of the Council of Ministers's President") %>%
      hc_add_series(italy, hcaes(x = date, y = new_cases),
                    type = "line", name = "New Cases") %>% 
      hc_add_series(italy, hcaes(x = date, y = round(SMA(new_cases, 7),0)),
                    type = "line", name = "Moving Average (7)") %>% 
      hc_legend(enabled = T) %>% 
      hc_xAxis(
        title = list(text = "Day"),
        plotLines = list(
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 23/02-25/02"),
            value = datetime_to_timestamp(as.Date("2020-02-25")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/03"),
            value = datetime_to_timestamp(as.Date("2020-03-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 08/03-09/03"),
            value = datetime_to_timestamp(as.Date("2020-03-09")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 11/03"),
            value = datetime_to_timestamp(as.Date("2020-03-11")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 22/03"),
            value = datetime_to_timestamp(as.Date("2020-03-22")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/04"),
            value = datetime_to_timestamp(as.Date("2020-04-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 10/04"),
            value = datetime_to_timestamp(as.Date("2020-04-10")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 27/04"),
            value = datetime_to_timestamp(as.Date("2020-04-27")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          )
        )
      )
  })
  
  #--- dead trend
  output$dead_cases <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_yAxis(title = list(text = "Value")) %>% 
      hc_title(text = "Dead Daily Trend") %>% 
      hc_subtitle(text = "D.P.C.M. = Decree of the Council of Ministers's President") %>%
      hc_add_series(italy, hcaes(x = date, y = daily_dead),
                    type = "line", name = "Dead") %>% 
      hc_add_series(italy, hcaes(x = date, y = round(SMA(daily_dead, 7),0)),
                    type = "line", name = "Moving Average (7)") %>% 
      hc_legend(enabled = T) %>% 
      hc_xAxis(
        title = list(text = "Day"),
        plotLines = list(
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 23/02-25/02"),
            value = datetime_to_timestamp(as.Date("2020-02-25")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/03"),
            value = datetime_to_timestamp(as.Date("2020-03-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 08/03-09/03"),
            value = datetime_to_timestamp(as.Date("2020-03-09")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 11/03"),
            value = datetime_to_timestamp(as.Date("2020-03-11")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 22/03"),
            value = datetime_to_timestamp(as.Date("2020-03-22")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/04"),
            value = datetime_to_timestamp(as.Date("2020-04-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 10/04"),
            value = datetime_to_timestamp(as.Date("2020-04-10")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 27/04"),
            value = datetime_to_timestamp(as.Date("2020-04-27")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          )
        )
      )
  })
  
  #--- total cases trend
  output$tot_cases <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_title(text = "Total Cases Cumulative Trend") %>% 
      hc_add_series(italy, hcaes(x = date, y = total_cases),
                    type = "line", name = "Total Cases") %>% 
      hc_legend(enabled = T) %>% 
      hc_yAxis(title = list(text = "Total Cases")) %>% 
      hc_xAxis(title = list(text = "Day"))
  })
  
  #--- mortality
  output$mortality <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_title(text = "Mortality Rate (%)") %>% 
      hc_add_series(italy, hcaes(x = date, y = mortality),
                    type = "line", name = "Mortality Rate") %>% 
      hc_legend(enabled = T) %>% 
      hc_yAxis(title = list(text = "Rate (%)")) %>% 
      hc_xAxis(title = list(text = "Day"))
  })
  
  #--- regions
  output$regions_bar <- renderHighchart({
    data <- regions %>% 
      filter(date == max(date)) %>% 
      select(region, total_cases, dead)
    highchart(type = "chart") %>% 
      hc_add_series(data = data, type = "column", 
                    hcaes(x = region, y = total_cases), name = "Total Cases") %>% 
      hc_add_series(data = data, type = "column", 
                    hcaes(x = region, y = dead), name = "Dead") %>% 
      hc_title(text = "Regional Distribution") %>% 
      hc_xAxis(categories = data$region, labels = list(rotation = 80)) %>% 
      hc_yAxis(title = list(text = "Total Cases / Dead"))
  })
  
  #---------------------------- regions ---------------------------------------#
  
  region <- reactive({
    regions %>% filter(region == input$region)
  })
  
  #--- actual
  output$actual_reg <- renderValueBox({
    valueBox(
      format(region()$actual_cases[region()$date == max(region()$date)],
             big.mark = ".", decimal.mark = ","
      ),
      "Actual Cases",
      icon = icon("plus-circle"),
      color = "red"
    )
  })
  
  #--- healed
  output$healed_reg <- renderValueBox({
    valueBox(
      format(region()$healed[region()$date == max(region()$date)],
             big.mark = ".", decimal.mark = ","
      ),
      "Healed",
      icon = icon("check-circle"),
      color = "red"
    )
  })
  
  #--- dead
  output$death_reg <- renderValueBox({
    valueBox(
      format(region()$dead[region()$date == max(region()$date)],
             big.mark = ".", decimal.mark = ","
      ),
      "Dead",
      icon = icon("times-circle"),
      color = "red"
    )
  })
  
  #--- total
  output$total_reg <- renderValueBox({
    valueBox(
      format(region()$total_cases[region()$date == max(region()$date)],
             big.mark = ".", decimal.mark = ","
      ),
      "Total Cases",
      icon = icon("users"),
      color = "red"
    )
  })
  
  #--- pie total cases
  output$pie_cases_reg <- renderHighchart({
    data <- data.frame(
      Actual = region()$actual_cases[region()$date == max(region()$date)],
      Healed = region()$healed[region()$date == max(region()$date)],
      Dead = region()$dead[region()$date == max(region()$date)]
    )
    data <- gather(data, Type, Value)
    data$color = c("#0000FF","#00FF00","#FF0000")
    highchart(type = "chart") %>% 
      hc_add_series(data = data, type = "pie", 
                    hcaes(x = Type, y = Value, color = color)) %>% 
      hc_title(text = "Total Cases Distribution")
  })
  
  #--- bar tested
  output$tested_reg <- renderHighchart({
    data <- region()[(nrow(region())-4):nrow(region()), c(2,8)]
    highchart(type = "chart") %>% 
      hc_add_series(data = data, type = "column", 
                    hcaes(x = as.character(date), y = tested), name = "Tested") %>% 
      hc_title(text = "Tested Cases - Last 5 days") %>% 
      hc_xAxis(categories = as.character(format(data$date, "%d/%m/%Y")))
  })
  
  #--- new cases trend
  output$new_cases_reg <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_yAxis(title = list(text = "Value")) %>% 
      hc_title(text = "New Cases Daily Trend") %>% 
      hc_subtitle(text = "D.P.C.M. = Decree of the Council of Ministers's President") %>%
      hc_add_series(region(), hcaes(x = date, y = new_cases),
                    type = "line", name = "New Cases") %>% 
      hc_add_series(region(), hcaes(x = date, y = round(SMA(new_cases, 7),0)),
                    type = "line", name = "Moving Average (7)") %>% 
      hc_legend(enabled = T) %>% 
      hc_xAxis(
        title = list(text = "Day"),
        plotLines = list(
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 23/02-25/02"),
            value = datetime_to_timestamp(as.Date("2020-02-25")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/03"),
            value = datetime_to_timestamp(as.Date("2020-03-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 08/03-09/03"),
            value = datetime_to_timestamp(as.Date("2020-03-09")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 11/03"),
            value = datetime_to_timestamp(as.Date("2020-03-11")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 22/03"),
            value = datetime_to_timestamp(as.Date("2020-03-22")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/04"),
            value = datetime_to_timestamp(as.Date("2020-04-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 10/04"),
            value = datetime_to_timestamp(as.Date("2020-04-10")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 27/04"),
            value = datetime_to_timestamp(as.Date("2020-04-27")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          )
        )
      )
  })
  
  #--- dead trend
  output$dead_cases_reg <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_yAxis(title = list(text = "Value")) %>% 
      hc_title(text = "Dead Daily Trend") %>% 
      hc_subtitle(text = "D.P.C.M. = Decree of the Council of Ministers's President") %>%
      hc_add_series(region(), hcaes(x = date, y = daily_dead),
                    type = "line", name = "Dead") %>% 
      hc_add_series(region(), hcaes(x = date, y = round(SMA(daily_dead, 7),0)),
                    type = "line", name = "Moving Average (7)") %>% 
      hc_legend(enabled = T) %>% 
      hc_xAxis(
        title = list(text = "Day"),
        plotLines = list(
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 23/02-25/02"),
            value = datetime_to_timestamp(as.Date("2020-02-25")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/03"),
            value = datetime_to_timestamp(as.Date("2020-03-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 04/03"),
            value = datetime_to_timestamp(as.Date("2020-03-04")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 08/03-09/03"),
            value = datetime_to_timestamp(as.Date("2020-03-09")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 11/03"),
            value = datetime_to_timestamp(as.Date("2020-03-11")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 22/03"),
            value = datetime_to_timestamp(as.Date("2020-03-22")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 01/04"),
            value = datetime_to_timestamp(as.Date("2020-04-01")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 10/04"),
            value = datetime_to_timestamp(as.Date("2020-04-10")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          ),
          list(
            label = list(text = "D.P.C.M. 27/04"),
            value = datetime_to_timestamp(as.Date("2020-04-27")),
            color = '#ff0000',
            dashStyle = "shortdash",
            width = 1
          )
        )
      )
  })
  
  #--- total cases trend
  output$tot_cases_reg <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_title(text = "Total Cases Cumulative Trend") %>% 
      hc_add_series(region(), hcaes(x = date, y = total_cases),
                    type = "line", name = "Total Cases") %>% 
      hc_legend(enabled = T) %>% 
      hc_yAxis(title = list(text = "Total Cases")) %>% 
      hc_xAxis(title = list(text = "Day"))
  })
  
  #--- mortality
  output$mortality_reg <- renderHighchart({
    highchart(type = "stock") %>% 
      hc_title(text = "Mortality Rate (%)") %>% 
      hc_add_series(region(), hcaes(x = date, y = mortality),
                    type = "line", name = "Mortality Rate") %>% 
      hc_legend(enabled = T) %>% 
      hc_yAxis(title = list(text = "Rate (%)")) %>% 
      hc_xAxis(title = list(text = "Day"))
  })
  
}

