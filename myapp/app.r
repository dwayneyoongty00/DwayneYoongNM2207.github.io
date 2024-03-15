library(shiny)
library(tidyverse)
library(lubridate)
# Read the CSV file
dogecoin_prices <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS4RH-eBfXIRq6rYzUtPCr5CvyNj3ooHdwZKCIQHh85djjAfJ76alcCHqcm1bl1tWvURB7oml_dBOKx/pub?gid=240089057&single=true&output=csv")
#modify the open_time to something reader in datetime from ludidate
dogecoin_prices <- dogecoin_prices %>%
  mutate(open_time = dmy_hm(open_time))


#create the UI for quarto from shiny
ui <- fluidPage(
  titlePanel("Dogecoin Price Chart of 2021"),
  selectInput("month", "Select Month:",
              choices = c("Whole Year", month.name),
              selected = "Whole Year"),
  plotOutput("pricePlot")
)

#now we create the server

server <- function(input, output) {
  #now we deside if we have to filter or not, if its the whole year then we dont need to filter but if we do then we filter based on the month
  output$pricePlot <- renderPlot({
    data_to_plot <- if (input$month == "Whole Year") {
      dogecoin_prices
    } else {
      dogecoin_prices %>%
        filter(month(open_time) == match(input$month, month.name))
    }
    #standard plotting BUT 
    ggplot(data_to_plot, aes(x = open_time, y = price)) +
      geom_line() +
      theme_light() +
      labs(title = paste("Time Series Plot of DOGE Prices in", input$month),
           x = "Date",
           y = "Price (USD)") + #this is something interesting as we can split and scale the X according to months or days dependent on the view they chose
      scale_x_datetime(date_breaks = if (input$month == "Whole Year") "1 month" else "1 day", 
                       date_labels = "%b %d") 
  })
}

shinyApp(ui,server)