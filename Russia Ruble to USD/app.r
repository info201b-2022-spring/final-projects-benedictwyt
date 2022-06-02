library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

#load in data
exchange_rate_df <- read.csv("USD_RUB Historical Data.csv")

#cleaning and merging df

exchange_rate_df <- exchange_rate_df %>%
  mutate(date = as.Date(exchange_rate_df$Date, "%b %d,%Y")) %>%
  arrange(date) %>% 
  filter(date > "2022-02-23")

data_df <- exchange_rate_df %>%
  mutate(day = 1:nrow(exchange_rate_df))

filter_df <- data_df

#set up the UI
ui <- fluidPage(
  titlePanel("USD to Russia Ruble since the Ukraine war"),
  sidebarLayout(
    sidebarPanel(
      h3("Day Range"),
      sliderInput(
        inputId = "day",
        label = "select the day range",
        min = 1,
        max = nrow(exchange_rate_df),
        value = c(1, nrow(exchange_rate_df))
      )
    ),
    mainPanel(
      h5("This page focuses on finding the trend on USD to Russian Ruble 
         exchange rate since its invasion into Ukraine on February 24th, 2022. 
         The scatter plot demonstrates the everyday price of Russian Ruble and 
         connects them to depicts the trend. The widget on the sidebar allows
         the users to adjust the day range to zoom in the graph. If the users 
         click on the dot on the graph, the table below will show more detailed 
         information like the high and low of the price. Moreover, the 
         candlestick chart clearly visualize the fluctuation and stability of 
         this currency."),
      plotOutput(outputId = "scatter", click = "plot_click"),
      tableOutput(outputId = "data"),
      plotlyOutput(outputId = "plotlyCandle")
    )
  )
)

server <- function(input, output){
  output$scatter <- renderPlot({
    filter_df <- filter(data_df, day >= input$day[1] & day <= input$day[2])
    ggplot(data = filter_df, aes(x=day, y=Price)) + 
      ggtitle("USD to Russian Ruble exchange rate") +
      geom_point() +
      geom_line()
  })
  
  output$data <- renderTable({
    nearPoints(filter_df, input$plot_click, xvar = "day", yvar = "Price")
  })
  
  output$plotlyCandle <- renderPlotly({
    plot_ly(data = filter_df, type="candlestick", x = ~day,
            open = ~Open, close = ~Price,
            high = ~High, low = ~Low)
  })
}

shinyApp(ui = ui, server = server)

