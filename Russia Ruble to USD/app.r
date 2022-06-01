library(shiny)
library(dplyr)
library(ggplot2)

#load in data
exchange_rate_df <- read.csv("USD_RUB Historical Data.csv")
russia_losses_df <- read.csv("russia_losses_equipment.csv")

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
  titlePanel("Russia Ruble to USD since the Ukraine war"),
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
      #plotOutput(outputId = "scatter", hover = "plot_click"),
      plotOutput(outputId = "scatter"),
      #tableOutput(outputId = "data")
    )
  )
)

server <- function(input, output){
  output$scatter <- renderPlot({
    filter_df <- filter(data_df, day >= input$day[1] & day <= input$day[2])
    ggplot(data = filter_df, aes(x=day, y=Price)) + 
      geom_point() +
      geom_line()
  })
  
  #output$data <- renderTable({
  #  nearPoints(filter_df, input$plot_click, xvar = "day", yvar = "Price")
  #})
}

shinyApp(ui = ui, server = server)

