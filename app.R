#Package---------------------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(fmsb)
library(shinyWidgets)

#data-----------------------------------------------------------------------------------------
bank_VTBR_df <- read.csv("Radar Chart data/VTBR Historical Data.csv")
  bank_VTBR_df <- select(bank_VTBR_df, ï..Date, Price)

ebank_QIWI_df <- read.csv("Radar Chart data/QIWIDR Historical Data.csv")
  ebank_QIWI_df <- select(ebank_QIWI_df, ï..Date, Price)
  
telecom_MTSS_df <- read.csv("Radar Chart data/MTSS Historical Data.csv")
  telecom_MTSS_df <- select(telecom_MTSS_df, ï..Date, Price)

oil_LKOH_df <- read.csv("Radar Chart data/LKOH Historical Data.csv")
  oil_LKOH_df <- select(oil_LKOH_df, ï..Date, Price)

    
airline_AFLT_df <- read.csv("Radar Chart data/AFLT Historical Data.csv")
  airline_AFLT_df <- select(airline_AFLT_df, ï..Date, Price)


char_df <- merge(bank_VTBR_df, ebank_QIWI_df, by = "ï..Date")
char_df <- merge(char_df, telecom_MTSS_df, by = "ï..Date")
char_df <- merge(char_df, oil_LKOH_df, by = "ï..Date")
char_df <- merge(char_df, airline_AFLT_df, by = "ï..Date")

char_df <- setNames(char_df, c("Date", "Bank","Ebank","Telecom", "Oil", "Airline"))

#app----------------------------------------------------------------------------------------
ui <- fluidPage(
  h1("Impact on Russian Corporations' Stock Value"),
  em(h5("Which Industy is Most Significantly Damaged by the War?")),
  setBackgroundColor("ghostwhite"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
      inputId = "char", 
      label = "Select a Date", 
      choices = char_df$Date
    ), 
    h6("Note: Selecting a date allow you to vividly view the values on the table during that day via a fine Radar Chart: )"),
    img(src = "Stock.jpeg", height = 49.9*1.9, width = 120*1.9)),
    mainPanel(
      h5(strong("Table Value of the Day"), h6("(Unit: USD)")),
      tableOutput(outputId = "table"),
      h5(strong("The Radar Chart")),
      plotOutput(outputId = "radar")
    )
))

server <- function(input, output) {
  make_rader_df <- function(char_name){
    rd_df <- select(char_df, - Date)
    min_df <- summarise_all(rd_df, min)
    max_df <- summarise_all(rd_df, max)
    
    data_pt <- filter(char_df, Date == char_name)
    data_pt <- select(data_pt, - Date)
    
    return(do.call("rbind", list(max_df,min_df, data_pt)))
  }
  
  output$table <- renderTable({
    return(make_rader_df(input$char))
    
  })
  output$radar <- renderPlot({
    radarchart(make_rader_df(input$char))
  })
}

shinyApp(ui = ui, server = server)