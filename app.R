library(shiny)
library(dplyr)
library(fmsb)

bank_VTBR_df <- read.csv("VTBR Historical Data.csv")
  bank_VTBR_df <- select(bank_VTBR_df, ï..Date, Price)

ebank_QIWI_df <- read.csv("QIWIDR Historical Data.csv")
  ebank_QIWI_df <- select(ebank_QIWI_df, ï..Date, Price)
  
telecom_MTSS_df <- read.csv("MTSS Historical Data.csv")
  telecom_MTSS_df <- select(telecom_MTSS_df, ï..Date, Price)

oil_LKOH_df <- read.csv("LKOH Historical Data.csv")
  oil_LKOH_df <- select(oil_LKOH_df, ï..Date, Price)

    
airline_AFLT_df <- read.csv("AFLT Historical Data.csv")
  airline_AFLT_df <- select(airline_AFLT_df, ï..Date, Price)


char_df <- merge(bank_VTBR_df, ebank_QIWI_df, by = "ï..Date")
char_df <- merge(char_df, telecom_MTSS_df, by = "ï..Date")
char_df <- merge(char_df, oil_LKOH_df, by = "ï..Date")
char_df <- merge(char_df, airline_AFLT_df, by = "ï..Date")

char_df <- setNames(char_df, c("Date", "Bank","Ebank","Telecom", "Oil", "Airline"))

ui <- fluidPage(
  selectInput(
    inputId = "char", 
    label = "Select a Date", 
      choices = char_df$Date
  ),
  tableOutput(outputId = "table"), 
  plotOutput(outputId = "radar")
)

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