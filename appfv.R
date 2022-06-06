#Package---------------------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(fmsb)
library(shinyWidgets)
library(ggplot2)
library(plotly)

#Bufan Part----------------------------------------------------------------------------------

bufan_intro_page <- tabPanel(
  "Introduction", 
  img(src = "peace.jpeg", height = 400*1.095, width = 800*1.095),
  h1("Introduction"),
  p("Ever since the war between Russia and Ukraine has been declared,
    it became one of the topics that each individual should know.
    For data scientists, the war provides us the opportunities to collect data on modern war,
    to see what equipments are commonly used in a modern wa. We also want to know what impact
    the war will cause on currency and the stock market, and which industry was damaged the most."),
  p(),
  p("Our ptoject will first analyzes how the war impace the USD to Russian Ruble exchange rate
    on \"Currency Analysis\" page using \"Russia Ruble to USD/USD_RUB Historical Data.csv\"."),
  p(),
  p("Our project will then analyzes three main types of equipments that used by Russia on 
    \"Equipment Loss Analysis\" page using \"russia_losses_equipment.csv\". This dataset was collected
    by Armed Forces of Ukraine and Ministry of Defence of Ukraine. Those data are about the 
    equipment losses of Russia on each date. Equipment includes tank, aircraft, and helicopters ect."),
  p(),
  p("Our project will finally analyzes the impact on russian coporations' stock value and which industry
    is most significantly damaged on \"Russia Based Firms' Stock Values Analysis\" page using
    \"Radar Chart data/VTBR Historical Data.csv\" , \"Radar Chart data/QIWIDR Historical Data.csv\"
    \"Radar Chart data/MTSS Historical Data.csv\", \"Radar Chart data/LKOH Historical Data.csv\", 
    and \"Radar Chart data/AFLT Historical Data.csv\".")
)

bufan_takeaway_page <-
  tabPanel(
    "Takeaway", 
    h1("Summary Takeaways"),
    p("On this page, we will provide some conclusions from our perspectives after analyzing the data."),
    p(),
    h3("Currency Analysis"),
    img(src = "exchange_rate.png", height = 200, width = 400),
    p(),
    p("As we can see on the graph, the exchange rate of the USD to the Ruble 
      increased dramatically at the beginning of the war. It means that the 
      value of the Russian Ruble was depreciated. When the Biden administrateion 
      banned the U.S. imports of Russian oil and gas on May 7th, the rate reached
      its highest point. However, with the continuing of the war, the Russian 
      government jacked up the interest rate to 20% and imposed strict capital 
      controls on those wishing to exchange their rubles for dollars. These 
      policies successfully recovered the Russian ruble to its original price. 
      Nowadays, the rate is even lower than the rate before the war, which means
      the rubles one dollar exchange decrease. But it is noteworthy to 
      observe how long the policies can sustain without harming the Russia economy."),
    p(),
    h3("Equipment Loss Analysis"),
    img(src = "Equip_loss.jpg", height = 200, width = 1000),
    p(),
    p("As we can see on the graph, at the begining of the war, the losses of each equipment are a lot, which can indicate 
    the war peak is at the beginning. As times passes by, the losses of aircraft and helicopter decrease, but the
    loss of tank still fluctuates a lot, which can indicate the war is mainly on land afterwards."),
    p(),
    h3("Russia Based Firms' Stock Values Analysis"),
    img(src = "Price.jpg", height = 200, width = 800),
    p(),
    p("As we can see on the graph, basically all industries we analyzed (Bank, E-Bank, Telecom, Oil, and Airline) 
      got damaged severely during the war(Example.Date => 5/6/2022) comparing to the status before the war (Example.Date => 2/10/2022). 
      In addition, we can see that the damage to E-bank, Bank, and Airline industries reamins to be relatively heavier than the 
      industries of Oil and Telecom as the war begin to cool down, meaning those industies took more time to recover.")
  )

#Joshua ----------------------------------------------------------------------------------------
    #load in data
exchange_rate_df <- read.csv("USD_RUB Historical Data.csv", fileEncoding = 'UTF-8-BOM')

    #cleaning and merging df
exchange_rate_df <- exchange_rate_df %>%
  mutate(date = as.Date(exchange_rate_df$Date, "%b %d,%Y")) %>%
  arrange(date) %>% 
  filter(date > "2022-02-23")

data_df <- exchange_rate_df %>%
  mutate(day = 1:nrow(exchange_rate_df))

filter_df <- data_df

#set up the UI
joshua_analysis_page <- tabPanel(
  "Currency Analysis",
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
      h5("This page focuses on finding the trend of USD to Russian Ruble 
         exchange rate since its invasion into Ukraine on February 24th, 2022. 
         The scatter plot demonstrates the everyday price of the Russian Ruble 
         and connects them to depict the trend. The widget on the sidebar allows
         the users to adjust the day range to zoom in on the graph. If the users
         click on the dot on the graph, the table below will show more detailed 
         information like the high and low of the price. Moreover, the 
         candlestick chart clearly visualizes the fluctuation and stability of 
         this currency."),
      plotOutput(outputId = "scatter", click = "plot_click"),
      h6("As we can observe in the chart, the exchange rate surged at the 
          beginnning of the war and peaked at day 8. After that, the rate slowly 
          fell and bottomed at day 62."),
      tableOutput(outputId = "data_joshua"),
      plotlyOutput(outputId = "plotlyCandle")
    )
  )
  )



#Derek-------------------------------------------------------------------------------------------
      #load in data
equipment_loss <- read.csv("russia_losses_equipment.csv")
equipment_loss_new <- mutate(equipment_loss, new_aircraft = aircraft - lag(aircraft)) %>%
  mutate(equipment_loss, new_helicopter = helicopter - lag(helicopter)) %>%
  mutate(equipment_loss, new_tank = tank - lag(tank))
      #UI
derek_analysis_page <- tabPanel(
  "Equipment Loss Analysis",
  h2("Russia Equipment Loss"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "equipment",
                   label = "Different Types of Equipments: ",
                   choices = list("Aircraft" = 1,
                                  "Helicopter" = 2,
                                  "Tank" = 3),
                   selected = 1),
      p("This page analyzes three main types of equipments that used by Russia, which
    are aircraft, helicopter, and tank. It can infer the battle situation from
    time to time. By clicking the dot in the graph, you can see the actual number
    of that equipment on that day.")
    ),
    mainPanel(
      plotOutput(outputId = "line", click = "plot_click"),
      tableOutput(outputId = "data_derek")
    )
  )
)

#Benedict-------------------------------------------------------------------------------------------------
        #data-----------------------------------------------------------------------------------------
bank_VTBR_df <- read.csv("Radar Chart data/VTBR Historical Data.csv", fileEncoding = 'UTF-8-BOM')
bank_VTBR_df <- select(bank_VTBR_df, Date, Price)

ebank_QIWI_df <- read.csv("Radar Chart data/QIWIDR Historical Data.csv", fileEncoding = 'UTF-8-BOM')
ebank_QIWI_df <- select(ebank_QIWI_df, Date, Price)

telecom_MTSS_df <- read.csv("Radar Chart data/MTSS Historical Data.csv", fileEncoding = 'UTF-8-BOM')
telecom_MTSS_df <- select(telecom_MTSS_df, Date, Price)

oil_LKOH_df <- read.csv("Radar Chart data/LKOH Historical Data.csv", fileEncoding = 'UTF-8-BOM')
oil_LKOH_df <- select(oil_LKOH_df, Date, Price)

airline_AFLT_df <- read.csv("Radar Chart data/AFLT Historical Data.csv", fileEncoding = 'UTF-8-BOM')
airline_AFLT_df <- select(airline_AFLT_df, Date, Price)


char_df <- merge(bank_VTBR_df, ebank_QIWI_df, by = "Date")
char_df <- merge(char_df, telecom_MTSS_df, by = "Date")
char_df <- merge(char_df, oil_LKOH_df, by = "Date")
char_df <- merge(char_df, airline_AFLT_df, by = "Date")

char_df <- setNames(char_df, c("Date", "Bank","E-Bank","Telecom", "Oil", "Airline"))
          #UI 
benedict_analysis_page <- tabPanel(
  "Russia Based Firms' Stock Values Analysis",
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
      h6(strong("Note:")),
      h6("Selecting a date allow you to vividly view the values on the table during that day via a fine Radar Chart: )",
         img(src = "Stock.jpeg", height = 49.9*1.9, width = 120*1.9)), 
      br(),
      h5(strong("Terms:")),
      h6("- Bank (VTB Bank): Russian majority state-owned bank headquartered in various federal districts of Russia"), 
      h6("- E-Bank (QIWI): Russian company that provides payment and financial services in Russia and CIS countries"), 
      h6("- Telecom (Mobile TeleSystems): Russia's largest mobile operator and a leading provider of media and digital services"), 
      h6("- Oil (The PJSC Lukoil Oil Company): Russian multinational energy corporation headquartered in Moscow, specializing in the business of extraction, production, transport, and sale of petroleum, natural gas, and petroleum products"), 
      h6("- Airline (Aeroflot-Rossiyskiye Avialinii): The flag carrier and the largest airline of Russia")), 
    mainPanel(
      h5(strong("Table Value of the Day"), h6("(Unit: USD)")),
      tableOutput(outputId = "table"),
      h5(strong("The Radar Chart")),
      plotOutput(outputId = "radar")
    )
  )
)

#End product------------------------------------------------------------------------------------------
ui <- navbarPage(
  title = "Info 201 Final Project", 
  bufan_intro_page, 
  joshua_analysis_page, 
  derek_analysis_page, 
  benedict_analysis_page, 
  bufan_takeaway_page
)

server <- function(input, output){
  #Joshua
  output$scatter <- renderPlot({
    filter_df <- filter(data_df, day >= input$day[1] & day <= input$day[2])
    ggplot(data = filter_df, aes(x=day, y=Price)) + 
      ggtitle("USD to Russian Ruble exchange rate") +
      geom_point() +
      geom_line()
  })
  
  output$data_joshua <- renderTable({
    nearPoints(filter_df, input$plot_click, xvar = "day", yvar = "Price")
  })
  
  output$plotlyCandle <- renderPlotly({
    plot_ly(data = filter_df, type="candlestick", x = ~day,
            open = ~Open, close = ~Price,
            high = ~High, low = ~Low)
  })
  
  #derek
  output$line <- renderPlot({
    if (input$equipment == 1) {
      equipment_loss <- select(equipment_loss_new, day, new_aircraft) 
      ggplot(data = equipment_loss, aes(x=day, y = new_aircraft)) + 
        geom_line() + geom_point() + xlab("Number of Days Since the War Started") +
        ylab("Number of Aircraft Loss on Each Day")
    } else if (input$equipment == 2) {
      equipment_loss <- select(equipment_loss_new, day, new_helicopter)
      ggplot(data = equipment_loss, aes(x=day, y = new_helicopter)) +
        geom_line() + geom_point() + xlab("Number of Days Since the War Started") +
        ylab("Number of Helicopter Loss on Each Day")
    } else {
      equipment_loss <- select(equipment_loss_new, day, new_tank)
      ggplot(data = equipment_loss, aes(x=day, y = new_tank)) +
        geom_line() + geom_point() + xlab("Number of Days Since the War Started") +
        ylab("Number of Tank Loss on Each Day")
    }
  })
  
  output$data_derek <- renderTable({
    if (input$equipment == 1) {
      equipment_loss <- equipment_loss_new %>% select(day, new_aircraft)
      nearPoints(equipment_loss, input$plot_click, xvar = "day",yvar = "new_aircraft")
    } else if (input$equipment == 2) {
      equipment_loss <- equipment_loss_new %>% select(day, new_helicopter)
      nearPoints(equipment_loss, input$plot_click, xvar = "day",yvar = "new_helicopter")
    } else {
      equipment_loss <- equipment_loss_new %>% select(day, new_tank)
      nearPoints(equipment_loss, input$plot_click, xvar = "day",yvar = "new_tank")
    }
  })
  
  #Benedict
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