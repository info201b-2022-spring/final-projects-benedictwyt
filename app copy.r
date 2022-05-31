library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")

#load in data
equipment_loss <- read.csv("russia_losses_equipment.csv")
equipment_loss_new <- mutate(equipment_loss, new_aircraft = aircraft - lag(aircraft)) %>%
  mutate(equipment_loss, new_helicopter = helicopter - lag(helicopter)) %>%
  mutate(equipment_loss, new_tank = tank - lag(tank))
  
View(equipment_loss_new)

# Set up the UI
ui <- fluidPage(
  h2("Russia Equipment Loss"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "equipment",
                   label = "Different Types of Equipments: ",
                   choices = list("Aircraft" = 1,
                                  "Helicopter" = 2,
                                  "Tank" = 3),
                   selected = 1)
    ),
    mainPanel(
      plotOutput(outputId = "line", click = "plot_click"),
      tableOutput(outputId = "data")
    )
  )
  
)

# Set up the server
server <- function(input, output) {
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
  
  output$data <- renderTable({
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

}

# Set up the app page
shinyApp(ui = ui, server = server)


