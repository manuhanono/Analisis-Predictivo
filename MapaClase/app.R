library(leaflet)
library(tidyverse)
library(shiny)


mapa1 = dat
ui <- fluidPage(
    titlePanel("Airbnb - Análisis Predictivo"),
    tabPanel("Análisis Geográfico", h1("Análisis Geográfico"),
             fluidRow(
               column(radioButtons("VerMapa", "",c("Ver los más caros","Ver los más baratos")), numericInput("Año", "Insertar un número:", min = 10, max = 51707, step = 1, value = c(10))
                      ,width = 3), column(leafletOutput("Grafico5"),width = 9)),
             fluidRow(column(h5(textOutput("TextG5")), width = 9, offset = 3)))
)

server <- function(input, output) {
  data2 <- reactive({
    data <- mapa1
    if(!is.null(input$Año) & input$VerMapa == "Ver los más caros"){
      data<- tail(dat[order(dat$realSum), ], input$Año)
      data
    }
    else {
      data<- head(dat[order(dat$realSum), ], input$Año)
      data
    }
  })
  
  output$Grafico5 <- renderLeaflet({
    data2 <- data2()
    leaflet(data2) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions(),
      popup =  ~ paste("Precio: ",round(realSum, 2), ". Habitación:", room_type, ". Distancia al centro:", round(dist, 2), ". Día:", day, ". Capacidad:", person_capacity, ". Habitaciones:", bedrooms)
    ) %>% addProviderTiles(providers$CartoDB.Positron)
  })
  
  output$TextG5 <- renderText({
   if(input$VerMapa == "Insertar cantidad: ") {""}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
