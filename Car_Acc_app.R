#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## LIBRARIES
library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
#library(DBI)
library(shinydashboard)
### BUFFER ZONE

## Data loading
source('C:/Users/santi/Desktop/R/Projects/Car_Accidents/Data_preloading.R')

Vehice_TYPES<-unique(Veh_df$Vehicle_Type_categorical_abbreviated)

## Connection setup
# con <- dbConnect(odbc::odbc(), .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};",
#                  Database  = 'uk_car_accidents_',
#                  UID       = 'root',
#                  PWD       = 'donald_7rump$ismyh0m3boy',
#                  Port      = 3306)


### FRONT

# Define UI 
ui <- dashboardPage(
  dashboardHeader(title = 'Car Accidents'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Primer vistazo", tabName = "Primer_vistazo", icon = icon("map")),
      menuItem("Pricing", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    fluidRow(
      tabItems(
        # First tab content
        tabItem(tabName = "Primer_vistazo",
                fluidRow(
                  box(leafletOutput("mymap", height = 750)),
                  
                  box(
                    title = h2("Controles"),
                    #sliderInput("slider", "Number of observations:", 1, 100, 50)
                    sliderInput('slider1',
                                h4('Ingrese el numero de registros que desea visualizar'),
                                min = 1,
                                max = 5000,
                                value=100,
                                step = 1
                    ),
                    
                    dateInput('date1',h4('Fecha desde'),
                              min = '2017-01-01',
                              max = '2017-12-30',
                              value = '2017-01-01'
                    ),
                    
                    dateInput('date2',h4('Fecha hasta'),
                              min = '2017-01-02',
                              max = '2017-12-31',
                              value = '2017-01-31'
                    )
                  )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "widgets",
                fluidRow(
                  box(leafletOutput(outputId = 'mymap1')), #render plot of data
                  box(
                    title=h2('Parametros'),
                    column(4,
                      dateInput('date21',h4('Fecha desde'),
                              min = '2017-01-01',
                              max = '2017-12-30',
                              value = '2017-01-01'
                              ),
                      dateInput('date22',h4('Fecha hasta'),
                              min = '2017-01-02',
                              max = '2017-12-31',
                              value = '2017-01-31'
                              ),
                      checkboxGroupInput("checkgroup",h4('Tipo de vehiculo'),
          # choices = list('Otros'=c(1,16,22,17,90),'Motos'=c(2:5,18,23,97),'Transporte de pasajeros'=c(10,11),'Transporte de bienes'=c(20,21,19,98),'S/D'=-1)
                              choices = list('Autos'='Autos','Motos'='Motos')
                              )),
                    column(4,
                      numericInput('cars',label = h4('Valor promedio autos:'),min = 0,max=999999,value=50000),
                      numericInput('bikes',label = h4('Valor promedio motos:'),min = 0,max=999999,value=5000)),
                  column(4,
                      numericInput('number_of_cars',label = h4('N° de autos:'),min = 0,max=9999999,value=100000),
                      numericInput('number_of_bikes',label = h4('N° de motos :'),min = 0,max=9999999,value=5000))
                      ),
                    box(h2('Pricing'),verbatimTextOutput(
                      'tables'
                    ),width = 12)#,width = 8,height = 8)
      )
      
    )
  )
  )
  )
  )



### BACK

# Define server logic required to draw a histogram
server <- function(input, output) {
  # The currently selected tab from the first box
  # output$tabset1Selected <- renderText({
  #   input$tabset1
  # })
  
  # points <- eventReactive(input$recalc, {Sample<-sample(1:nrow(Acc),100);
  #   cbind(Acc$Longitude[Sample],Acc$Latitude[Sample] )
  # }, ignoreNULL = FALSE)
  

  output$mymap <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data =   cbind(
        Acc$Longitude[(Acc$Date>=input$date1&Acc$Date<=input$date2)],
        Acc$Latitude[(Acc$Date>=input$date1&Acc$Date<=input$date2)])[sample(1:nrow(cbind(
          Acc$Longitude[(Acc$Date>=input$date1&Acc$Date<=input$date2)],
          Acc$Latitude[(Acc$Date>=input$date1&Acc$Date<=input$date2)])),(input$slider1)),]
        ,
        clusterOptions = markerClusterOptions())
  })  
  
  
  output$mymap1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data =   cbind(
        Acc$Longitude[(Veh_df$Date>=input$date21&Veh_df$Date<=input$date22&Veh_df$Vehicle_Type_categorical_abbreviated%in%input$checkgroup)],
        Acc$Latitude[(Veh_df$Date>=input$date1&Veh_df$Date<=input$date22&Veh_df$Vehicle_Type_categorical_abbreviated%in%input$checkgroup)])
        ,
        clusterOptions = markerClusterOptions())
  })

  
  
  output$tables<-renderPrint({
    if('Autos'%in%input$checkgroup){
      Table_out<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                               Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month))/input$number_of_cars)*mean(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                       Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Amort))*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month))/input$number_of_cars)
    }
    
    if('Motos'%in%input$checkgroup){
      Table_out<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                               Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month))/input$number_of_bikes)*mean(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                       Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Amort))*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month))/input$number_of_bikes)
    }
    
    if('Autos'%in%input$checkgroup&'Motos'%in%input$checkgroup){
      Table_out<-list('Autos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                               Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month))/input$number_of_cars)*mean(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                       Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Amort))*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month))/input$number_of_cars)
      ,
      'Motos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                               Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month))/input$number_of_bikes)*mean(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                        Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Amort))*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month))/input$number_of_bikes)
      )
    }
    print(Table_out)
  },width = 1200)
 
}

# Run the application 
shinyApp(ui = ui, server = server)

