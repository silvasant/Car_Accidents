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
source('Data_preloading.R')

Vehice_TYPES<-unique(Veh_df$Vehicle_Type_categorical_abbreviated)



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
                  box(leafletOutput(outputId = "mymap"),height = 12),
                  
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
                                              choices = list('Autos'='Autos','Motos'='Motos'),selected = 'Autos'
                           )),
                    column(4,
                           numericInput('cars',label = h4('Valor promedio autos:'),min = 0,max=999999,value=50000),
                           numericInput('bikes',label = h4('Valor promedio motos:'),min = 0,max=999999,value=5000)
                           ,
                           selectInput("segment",h4('Segmentar por:'),
                                       choices = list('Edad del conductor'='Age_of_Driver_bucket',
                                                      'Ubicacion'='District_Name',
                                                      'Mes'='Month'),selected = 'Ubicacion'
                           )
                    ),
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
  
  
  pal <- colorFactor(c("navy", "red"), domain = c("Autos", "Motos"))
  output$mymap1 <- renderLeaflet({
    leaflet(Veh_df %>% filter((Date>=input$date21&Date<=input$date22),
                              Vehicle_Type_categorical_abbreviated%in%input$checkgroup) %>% 
              select(Longitude,Latitude,Vehicle_Type_categorical_abbreviated,District_Name)
    ) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(radius = ~ifelse(Vehicle_Type_categorical_abbreviated == "Autos", 10, 6),
                       color = ~pal(Vehicle_Type_categorical_abbreviated),
                       stroke = T, fillOpacity = 0.5
                       ,label = ~District_Name,
                       clusterOptions = markerClusterOptions()
      )
  })
  
  
  output$tables<-renderPrint({
    
    if('Autos'%in%input$checkgroup){
      Table_Mes<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                               Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month))/input$number_of_cars)*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month)))
    }
    
    if('Motos'%in%input$checkgroup){
      Table_Mes<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                               Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month))/input$number_of_bikes)*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month)))
    }
    
    if('Autos'%in%input$checkgroup&'Motos'%in%input$checkgroup){
      Table_Mes<-list('Autos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                            Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month))/input$number_of_cars)*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Month)))
      ,
      'Motos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                            Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month))/input$number_of_bikes)*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Month)))
      )
    }
    
    if('Autos'%in%input$checkgroup){
      Table_Ubicacion<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                     Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(District_Name))/input$number_of_cars)*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(District_Name)))
    }
    
    if('Motos'%in%input$checkgroup){
      Table_Ubicacion<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                     Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(District_Name))/input$number_of_bikes)*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(District_Name)))
    }
    
    if('Autos'%in%input$checkgroup&'Motos'%in%input$checkgroup){
      Table_Ubicacion<-list('Autos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                                  Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(District_Name))/input$number_of_cars)*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(District_Name)))
      ,
      'Motos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                            Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(District_Name))/input$number_of_bikes)*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(District_Name)))
      )
    }
    
    
    if('Autos'%in%input$checkgroup){
      Table_Edad<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Age_of_Driver_bucket))/input$number_of_cars)*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Age_of_Driver_bucket)))
    }
    
    if('Motos'%in%input$checkgroup){
      Table_Edad<-list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Age_of_Driver_bucket))/input$number_of_bikes)*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Age_of_Driver_bucket)))
    }
    
    if('Autos'%in%input$checkgroup&'Motos'%in%input$checkgroup){
      Table_Edad<-list('Autos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                                             Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Age_of_Driver_bucket))/input$number_of_cars)*input$cars,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Autos'
      ) %>% pull(Age_of_Driver_bucket)))
      ,
      'Motos'=list('Prima pura promedio estimada'=(table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                                            Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Age_of_Driver_bucket))/input$number_of_bikes)*input$bikes,
      'Siniestralidad estimada'=table(Veh_df %>%  filter(Date>=input$date21&Date<=input$date22,
                                                         Vehicle_Type_categorical_abbreviated=='Motos'
      ) %>% pull(Age_of_Driver_bucket)))
      )
    }
    
    if(input$segment=='Month'){Table_final<-Table_Mes}
    if(input$segment=='District_Name'){Table_final<-Table_Ubicacion}
    if(input$segment=='Age_of_Driver_bucket'){Table_final<-Table_Edad}
    
    
    print(Table_final)
  },width = 1200)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

