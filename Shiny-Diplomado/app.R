
# Librerias a usar --------------------------------------------------------
library(tidyverse)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(thematic)
library(dashboardthemes)
library(lubridate)
library(DT)
library(priceR)
library(dygraphs)
library(xts)
library(shinyWidgets)
library(memoise)
# Otros parametros
options(scipen=999)

# Funcion para obtener datos ----------------------------------------------


obtener_indicadores <- function(empresa = "FALABELLA") {
  url <- stringr::str_c("https://www.elmercurio.com/inversiones/json/json.aspx?categoria=",
                        empresa, "&time=10&indicador=2")
  df <- jsonlite::read_json(url)$Data %>%
    stringr::str_split(";") %>%
    dplyr::first() %>%
    I() %>%
    readr::read_delim(delim = ",", col_names = c("fecha", "precio", "vol"))
  df <- df %>%
    mutate(
      fecha = lubridate::ymd_hms(fecha),
      anio = lubridate::year(fecha)
    )
  df
}

# Lista de empresas -------------------------------------------------------


lista_empresas <- c("NUEVAPOLAR","SMU","BESALCO", "COPEC","FALABELLA",
                    "BSANTANDER", "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD",
                    "BCI", "LTM", "ENELCHILE", "SM-CHILE B", "CCU", "PARAUCO",
                    "ITAUCORP", "AGUAS-A", "COLBUN", "ENTEL", "ECL", "CONCHATORO",
                    "RIPLEY", "AESGENER", "ANDINA-B", "SONDA", "CAP", "ILC",
                    "SALFACORP", "SECURITY", "VAPORES", "ENELGXCH", "ANTARCHILE",
                    "BANMEDICA", "EMBONOR-B", "FORUS", "IAM", "MASISA", 
                    "ORO BLANCO","SK","SMSAAM")
lista_empresas <- sort(lista_empresas)


# App ---------------------------------------------------------------------

## Header ----
header <- dashboardHeader(title = "Análisis acciones")


## Sidebar ----
sidebar <-  dashboardSidebar(
  sidebarMenu(
    id="sidebar",
    menuItem("Inicio y contexto",
             tabName = "inicio"),
    menuItem("Análisis de acciones",
             tabName = "analisis")
  )
)


## Body ----
body <- dashboardBody(
  chooseSliderSkin("Shiny",color = "#1F5861"),
  shinyDashboardThemes(
    theme = "poor_mans_flatly"
  ),
  tabItems(
    tabItem(tabName = "inicio",
            HTML("<h1>Inicio y contexto</h1>"),
            column(width = 12,
                   box(status = "primary",
                       width = 12,
                       HTML('<p>Este dashboard fue creado como evaluación para
                         el curso de Shiny dentro del diplomado en Data Science
                         en la PUC.</p>
                            <p>El contexto de esta evaluación es desarrollar
                            una pequeña aplicación en donde se pueden analizar
                            los precios históricos de las acciones obtenidas
                            a través de una función de webscrapping desde
                            la página de "Inversiones" del diario El Mercurio.')
                       
                       
                   )
            )
    ),
    tabItem(tabName = "analisis",
            HTML("<h1>Análisis de acciones</h1>"),
            column(width = 8,
                   box(
                     width = 6,
                     height = 159,
                     status = "primary",
                     selectInput("empresa",
                                 label = h3("Empresa"),
                                 choices = as.character(lista_empresas))
                   ),
                   box(
                     width = 6,
                     status="primary",
                     uiOutput("slider")
                   ),
                   box(
                     width = 12,
                     height = "100%",
                     status = "success",
                     dygraphOutput("plot_test")
                   )
            ),
            column(width = 4,
                   box(
                     width= 8,
                     height = 605,
                     status = "success",
                     dataTableOutput("pricetable")
                   )
            )
            
    )
  )
)



ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  
  
  ### Reactives -----
  
  #### Data ----
  data_final <- reactive({
    
    data <- obtener_indicadores(gsub(" ","%20",input$empresa))
    
  })
  
  ### Outputs ----
  #### Plot (no usado al final!)----
  output$plot <- renderPlot({
    
    ggplot(data_final() %>% 
             filter(between(anio,input$anio[1],input$anio[2])))+
      geom_line(aes(as.Date(fecha), precio))+
      scale_y_continuous(labels=scales::label_dollar(prefix="$"))+
      ylab("Precio (CLP)")+
      xlab("Fecha")+
      theme(axis.text = element_text(size = 15))
    
  })
  
  #### Plot (este se usara alfinal!) ----
  
  
  output$plot_test <- renderDygraph({
    datax <- data_final() %>%
      filter(between(anio,input$anio[1],input$anio[2]))
    datax$fecha <- ymd_hms(datax$fecha)
    serietiempo <- xts(x = datax$precio, order.by = datax$fecha)
    p <- dygraph(serietiempo,
                 main = paste0("Valor acciones ",input$empresa)) %>%
      dyOptions(fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#1F5861") %>%
      dyRangeSelector() %>%
      dyAxis("y", label = "Precio (CLP)",
             valueFormatter = 'function(d){return "$"+d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
             axisLabelFormatter = 'function(d){return "$"+d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)  %>%
      dyRoller(rollPeriod = 1)

    p
    
    
    
  })
  
  
  #### DT ----
  
  output$pricetable <- renderDataTable({datatable(data_final() %>% 
                                                    group_by(anio) %>%
                                                    filter(between(anio,input$anio[1],input$anio[2])) %>% 
                                                    summarise('Precio Promedio' = round(mean(precio))),
                                                  class = 'hover display compact order-column',
                                                  rownames=FALSE,
                                                  height = 80,
                                                  colnames= c('Año','Precio Promedio (CLP)'),
                                                  caption = paste0("Promedio de precios de ",input$empresa),
                                                  options = list(
                                                    dom = 'Bfrtipl',
                                                    paging = TRUE,
                                                    autoWidth = TRUE,
                                                    lengthChange = FALSE,
                                                    pageLength = 15,
                                                    searching = FALSE
                                                  ))%>% 
      formatCurrency(columns = c('Precio Promedio'),
                     digits = 0,
                     mark = '.')
    
  })
  
  #### Slider dinamico ----
  
  output$slider <- renderUI({
    
    
    sliderInput("anio",label = h3("Rango Años"),
                min   = min(data_final()$anio), 
                max   = max(data_final()$anio),
                value = c(min(data_final()$anio),max(data_final()$anio)),
                sep = "")
    
  })
  
}

shinyApp(ui, server)