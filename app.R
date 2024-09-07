
#########################################################################################
# ES LA CORRECTA #######################################################################
#########################################################################################


# cargar las librerias que utiliza nuestro script

library(shiny)
library(tidyverse)
library(DT)
library(maps)
library(rcarbon)
library(leaflet)
library(Rcpp)
library(sp)
library(RColorBrewer)
library(pander)
library(htmlwidgets)
library(ggmap)
library(openxlsx)
library(leaflet.extras)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(shinythemes)

filemaker <- read.csv("datos/BdD Mesotime.csv",header = TRUE,sep=";",check.names = F)

ui <- fluidPage(
  
  tags$img(src="Logo_BdD.png",height = "167", width = "306"), 
  
  tags$img(src="geografia.png",height = "167", width = "396"),
  
  tags$img(src="Financiar.png",height = "137", width = "436"),

  theme = shinytheme("darkly"),
  tabsetPanel( 
    
    # pestaña 0: explicación del proyecto Shiny, financiación y datos
    tabPanel("About MesoTime",
             titlePanel(br()),
             mainPanel(
               
               br(),
               
               style="width:765px;height:130px;border:1px solid #000;",
               
               h4(strong("Beta version"), align = "center"),
               h5("2023-11-02", align = "center"),
               h6(strong("HOW CITE:"), "Pardo-Gordó, S.(xxxx)", em("Journal xxxx"), "doi: ___", align = "center"),
               
               br(),
               h5(strong("General information:"), "This interactive application developed with Shiny (Cheng",em("et al.,"),"2022) 
              makes it possible to consult all the radiometric information available related to the Mesolithic in Western Europe.
              This database has different processes to explore absolute dating. All the radiocarbon 
              analysis is based on the Rcarbon package (Crema y Bevan, 2020) ssociated with the R statistical software (R Core Team, 2021). ", align = "left"),      
               
               br(),
               h5("1.", strong("Spatial representation:"), "displays the radiocarbon dates from the BP value. 
                  The application can display individualised or clustered dates. By default, the application maps the clustered radiocarbon dates.
                  The slider must be moved for the application to start displaying results", align = "left"),
               h5(strong("Spatial outputs:"),"mapping all available dates that are equal to or less than those indicated in the slider. 
                  Similarly, if the grouping option is activated, dates are grouped by large administrative units. 
                  Finally, if the cursor is placed over a point, the app indicates the name of the site.", align = "left"),
               
               br(),
               h5("2.",strong("Single date calibration:"),"calibrates the radiocarbon dates. For this purpose, the date must be selected according to its acronym (laboratory identifier).
                  By default, the application uses the IntCal20 curve (Reimer",em("et al.,"), "2020). however", 
                  "the Marine20 marine curve can be selected (Heaton", em("et al.,"), 
                  "2020) and the corresponding reservoir effect must be indicated. Finally, the application allows you to  to select whether or not to normalise the date.",
                  align = "left"),
               h5(strong("Calibration output:"),"on the one hand, it generates a graph with the date calibration and the table with the basic information of the selected date to calibrate.
                  On the other hand, it generates a table with the calibration ranges at 1 and 2 deviations. The calibration format is in BP.", align = "left"),
               
               
               br(),
               h5("3.", strong("SPD by archaeological site:"), "calculates the sum of probability per site. To do so, the field of interest must be selected.
                  By default, the application uses the range 10000-7000 cal. BP. Similarly, by default, the SPD curve smoothing using 50 years is used. 
                  Default smoothing of the SPD curve using 50 years is used. To adjust the graph to the reservoir chronology, the start and end values for the the start and end values of the cumulative probability sum must be modified. sum must be modified.", align = "left"),
               h5(strong("Output SPD by site:"),"represents on a graph both the sum of probability sum of the selected reservoir as well as a line that represents the fit of the probability sum according to the years considered. years considered.
                  Similarly, at the bottom the table with all the dates used for the elaboration of the graph. the graph.", align = "left"),
               
               br(),
               h5("4.", strong("SPD by administrative unit:"), "compares the sum of probability per geographical unit. To do so, both the Country of interest and the category to be compared must be selected
By default, the application uses the range 10000-7000 cal. BP.",
                  align = "left"),
               h5(strong("Output SPD by administrative unit:"),"generates a graph with the different SPDs according to the category being analysed.", align = "left"),
               
               br(),
               h5("5.", strong("SPD by Techno-cultural complex:"), "compares the sum of probability based on cultural information. To do so, the techno-cultural complex to be compared must be selected.
By default, the application uses the range 10000-7000 cal. BP.",
                  align = "left"),
               h5(strong("Output SPD by Techno-cultural complex:"),"generates a graph with the different SPDs according to the techno-cultural complex being analysed.", align = "left"),
               
               
               br(),
               h5("6.", strong("SPD by Mesolithic periodisation:"), "compares the sum of probability based the Mesolithic periodisation in the W. Mediterranean.
By default, the application uses the range 10000-7000 cal. BP.",
                  align = "left"),
               h5(strong("Output SPD by Mesolithic periodisation:"),"generates a graph with the different SPDs according to cultural periodisation", align = "left"),
               
               
               
               
               
               
               br(),
               h4(strong("GESTIÓN Y ACTUALIZACIÓN"), align = "center"),
               h5("Salvador Pardo-Gordó", align = "center"),
               h5("Departamento de Geografía e Historia. Universidad de la Laguna", align = "center"),
               h6(strong("E-mail:"),"spardogo@ull.edu.es", align = "center"),
             )),
    
    # pestaña 1: presenta un mapa con todos los yacimientos fechados
    tabPanel("Spatial representation",
             
             titlePanel(""),
             fluidRow(
               
               
               column(4,offset = 1,
                      sliderInput(inputId = "slider_BP", 
                                  label = "Select 14C sample based on BP variable",
                                  min = 5000,
                                  max = 10000,
                                  value = 7200,
                                  step = 1)),
               
               column(2,selectInput(
                 inputId = "viewdates",
                 label = "Cluster radiocarbon information?:",
                 choices = list("Yes",
                                "No"),
		 selected = "Yes")),
               
               
               leafletOutput("mapa_dataciones"))),
    
    # pestaña 2: calibración de la fecha radicoarbónica       
 
    tabPanel("Single date calibration",
             titlePanel(""),
             fluidRow(
               
               column(2,selectInput(
                 inputId = "Sample",
                 label = "Dating identification:",
                 choices = sort(filemaker$Laboratory))),
               
               
               column(2,selectInput(
                 inputId = "Calibration_curve",
                 label = "Selection of the calibration curve",
                 choices = list("intcal20",
                                "intcal13",
                                "marine20",
                                "marine13"))),
               
               
               column(2, numericInput(
                 inputId = "resOffsets",
                 label = "DeltaR",
                 value = 0)),
               
               
               column(2,numericInput(
                 inputId =  "resErrors",
                 label = "error DeltaR",
                 value = 0)),
               
               column(2,selectInput(
                 inputId = "norm_calibration",
                 label = "Probability normalisation?",
                 choices = list("FALSE",
                                "TRUE"))),
               
               column(width=5,offset = 1,align="center",
                      plotOutput("calibrada"))),  # esto cerra el sidebarLayout
             
             h4("Main information"),
             tableOutput("ver_informacion"),
             
             h5("Calibration table"),   
             column(8,offset = 1,verbatimTextOutput("summary_cal"))),
    
    
    # pestaña 3: elaboración de un sumatorio de  probabiblidad
    # utiliza el criterio de yacimiento arqueológico
    
    tabPanel("SPD by archaeological site",
             titlePanel(""),
             fluidRow(
               column(2,selectInput(
                 inputId = "Site",
                 label = "SPD of the site:",
                 choices = sort(filemaker$Name_site),
                 selected = filemaker$Name_site==" ")),
               
               
               
               column(2, numericInput(
                 inputId =  "Starting_spd",
                 label = "Starting of the spd cal BP",
                 value = 10000)),
               
               column(2,numericInput(
                 inputId =  "Final_spd",
                 label = "Ending of the spd cal BP",
                 value = 7000)),
               
               column(2,numericInput(
                 inputId =  "media_movil",
                 label = "yrs to smooth curve:",
                 value = 50)),
               
               
               
               column(width=5,offset = 1,plotOutput("spd_site"))),  # esto cerra el sidebarLayout
             
             h5("Main information radiocarbon dates of the site"),
             tableOutput("ver_informacion_dataciones"),),    
    
    
    # pestaña 4: elaboración de un sumatorio de  probabiblidad
    # utiliza el criterio de isla arqueológico
    # también realiza varios spd de la isla según el contexto estratigráfico
    
    tabPanel("SPD by administrative unit",
             titlePanel(""),
             fluidRow(
               column(2,selectInput(
                 inputId = "Country",
                 label = "SPD by Country:",
                 choices = sort(filemaker$Country),
                 selected = filemaker$Country==" ")),
               
               column(2,selectInput(
                 inputId = "subdividir",
                 label = "Build SPD by:",
                 choices = list("Cultural_horizon",
                                "Life",
                                "Sample",
                                "Method"),
                 selected = list("Cultural_horizon"))),
               
               
               column(2, numericInput(
                 inputId =  "Inicio_spd_isla",
                 label = "Starting of the spd cal BP",
                 value = 10000)),
               
               column(2,numericInput(
                 inputId =  "Final_spd_isla",
                 label = "Ending of the spd cal BP",
                 value = 7000)),
               
               
               column(width=8,offset = 2,plotOutput("spd_isla"))
               
             )),
    
    
    # pestaña 5: elaboración de un sumatorio de  probabiblidad
    
    tabPanel("SPD by Techno-cultural complex",
             titlePanel(""),
             fluidRow(
               column(2,selectInput(
                 inputId = "Technocomplex",
                 label = "SPD by Technocomplex:",
                 choices = sort(filemaker$Cultural_horizon),
                 selected = filemaker$Cultural_horizon==" ")),


               column(2, numericInput(
                 inputId =  "Inicio_spd_cult",
                 label = "Starting of the spd cal BP",
                 value = 10000)),

               column(2,numericInput(
                 inputId =  "Final_spd_cult",
                 label = "Ending of the spd cal BP",
                 value = 7000)),


               column(width=8,offset = 2,plotOutput("spd_cult"))

             )),

    # pestaña 6: elaboración de un sumatorio de  probabiblidad
    
    tabPanel("SPD by Mesolithic phases",
             titlePanel(""),
             fluidRow(
               column(2,selectInput(
                 inputId = "Phase",
                 label = "SPD by phase:",
                 choices = sort(filemaker$Region),
                 selected = filemaker$Region==" ")),
               
               
               column(2, numericInput(
                 inputId =  "Inicio_spd_period",
                 label = "Starting of the spd cal BP",
                 value = 10000)),
               
               column(2,numericInput(
                 inputId =  "Final_spd_period",
                 label = "Ending of the spd cal BP",
                 value = 7000)),
               
               
               column(width=8,offset = 2,plotOutput("spd_period"))
               
             )),
    
    
  ) # parentesis asociado al panel de todas las pestañas
  
  
) # parentesis asociado a ui

server <- function(input, output, session){
  
  ##########################################################################
  #             representacion de la información del PANEL 1               #
  ##########################################################################
  output$mapa_dataciones <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 42, lng = -9, zoom = 5.2)})
  
  
  df_bp <- reactive({
    filemaker[filemaker$CRA <= input$slider_BP,]
    
  })
  
  colorData <- filemaker$CRA
  
  pal <- colorBin("Greens", colorData, 9, pretty = TRUE)
  
  
  
  
  observe({
    
    if (input$viewdates == "Yes") 
    {
      
      leafletProxy(mapId = "mapa_dataciones", data = df_bp()) %>%
        clearMarkers() %>%   ## clear puntos previos
        
        # clearMarkerClusters() %>% 
        
        addCircleMarkers(
          clusterOptions = markerClusterOptions(),
          radius = 4,
          stroke = F, fillOpacity = 10,
          label =~Name_site,
          labelOptions = labelOptions(textsize = "16px",
                                      style =  list("font-style" = "italic",
                                                    "font-family" = "times")),
          lng = ~LongGM,
          lat = ~LatGM,
          
          fillColor=pal(colorData))  %>%
        
        clearControls()  ## clear legenda, así no la duplica
    }
    
    if (input$viewdates == "No") 
    {
      leafletProxy(mapId = "mapa_dataciones", data = df_bp()) %>%
        clearMarkers() %>%   ## clear puntos previos
        
        addCircleMarkers(
          radius = 4,
          stroke = F, fillOpacity = 1,
          label =~Name_site,
          lng = ~LongGM,
          lat = ~LatGM,
          labelOptions = labelOptions(textsize = "16px",
                                      style =  list("font-style" = "italic",
                                                    "font-family" = "times")),
          fillColor=pal(colorData))  %>%
        
        
        clearMarkerClusters () %>%  ## elimina los clusteres cuando hay visualización de puntos
        clearControls() %>% ## clear legenda, así no la duplica
        
        addLegend("bottomleft", pal = pal, values = input$df_bp,
                  title = "Before Present",
                  opacity = 10,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) 
    }
    
  })
 
  ##########################################################################
  #             representacion de la información del PANEL 2               #
  ##########################################################################  
  
  # selección de los datos asociados al input
  
  datos <- reactive({
    subset(filemaker,Laboratory == input$Sample,c("Name_site","Country","CRA","Error"))
    
  })
  
  
  
  
  # representación de la calibración
  output$calibrada <- renderPlot({
    
    acalibrar <- calibrate(datos()$CRA, datos()$Error, 
                           calCurves= input$Calibration_curve,
                           resOffsets=input$resOffsets,resErrors=input$resErrors,
                           normalised= input$norm_calibration)
    
    plot(acalibrar,HPD=TRUE,calendar="BP",main=datos()$Name_site)
    
  }) 
  
  # Generar una tabla con la información calibrada
  output$summary_cal <- renderPrint({
    dataset <- datos()
    dataset <- calibrate(datos()$CRA,datos()$Error)
    datasum <- summary(dataset,calendar="BP")
    datusuma <- datasum[,-1]
    pandoc.table(datusuma, justify = "centre",style ="multiline", caption = " Media y calibración a 1 / 2 sigmas")
  })
  
  # Generar una tabla con la información principal de la fecha
  
  datos_fecha <- reactive({
    subset(filemaker,Laboratory == input$Sample,c("Name_site","Country","Laboratory","CRA","Error","Level","Life","Sample","Specie",
                                                   "Cultural_horizon","Short reference"))
    
  })
  
  output$ver_informacion <-  renderTable({ 
    datos_fecha()
  }, 
  bordered = TRUE,
  spacing = 'xs',
  width = '75%', align = 'c')  
  
  
  ##########################################################################
  #             representacion de la información del PANEL 3               #
  ##########################################################################  
  
  # selección de los datos asociados al input
  
  datos_spd_2 <- reactive({
    subset(filemaker,Name_site == input$Site,c("CRA","Error"))
  })
  
  output$spd_site <- renderPlot({
    Calib_matrix_2 <- calibrate(datos_spd_2()$CRA,datos_spd_2()$Error)
    sumprob_2 <-spd(Calib_matrix_2,timeRange = c(input$Starting_spd,input$Final_spd))
    plot(sumprob_2,calendar="BP",main=input$Site, fill="purple")
    plot(sumprob_2,calendar="BP",runm=input$media_movil,add=TRUE,
         type="simple",col="red",lwd=2,lty=1)
    
  })
  
  # Generar una tabla con la información principal de la fecha
  
  datos_spd_site <- reactive({
    subset(filemaker,Name_site == input$Site,c("Name_site","Country","Laboratory",
                                                       "CRA","Error","Level","Life",
                                                       "Sample","Specie",
                                                       "Cultural_horizon","Short reference"))
    
  })
  
  output$ver_informacion_dataciones <-  renderTable({ 
    datos_spd_site()
  }, 
  bordered = TRUE,
  spacing = 'm',
  width = '85%', align = 'c') 
  
  
  ##########################################################################
  #             representacion de la información del PANEL 4               #
  ##########################################################################  
  
  datos_spd_isla <- reactive({
    subset(filemaker,Country == input$Country,c("Name_site","CRA","Error","Life","Cultural_horizon","Method","Sample"))
  })
  
  
  output$spd_isla<- renderPlot({
    Calib_matrix_isla <- calibrate(datos_spd_isla()$CRA,datos_spd_isla()$Error)
    sumprob_isla <- stackspd(x=Calib_matrix_isla,
                             group = if (input$subdividir == "Life") 
                             {datos_spd_isla()$Life}
                             else if (input$subdividir == "Cultural_horizon")
                             {datos_spd_isla()$Cultural_horizon}
                             else if (input$subdividir == "Sample")
                             {datos_spd_isla()$Sample}
                             else if (input$subdividir == "Method")
                             {datos_spd_isla()$Method},
                             timeRange = c(input$Inicio_spd_isla,input$Final_spd_isla),
                             bins = NA)
    
    plot(sumprob_isla,type='stacked',calendar="BP") })
  

  ##########################################################################
  #             representacion de la información del PANEL 5               #
  ##########################################################################  
  
  datos_spd_cultura <- reactive({
    subset(filemaker,Cultural_horizon == input$Technocomplex,c("CRA","Error","Country","Cultural_horizon"))
  })

  output$spd_cult<- renderPlot({
    Calib_matrix_cult <- calibrate(datos_spd_cultura()$CRA,datos_spd_cultura()$Error)

    sumprob_period <- stackspd(x=Calib_matrix_cult,
                              group = datos_spd_cultura()$Country,
                              timeRange = c(input$Inicio_spd_period,input$Final_spd_period),bins = NA,
                             datenormalised = FALSE)

    plot(sumprob_period,type='multipanel',calendar="BP") })


  ##########################################################################
  #             representacion de la información del PANEL 5               #
  ##########################################################################  
  
  datos_spd_period <- reactive({
    subset(filemaker,Region == input$Phase,c("CRA","Error","Region","Periodization"))
  })
  
  output$spd_period<- renderPlot({
    Calib_matrix_period <- calibrate(datos_spd_period()$CRA,datos_spd_period()$Error)
    
    sumprob_cult <- stackspd(x=Calib_matrix_period ,
                             group = datos_spd_period()$Periodization,
                             timeRange = c(input$Inicio_spd_period,input$Final_spd_period),bins = NA,
                             datenormalised = FALSE)
    
    plot(sumprob_cult,type='multipanel',calendar="BP") })
  
} # clado del server

shinyApp(ui, server)
