#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyFiles)
library(DT)


source("src/functions.R")
source("src/funcion_berlin.R")
source("src/function_sanity_check.R")
source("src/function_reports.R")

#input <- c("data/Procolo_COVID-19_Prueba1_4Abr20.eds")
#output <- c("results/")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  ###### VALIDATION FONT
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: #ff0000;
                    font-weight: bold;
                    }
                    "))
  ),
  
  titlePanel( div(column(width = 6, h1("ARPA - Automatic analysis of RT-PCR results")), 
                  column(width = 4, tags$img(src = "images/arpa.jpeg"))),
              windowTitle="rt-PCR-analysis"),
  
  hr(),
  
  ###### SIDE BAR - CONTROLADOR DE ACCIONES
  sidebarPanel(
    
    ######## h2("Selecciona el archivo a procesar"),
    #fileInput("rtpcr", "Sube el archivo a procesar"),
    h5('Select the EDS file'),
    shinyFilesButton('file_eds', 'EDS File', 'Select the EDS file', FALSE),
    hr(),
    
    
    ######## h2("Selecciona el directorio para los resultados"),
    h5('Select the directory to store the reports'),
    shinyDirButton('directory', 'Results directory', 'Select the directory to store the reports'),
    #shinyDirChoose(input, 'out_dir', roots = c(home = '~')),
    #fileInput("dir_out", "Selecciona el directorio para almacenar los resultados"),
    hr(),
    
    ######## BUTTON TO GENERATE SUMMARY TABLE
    h5('Press to start analysis'),
    actionButton("analizar", "Start analysis"),
    hr(),
    
    ######## BUTTON TO GENERATE REPORTS
    h5('Press to generate HTML reports'),
    actionButton("reportes", "Generate reports"),
    hr()

  ),
  
  ###### DISE??O DEL PANEL PARA IMPRESION DE RESULTADOS
  mainPanel(
    
    tabsetPanel(
      id = "navbar",
      tabPanel(title = "Summary table",
               value = "table",
               
               
               ###### ARCHIVO A PROCESAR    
               fluidRow(
                 h4("EDS file"),
                 textOutput("input_eds_file")
               ),
               hr(),
               hr(),
               
               
               ###### DIRECTORIO DE RESULTADOS
               fluidRow(
                 h4("Output directory"),
                 textOutput("output_dir")
               ),
               hr(),
               hr(),
               hr(),
               hr(),
               
               ###### TABLA DE RESULTADOS
               fluidRow( 
                 h3("Summary Table"),
                 #textOutput("run_ready")
                 span(textOutput(outputId = 'test_table_text'), style="color:red"), 
                 dataTableOutput(outputId = 'run_ready')
               ),
               
               hr(),
               hr(),
              
               ###### DIRECTORIO DE RESULTADOS
               fluidRow(
                 h3("Report generation"),
                 textOutput("text_reports2"),
                 textOutput("text_reports"),
                 span(textOutput("test_reports"), style="color:red") 
               )
               
      ),
      tabPanel(title = "QC Analysis",
               value = "curves", 
               ###### TABLA DE RESULTADOS
               fluidRow( 
                 h3("QC Table"),
                 span(textOutput(outputId = 'test_qc'), style="color:red"),
                 #textOutput("run_ready")
                 dataTableOutput(outputId = 'qc_ready')
               ), 
               hr(),
               hr(),
               
               fluidRow( 
                 h3("QC results"),
                 span(textOutput(outputId = 'test_qc2'), style="color:red"),
                 #textOutput("run_ready")
                 textOutput("qc_label")
               ), 
               hr(),
               hr(),
               
               fluidRow(
                 h3(textOutput("caption1")),
                 span(textOutput(outputId = 'test_plot1'), style="color:red"), 
                 plotOutput("plot1"),
                 br(),
                 br(),
                 br(),
                 h2(textOutput("caption2")),
                 span(textOutput(outputId = 'test_plot2'), style="color:red"), 
                 plotOutput("plot2"),
                 br(),
                 br(),
                 br(),
                 h2(textOutput("caption3")),
                 span(textOutput(outputId = 'test_plot3'), style="color:red"), 
                 plotOutput("plot3")
               )
      ), 
      tabPanel(title = "Curves per sample",
               value = "samples", 
               
               h3("Select one sample to visualize its amplification curves"),
               br(),
               span(textOutput(outputId = 'test_table_text2'), style="color:red"), 
               br(),
               selectInput(inputId = "sample", label = "", choices = NULL),
               br(),
               br(),
               
               h3(textOutput("caption_sample")),
               plotOutput("plot_sample")
      )
    )
  )
)


###### SERVIDOR
server <- function(input, output, session) {
  
  ###### LEER ESTRUCTURA DE DIRECTORIOS LOCAL
  ###### DEPENDE DEL SISTEMA OPERATIVO
  osSystem <- Sys.info()["sysname"]
  
  if (osSystem == "Darwin"){
    
    volumes <- getVolumes()()
    ###### DESPLIEGUE PARA LA ELECCION DEL ARCHIVO EDS A PROCESAR
    shinyFileChoose(input,'file_eds', roots=volumes, session=session)
    
    input_eds_file <- reactive({
      inFile <- parseFilePaths(volumes, input$file_eds)
      inFile.path <- as.character(inFile$datapath)
    })
    
    ###### DESPLIEGUE PARA LA ELECCION DEL DIRECTORIO DE SALIDA
    
    shinyDirChoose(input, 'directory', roots=volumes, session=session)
    
    output_dir <- reactive({
      return(print(parseDirPath(volumes, input$directory)))
    })
  }
  else{
    ###### DESPLIEGUE PARA LA ELECCION DEL ARCHIVO EDS A PROCESAR
    shinyFileChoose(input,'file_eds', roots=c('wd' = '/home/'), session=session)
    
    input_eds_file <- reactive({
      inFile <- parseFilePaths(c('wd' = '/home/'), input$file_eds)
      inFile.path <- as.character(inFile$datapath)
    })
    
    
    ###### DESPLIEGUE PARA LA ELECCION DEL DIRECTORIO DE SALIDA
    
    shinyDirChoose(input, 'directory', roots=c('wd' = '/home/'), session=session)
    
    output_dir <- reactive({
      return(print(parseDirPath(c('wd' = '/home/'), input$directory)))
    })
  }
      
    
  ####### IMPRIMIR LA RUTA DEL ARCHIVO EDS A PROCESAR
  output$input_eds_file <- renderText({
    input_eds_file()
  })
  
  
  ####### IMPRIMIR EL DIRECTORIO DE SALIDA
  output$output_dir <- renderText({
    output_dir()
  })
  
  ####### CORRER EL PROCESO DE CLASIFICACION AL DARLE CLICK AL BOTON ANALIZAR
  table_out <- eventReactive(input$analizar, {
    
    
    rtpcr <- input_eds_file()
    
    output <- output_dir()
    
    ######## VALIDATE THAT  INPUT EDS FILE WAS SELECTED
    ######## OTHERWISE PRINT TEXT DESCRIBIING THE ERROR
    validate(
      need(rtpcr != "", "NO EDS FILE WAS SELECTED")
    )
    
    ######## VALIDATE THAT OUTPUT DIRECTORY WAS SELECTED
    ######## OTHERWISE PRINT TEXT DESCRIBIING THE ERROR
    validate(
      need(output != "", "NO OUTPUT DIRECTORY WAS SELECTED")
    )
    
    
    ####### VALIDACIONES DE ENTRADA: HAS RESULTS, PROBE NAMES, QC NAMES
    sanity_result <- funcion_sanity_checks(input_eds = rtpcr)
    validate(
      need(sanity_result == "PASS", sanity_result)
    )
    
    withProgress(message = 'Analysis in process', value = 0.3, {
      all_results <- funcion_berlin(
        input_eds = rtpcr, 
        output = paste(output, "/", sep=""))
      
      #all_results$qc_results <- "ERROR"
      #all_results$test_results <- "ERROR-TABLE"
      #all_results$single_plots <- "ERROR-PLOTS"
      
      #all_results <- list(
      #  test_results = "EROR",
      #  qc_results = "qc_results", 
      #  single_plots = "single_plots"
      #)
    })
    
    return(all_results)
    
  })
  
  ####### IF THE COMPUTATION OF TEST_RESULTS GENERATED AN ERROR, PRINT THE ERROR
  output$test_table_text <- renderText({
    if (is.character(table_out()$test_results)){
      table_out()$test_results
    }
  })
  
  ####### DESPLEGAR TABLA DE RESULTADOS
  output$run_ready <- renderDataTable({
    table_out()
    
    if (is.data.frame(table_out()$test_results)){
      datatable(table_out()$test_results) %>% 
        formatStyle( 'classification', 
                     target = 'row',
                     backgroundColor = styleEqual(c("Positivo", "Negativo"),
                                                  c('pink', 'aquamarine')) )
    }
    
    #qc <- table_out()$qc_results
    
    #if (qc$QC != "PASS"){
    #  datatable(table_out()$test_results) %>% 
    #    formatStyle('gen_e', 
    #                target='row',
    #                backgroundColor = "yellow" )
    #}else{
    #  datatable(table_out()$test_results) %>% 
    #    formatStyle( 'classification', 
    #                 target = 'row',
    #                 backgroundColor = styleEqual(c("Positivo", "Negativo"),
    #                                              c('pink', 'aquamarine')) )
    #}
  })
  
  
  ################################################################################
  # IMPRIMIR CURVAS DE QC 
  ################################################################################
  ####### IF THE COMPUTATION OF TEST_RESULTS GENERATED AN ERROR, PRINT THE ERROR
  output$test_qc <- renderText({
    if (is.character(table_out()$qc_results)){
      table_out()$qc_results
    }
  })
  
  ####### DESPLEGAR TABLA DE QC
  output$qc_ready <- renderDataTable({
    table_out()
    qc <- table_out()$qc_results
    if (is.list(qc)){
      qc_table <- qc$qc.values
      datatable(qc_table) 
    }
    
  })
  
  ####### DESPLEGAR CLASIFICACION FINAL DE QC
  
  output$test_qc2 <- renderText({
    if (is.character(table_out()$qc_results)){
      table_out()$qc_results
    }
  })
  
  output$qc_label <- renderText({
    table_out()
    qc <- table_out()$qc_results
    if (is.list(qc)){
      qc_final <- qc$QC
      qc_final
    }
  })
  
  ####### PLOTS NTC
  output$caption1 <- renderText({
    "NTC"
  })
  
  output$test_plot1 <- renderText({
    if (is.character(table_out()$single_plots)){
      table_out()$single_plots
    }
  })
  
  output$plot1 <- renderPlot({
    table_out()
    plots <- table_out()$single_plots
    if (is.list(plots)){
      (plots[['NTC']] + ggtitle("NTC"))
    }
  })
  
  ####### PLOTS PTC
  
  output$caption2 <- renderText({
    "PTC"
  })
  
  output$test_plot2 <- renderText({
    if (is.character(table_out()$single_plots)){
      table_out()$single_plots
    }
  })
  
  output$plot2 <- renderPlot({
    table_out()
    plots <- table_out()$single_plots
    if (is.list(plots)){
      (plots[['PTC']] + ggtitle('PTC'))
    }
  })

  ####### PLOTS CRE
  
  output$caption3 <- renderText({
    'CRE'
  })
  
  output$test_plot3 <- renderText({
    if (is.character(table_out()$single_plots)){
      table_out()$single_plots
    }
  })
  
  output$plot3 <- renderPlot({
    table_out()
    plots <- table_out()$single_plots
    if (is.list(plots)){
       (plots[['CRE']] + ggtitle('CRE'))
    }
  })
  
  ################################################################################
  #Get sample names for drop-down menu
  ################################################################################
  
  ####### IF THE COMPUTATION OF TEST_RESULTS GENERATED AN ERROR, PRINT THE ERROR
  output$test_table_text2 <- renderText({
    if (is.character(table_out()$test_results)){
      table_out()$test_results
    }
  })
  
  observe({
    if (is.data.frame(table_out()$test_results)){
      updateSelectInput(session = session, inputId = "sample", choices = table_out()[['test_results']]$sample_name)
    }
  })
  
  output$caption_sample <- renderText({
    table_out()
    sample <- input$sample
    sample
  })
  
  output$plot_sample <- renderPlot({
    table_out()
    sample <- input$sample
    plots <- table_out()$single_plots
    if (is.list(plots)){
      plot <- plots[[sample]]
      (plot)
    }
  })

  
  ####### CORRER EL PROCESO DE GENERACION DE REPORTES AL DARLE CLICK AL BOTON REPORTES
  
  text_reports_status <- eventReactive(input$reportes, {
    
    rtpcr <- input_eds_file()
    output <- output_dir()
    
    all_results <- table_out()

    if (!is.character(all_results$single_plots) & !is.character(all_results$test_results) & !is.character(all_results$qc_results)){
      withProgress(message = 'reports in process', value = 0.3, {
        function_reports(
          results_list = table_out(), 
          input_eds = rtpcr, 
          output = paste(output, "/", sep=""))
      })
    }
    
    return(output)
  })
  
  ####### IMPRIMIR EL DIRECTORIO DE SALIDA
  output$text_reports <- renderText({
    text_reports_status()
  })
  
  ####### IMPRIMIR EL DIRECTORIO DE SALIDA
  output$text_reports2 <- renderText({
    "REMEMBER THAT THE ANALYSIS OF THE SAMPLE SHOULD BE COMPLETED BEFORE THE REPORTS CAN BE GENERATED"
})
  
  output$test_reports <- renderText({
    all_results <- table_out()
    if (is.character(all_results$single_plots) | is.character(all_results$test_results) | is.character(all_results$qc_results)){
      "THERE WAS AN ERROR DURING THE ANALYSIS, THE REPORT WILL NOT BE GENERATED"
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

