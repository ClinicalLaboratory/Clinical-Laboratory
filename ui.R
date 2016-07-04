library(shiny)
library(ggplot2)  
library(DT)
library(zoo)
library(forecast)
library(shinyapps)
library(date)
shinyUI(fluidPage(
        title = 'Demand Forecasting',
        sidebarLayout(
                sidebarPanel(
                        conditionalPanel(
                                
                                ########################################
                                tags$hr(),
                                h4('Welcome to Clinal Laboratory Demand Forecasting Tool.'),
                                
                                ####################################################

                                
                                radioButtons("ftype", "File Type:", c("CSV" = "csv","Text" = "txt")),
                                ####################################################
                                fileInput('file1', 'Please Choose a File:',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                
                                dateInput('date',
                                          label = 'Start date of your data',
                                          value = Sys.Date()
                                ),
                                selectInput("dataFreq", "The cycle time of the data is:", 
                                            choices = c("Day of the week", "Month of the year")),
                                sliderInput("EstimatedPoints",                                  
                                            "Number of points to be estimated:",
                                            min = 1, max = 50, value = 6, step = 1),
                                tags$hr(),
                                sliderInput("Points_In_Pattern", "Number of observations in a cycle:",
                                            min = 1, max = 50, value = 7, step = 1),
                                tags$hr(),
                                submitButton("Update Parameters"),
                                tags$hr(),
                                downloadButton('downloadData', 'Download Results as CSV File')
                                #verbatimTextOutput("dateText")
                                #verbatimTextOutput("dateText2"),
                                #verbatimTextOutput("dateRangeText"),
                                #verbatimTextOutput("dateRangeText2")
                                ##########################################
                        )
                ),   
                mainPanel(
                        tabsetPanel(
                                id = 'dataset',
                                tabPanel("Holt-Winters Mult-Model",
                                         
                                         wellPanel(
                                                 plotOutput("HWMplot"),  
                                                 
                                                 fluidRow(
                                                         column(3, checkboxInput(inputId = "showGridHWM",label = strong("Show Grid on Graph"),value = FALSE)),
                                                         column(3,textInput("xaxisHWM", label =strong("X-Axis"), value = "Date")),
                                                         column(3,textInput("yaxisHWM", label =strong("Y-Axis"), value = "Original Data / Fitted Data")),
                                                         column(3,textInput("titleHWM", label =strong("Graph Title"), value = "Multiplicative Holt-Winters Model"))
                                                 ),
                                                 #DT::dataTableOutput('ex1')
                                                  DT::dataTableOutput('HWMtable')
                                         )
                                         
                                ),
                                
                                tabPanel('Holt-Winters Add-Model', 
                                         wellPanel(
                                                 plotOutput("HWAplot"),
                                                 fluidRow(
                                                         column(3, checkboxInput(inputId = "showGridHWA",label = strong("Show Grid on the Graph"),value = FALSE)),
                                                         column(3,textInput("xaxisHWA", label =strong("X-Axis"), value = "Date")),
                                                         column(3,textInput("yaxisHWA", label =strong("Y-Axis"), value = "Original Data / Fitted Data")),
                                                         column(3,textInput("titleHWA", label =strong("Graph Title"), value = "Additive Holt-Winters Model"))
                                                 ),
                                                 DT::dataTableOutput('HWAtable')
                                         )
                                         
                                ),
                                
                                tabPanel('Regression Model', 
                                         wellPanel(
                                                 plotOutput("RMplot"),
                                                 fluidRow(
                                                         column(3, checkboxInput(inputId = "showGridRM",label = strong("Show Grid on the Graph"),value = FALSE)),
                                                         column(3,textInput("xaxisRM", label =strong("X-Axis"), value = "Date")),
                                                         column(3,textInput("yaxisRM", label =strong("Y-Axis"), value = "Original Data / Fitted Data")),
                                                         column(3,textInput("titleRM", label =strong("Graph Title"), value = "Linear Regression Model"))
                                                 ),
                                                 DT::dataTableOutput('Regressiontable')
                                         )
                                ), 
                                
                                tabPanel('Models Comparison', 
                                         wellPanel(plotOutput("CompPlot"),dataTableOutput('CompTables')
                                         )    
                                ),
                                
                                tabPanel('Help and Citation', 
                                         
                                         fluidRow(
                                                 column(12,
                                                       # downloadButton('downloadHelp', 'Download pdf help file'),
                                                        downloadButton('downloadCSVDemo', 'Download sample CSV data'),
                                                        downloadButton('downloadTextDemo', 'Download sample Text data'),
                                                        downloadButton('downloadcitation', 'Download Citation'),
                                                        tags$hr(),
                                                        h4("How to Cite this software"),
                                                        h5("This software was developed using R statistical Packges and is under the open source license agreement."),
                                                        h5("When using the software please cite the following article"),
                                                        p("E. A. Mohammed and C. Naugler,
                                                          Open-Source Software for Demand Forecasting of Clinical Laboratory Test Volumes using Time Series Analysis,Journal of Pathology Informatics (Under Review)"),
                                                       
                                                        ####################################
                                                        tags$hr(), 
                                                        h4("About the Authors and Contact Information"),
                                                        fluidRow(
                                                                column(4,div(style = "height:0px;background-color: white;"),imageOutput("image2")),
                                                                column(4,div(style = "height:0px;background-color: white;"),offset=2,imageOutput("image4")),
                                                                fluidRow(
                                                                column(5,div(style = "background-color: white;"),
                                                                p("Emad A.Mohammed, Ph.D."),
                                                                p("Email: eamohamm@ucalgary.ca")),
                                                                
                                                                column(5,div(style = "background-color: white;"),offset=1,
                                                                  p("Christopher Naugler, Ph.D."),
                                                                  p("Email: Christopher.Naugler@cls.ab.ca"))
                                                                )                                                               
                                                                
                                                        ),
                                                        tags$hr(), 
                                                        h4("")
                                                        
                                                 )
                                         )
                                         
                                )
                        )
                )
        )
)
)
