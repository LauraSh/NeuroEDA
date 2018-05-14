
  
  fluidPage(
  
    
    fluidRow(
     
      column(12, offset = 0,
             tags$img(src = 'images/header2.jpg' , height = '85px', align='left')
            
      )      
    ),
    

    
    navbarPage("DATASET", collapsible = TRUE, windowTitle = "data visualisation",
               tabPanel("Upload",
                        sidebarLayout(
                          sidebarPanel(width=3,
                            fileInput('file1', 'Choose CSV File',
                                      accept=c( 'text/csv',
                                                'text/comma-separated-values',
                                                'text/tab-separated-values',
                                                'csv',
                                                'tsv')
                                      
                                      
                                      ),
                            tags$hr(),
                            checkboxInput('header', 'Header', TRUE),
                            radioButtons('sep', 'Separator',
                                         c(Comma=',',
                                           Semicolon=';',
                                           Tab='\t'),
                                         ','),
                            radioButtons('quote', 'Quote',
                                         c(None='',
                                           'Double Quote'='"',
                                           'Single Quote'="'"),
                                         '"'),
                            tags$br(),
                            actionButton("tut1", "Tutorial" ),
                            tags$br(), tags$br(),
                            actionButton("news", "What's new")
                          ),
                          mainPanel(width = 9,
                            verbatimTextOutput("head"),
                            tags$br(),
                            
                            tableOutput("head2"),
                            tags$p(tags$h4(" ..."))
                          )
                        )
               ),
               
               tabPanel("Summary",
                        
                        mainPanel(
                          verbatimTextOutput("summary"),
                          actionButton("tut2", " Tutorial" ),
                          width = 12
                        )
                        
               ),
               
               
               tabPanel("Table",
                        
                        mainPanel(
                          dataTableOutput("contents"),
                          actionButton("tut3", " Tutorial" ),
                          width = 12
                        )
               ),
               
               tabPanel("Plot",
                        sidebarLayout(
                          sidebarPanel( width=3,
                            radioButtons('plots', 'Plot type:',
                                         c('Boxplot'= "bxp",
                                           'Histogram'= "hst",
                                           'Barplot'= "brp",
                                           'Scatterplot'= "scp",
                                          
                                           'Time-series plot'= "tsp"
                                           
                                           ), selected = "bxp"),
                            conditionalPanel("input.plots ==='bxp' || input.plots ==='hst'",
                                             selectInput('x1', 'Y Variable', names(inFile)),
                                             selectInput('y1', 'Facet', choices = c("none" , names(inFile)), selected="none")),
                            
                            conditionalPanel("input.plots ==='brp'",
                                             selectInput('x4', 'X Variable', names(inFile)),
                                             selectInput('y4', 'Fill - factor', choices = c("none" , names(inFile)), selected="none")),
                            
                            conditionalPanel("input.plots ==='tsp'",
                                             selectInput('x3', 'Y Variable', names(inFile)),
                                             selectInput('y3', 'Color', choices = c("none" , names(inFile)), selected="none")),
                            
                            
                                             
                          
                            conditionalPanel("input.plots ==='scp'" ,
                                             selectInput('x2', 'X Variable', names(inFile)),
                                             selectInput('y2', 'Y Variable', names(inFile), selected=names(inFile)[[2]]),
                                             selectInput('z2', 'Color', choices = c("none" , names(inFile)), selected="none")
                                             ),
                            actionButton("tut4", " Tutorial" )
                                   
                           
                          ),
                          mainPanel(width=9,
                            plotOutput("plotPlot")
                          )
                        )
               ),
               
               navbarMenu("Analysis",
               
               tabPanel("Correlation analysis",
                        
                        fluidPage(
                          
                          wellPanel(
                            
                            checkboxGroupInput('ggvars', 'Columns:',
                                               names(inFile), selected = names(inFile)[[1]], inline = TRUE),
                            tags$style(type="text/css", HTML("#ggvars>*{float: left; margin-right: 15px; height: 20px;} # {height: 20px;}"))
                          ),
                        
                          
                          plotOutput("plot2", height="480px"), 
                          tags$br(),
                          tags$br(),
                          actionButton("tut5", " Tutorial" ),
                          tags$br(),
                          tags$br(),
                          width = 12
                          
                         
                        )
               ),
               
               
               
               
              
               
               tabPanel("Regression",
                        
                        
                        sidebarLayout(
                        
                          sidebarPanel(width=3,
                            selectInput('xcol2', 'X Variable', names(inFile)),
                            selectInput('ycol2', 'Y Variable', names(inFile), selected=names(inFile)[[2]]),
                            radioButtons("line", "Type",
                                         c("linear", "robust", "loose")),
                            selectInput('fac2', 'Facet:', choices = c("none" , names(inFile)), selected="none"),
                            actionButton("tut6", " Tutorial" )
                            
                          ),
                          mainPanel( width=9,
                            plotOutput('ggp'),
                            
                         
                            
                            tags$p("Coefficients"),
                            verbatimTextOutput("coefs") ,
                            
                            conditionalPanel(
                              condition = "input.line == 'robust'",
                              
                              tags$p("Robust coefficients")
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.line == 'robust'",
                              
                              verbatimTextOutput("coefsr")
                              
                            )
                            
                           
                        )
               
                        )
                        ),
               
               
               tabPanel("K-means clustering",
                        
                        
                        
                        
                        sidebarPanel(width=3,
                                     selectInput('xcol', 'X Variable', names(inFile)),
                                     selectInput('ycol', 'Y Variable', names(inFile), selected=names(inFile)[[2]]),
                                     numericInput('clusters', 'Cluster count', 2,
                                                  min = 1, max = 9),
                                     actionButton("tut7", " Tutorial" )
                                     
                                     
                        ),
                        mainPanel(width=9,
                                  plotOutput('plot1',
                                             
                                             click = "km_click",
                                             dblclick = dblclickOpts(
                                               id = "km_dblclick"
                                             )),
                                  fluidRow(
                                    column(width = 12,
                                           verbatimTextOutput("click_info")
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 12,
                                           verbatimTextOutput("dblclick_info")
                                    )
                                  )
                        )
               ), 
                    
                        
               tabPanel("mClust",
                       fluidPage(
                          
                          
                          
                          wellPanel(
                            fluidRow(
                              
                              column(12, 
                                     radioButtons("sel", "Selection:",
                                                  c("BIC", "classification", "uncertainty", "density"), inline = TRUE),
                                     tags$style(type="text/css", HTML("#sel>*{float: left; margin-right: 15px; height: 20px;} # {height: 20px;}"))
                                     
                                     
                              )      
                            ),
                            fluidRow(
                              
                              column(12, 
                                     radioButtons("noc", "Max number of components:",
                                                  c(2, 3, 4, 9, 15, 20), inline = TRUE, selected=9),
                                     tags$style(type="text/css", HTML("#noc>*{float: left; margin-right: 15px; height: 20px;} # {height: 20px;}"))
                                     
                                     
                              )  ),
                            fluidRow(
                            
                            column(12, checkboxGroupInput('mclvars', 'Columns:',
                                               names(inFile), selected = c(names(inFile)[[1]] , names(inFile)[[2]]), inline = TRUE),
                            tags$style(type="text/css", HTML("#mclvars>*{float: left; margin-right: 15px; height: 20px;} # {height: 20px;}"))
                         )),
                         
                         fluidRow(''),
                         fluidRow('')
                         
                        
                         
                         ),
                         
                         
                          mainPanel(
                            plotOutput("mcl", height="480px"),
                            tags$br(),
                            actionButton("tut8", " Tutorial" ),
                            tags$br(),
                            tags$br(),
                            width = 12
                          )
                        )
                        
                        
                       
                        )
               ),
               
               
               tabPanel("Authors",
                        
                        
                          mainPanel( 
                            tags$br(),tags$br(),tags$br(),
                            tags$p("." , align = "center"),
                            tags$br(),
                            tags$p("Authors" , align = "center"),
                            tags$p("Laura Shala" , align = "center"),
                            tags$p("Ondřej Klempíř" , align = "center"),
                            tags$br(),
                            tags$p("Department of Biomedical Informatics" , align = "center"),
                            tags$p("Faculty of Biomedical Engineering . CTU Prague" , align = "center"),
                            tags$p(tags$a(href = "http://kbi.fbmi.cvut.cz", "http://kbi.fbmi.cvut.cz") , align = "center"),
                            tags$br(),
                            tags$p("2017" , align = "center"),
                            tags$br(),tags$br(),tags$br(),
                            tags$p(tags$a(href="https://shiny.rstudio.com/" , "Shiny . RStudio") , align = "center"),
                            
                            width=12
                          )
                        )
               
    )
               
    )
    
  



  


