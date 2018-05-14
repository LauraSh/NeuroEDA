options(shiny.maxRequestSize=10*1024^2) 

function(input, output, clientData, session) {


datasetInput <- reactive({

inFile <- input$file1

if (is.null(inFile))
return(NULL)
validate(
need(input$file1 %in% c(
'text/csv',
'text/comma-separated-values',
'text/tab-separated-values',

'csv',
'tsv'
), "Wrong file format,  please select .csv or .tsv file"))




read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
})



output$summary <- renderPrint({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

summary(inFile)


})

output$head <- renderPrint({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

str(inFile)

})

output$head2 <- renderTable({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

head(inFile)

})


output$contents <- renderDataTable({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

inFile},
options = list(
pageLength = 10
)

)


###############################################################plots##################################
observe({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

updateSelectInput(session, "x1",
choices = names(inFile)
)
updateSelectInput(session, "y1",
choices = c(names(inFile),"none"), selected="none"
)
updateSelectInput(session, "x2",
choices = names(inFile)
)

updateSelectInput(session, "y2",
choices = names(inFile), selected=names(inFile)[[2]]
)
updateSelectInput(session, "z2",
choices = c(names(inFile), "none"), selected="none"
)
updateSelectInput(session, "x3",
                  choices = names(inFile)
)
updateSelectInput(session, "y3",
                  choices = c(names(inFile),"none"), selected="none"
)

updateSelectInput(session, "x4",
                  choices = names(inFile)
)
updateSelectInput(session, "y4",
                  choices = c(names(inFile),"none"), selected="none"
)


})


pdata1 <- reactive({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

if(input$y1 == "none"){
inFile[, c(input$x1, input$x1)]}
else{
inFile[, c(input$x1, input$y1)]
}
})

pdata2 <- reactive({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

if(input$z2 == "none"){
inFile[, c(input$x2, input$y2)]}
else{
inFile[, c(input$x2, input$y2, input$z2)]
}
})

pdata3 <- reactive({
  
  inFile <- datasetInput()
  if (is.null(inFile))
  {inFile = default_dataset}
  
  if(input$y3 == "none"){
    inFile[, c(input$x3, input$x3)]}
  else{
    inFile[, c(input$x3, input$y3)]
  }
})

pdata4 <- reactive({
  
  inFile <- datasetInput()
  if (is.null(inFile))
  {inFile = default_dataset}
  
  if(input$y4 == "none"){
    inFile[, c(input$x4, input$x4)]}
  else{
    inFile[, c(input$x4, input$y4)]
  }
})


output$plotPlot <- renderPlot({

if(input$plots == "bxp"){
if(input$y1 == "none"){

p <- ggplot(pdata1(), aes(x="pdata1()[,1]", y=pdata1()[,1])) 
p + geom_boxplot() + theme_light() + labs(x="", y=input$x1) 
}
else{
p <- ggplot(pdata1(), aes(factor(pdata1()[,2]), pdata1()[,1]))
p + geom_boxplot() + theme_light() + labs(x=input$y1, y=input$x1) 

}
}
else if(input$plots == "hst"){

if(input$y1 == "none"){

p <- ggplot(pdata1(), aes(pdata1()[,1]))
p + geom_histogram(bins = 25) + theme_light() + labs(x=input$x1)
}
else{
ahist = pdata1()
ahist$fac = inFile[, input$y1]
p <- ggplot(ahist, aes_string(input$x1))
p + geom_histogram(bins = 25) +  facet_wrap(~fac, scales = 'free_x') + theme_light() + labs(x=input$x1)

}

}
  
  else if(input$plots == "brp"){
    
    if(input$y4 == "none"){
      
      p<-ggplot(pdata4(), aes(pdata4()[,1], ..count..)) 
      p + geom_bar() + theme_light() + labs(x=input$x4)

    }
    else{
      p<-ggplot(pdata4(), aes(pdata4()[,1], ..count..)) 
      p + geom_bar(aes(fill = factor(pdata4()[,2])), position = "dodge" ) + theme_light() + labs(x=input$x4)
    }
    
  }
  
  else if(input$plots == "tsp"){
    if(input$y3 == "none"){

      p <- ggplot(pdata3(), aes(x=1:length(pdata3()[,1]), y=pdata3()[1]))
      p + geom_line() + theme_light() + labs(x="time", y=input$x3)
    
    }
    else{
      
      p <- ggplot(pdata3(), aes(x=1:length(pdata3()[,1]), y=pdata3()[,1], colour=pdata3()[,2]))
      p + geom_line() + theme_light() + labs(x="time/index", y=input$x3)
      
    }
  }
else if(input$plots == "scp"){

if(input$z2 == "none"){

p <- ggplot(pdata2(), aes(x=pdata2()[,1], y=pdata2()[,2]))
p + geom_point() + theme_light() + labs(x=input$x2, y=input$y2)
}
else{
p <- ggplot(pdata2(), aes(x=pdata2()[,1], y=pdata2()[,2]))
p + geom_point(aes(color=factor(pdata2()[,3]))) + theme_light() + labs(x=input$x2, y=input$y2, color=input$z2)

}
}

else{
ggplot(inFile, aes(x=pdata4()[,1] , y=pdata4()[, 2])) + geom_line(linejoin = "round",
linemitre = 5)
}
})
#################################################k-means.clustering###################

observe({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

updateSelectInput(session, "xcol",
choices = names(inFile)
)
updateSelectInput(session, "ycol",
choices = names(inFile), selected=names(inFile)[[2]]
)

})
# Combine the selected variables into a new data frame
selectedData <- reactive({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

inFile[, c(input$xcol, input$ycol)]
# inFile[, 2:3]
})

clusters <- reactive({
kmeans(selectedData(), input$clusters)
})

output$plot1 <- renderPlot({
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
            "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

par(mar = c(5.1, 4.1, 0, 1))
plot(selectedData(),
col = clusters()$cluster,
pch = 20, cex = 3)
points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
})

output$click_info <- renderPrint({
  cat("Click info:\n\n")
  if (is.null(datasetInput())){
  nearPoints(inFile, input$km_click, xvar = input$xcol, yvar = input$ycol)
  }
  else{
    nearPoints(datasetInput(), input$km_click, xvar = input$xcol, yvar = input$ycol)
  }
})
output$dblclick_info <- renderPrint({
  cat("Doubleclick info:\n\n")
  if (is.null(datasetInput())){
  nearPoints(inFile, input$km_dblclick, xvar = input$xcol, yvar = input$ycol)
  }
  else{
    nearPoints(datasetInput(), input$km_dblclick, xvar = input$xcol, yvar = input$ycol)
  }
})


###################regrese1##################################################


observe({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

updateSelectInput(session, "xcol2",
choices = names(inFile)
)
updateSelectInput(session, "ycol2",
choices = names(inFile), selected=names(inFile)[[2]]
)
updateSelectInput(session, "fac2",
choices = c("none" , names(inFile)), selected="none"
)

})


selectedData2 <- reactive({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}


if(input$fac2 == "none"){
inFile[, c(input$xcol2, input$ycol2)]}

else{
  inFile[, c(input$xcol2, input$ycol2, input$fac2)]}


})



lmvar <- reactive({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}


lm(selectedData2()[,2]~selectedData2()[,1], selectedData2())
})

rlmvar <- reactive({

inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}



rlm(selectedData2()[,2]~selectedData2()[,1], selectedData2())

})

lmc1 <- reactive({

summary(lmvar())$coefficients[1]
})

lmc2 <- reactive({

summary(lmvar())$coefficients[2]
})

lmrs <- reactive({

summary(lmvar())$r.squared
})

rlmc1 <- reactive({

summary(rlmvar())$coefficients[1]
})

rlmc2 <- reactive({

summary(rlmvar())$coefficients[2]
})




output$ggp <- renderPlot({
  
inFile <- datasetInput()
if (is.null(inFile))
{inFile = default_dataset}

a <- selectedData2()
#g<- qplot(selectedData2()[,1] , y=selectedData2()[,2] ) + geom_point() + theme_light() + labs(x=input$xcol2 , y=input$ycol2)
g<-ggplot(data=a, aes_string( input$xcol2,  input$ycol2))+geom_point()+ theme_light() + labs(x=input$xcol2 , y=input$ycol2)

if(input$fac2 != "none"){
a <- selectedData2()
a$fac = inFile[, input$fac2]
#g <- qplot(x=selectedData2()[,1] , y=selectedData2()[,2], f=selectedData2()[,3]) + facet_grid(.~ f)+ geom_point()  + theme_light() + labs(x=input$xcol2 , y=input$ycol2)
g<-ggplot(data=a, aes_string(input$xcol2,  input$ycol2, input$fac2))+geom_point()+facet_wrap(~ fac) + theme_light() + labs(x=input$xcol2 , y=input$ycol2)
  
  }



if(input$line == "linear")
{
  g  + stat_smooth(method = "lm")   }
  else if (input$line == "loose")
  {
  g  + stat_smooth(method="loess")   } 
  else 
  {
  g + geom_smooth(method = "lm" , 
  aes(colour="squares"), se=FALSE) + stat_smooth(method = "rlm" ,
  aes(colour="robust"), se=FALSE)  
  }
  
  
  
})
  
  
  
  output$coefs <- renderPrint(
  
  if(input$line == "linear")
  {
  cat("intercept:" , lmc1() ,  "scope:", lmc2() , "r^2:" ,lmrs()) }
  else if (input$line == "loose")
  {
  } 
  else 
  {
  cat("intercept:" , lmc1() ,  "scope:", lmc2() ) 
  
  }
  
  
  )
  
  output$coefsr <- renderPrint(
  
  if(input$line == "linear")
  {
  }
  else if (input$line == "loose")
  {
  } 
  else 
  {
  
  cat("intercept:" , rlmc1() ,  "scope:", rlmc2()) 
  }
  
  
  )
  
  ######################regresni###diagnostika######################################################
  
  
  observe({
    
    inFile <- datasetInput()
    if (is.null(inFile))
    {inFile = default_dataset}
    
    updateSelectInput(session, "xcol3",
                      choices = names(inFile)
    )
    updateSelectInput(session, "ycol3",
                      choices = names(inFile), selected=names(inFile)[[2]]
    )
    
    
  })
  
  
  selectedData3 <- reactive({
    
    inFile <- datasetInput()
    if (is.null(inFile))
    {inFile = default_dataset}
    
    
    
    inFile[, c(input$xcol3, input$ycol3)]
    
  })
  
  output$lrd <- renderPlot({
    
    inFile <- datasetInput()
    if (is.null(inFile))
    {inFile = default_dataset}
    
    g3<- qplot(selectedData3()[,1] , y=selectedData3()[,2] , data = inFile ) + geom_point() + theme_light() + labs(x=input$xcol3 , y=input$ycol3)
    g3 + geom_smooth(method="lm")
    
  })
  
  output$lrdd <- renderPlot({
    
    inFile <- datasetInput()
    if (is.null(inFile))
    {inFile = default_dataset}
    
    gd3 <- lm(selectedData3()[,2]~selectedData3()[,1], selectedData3())
    
    autoplot(gd3, which = 1:6, ncol = 3, label.size = 3) + theme_light()
    
  })
  
  
  ######################ggpairs###########################################korelacni###analyza#####
  
  observe({
  
  inFile <- datasetInput()
  if (is.null(inFile))
  {inFile = default_dataset}
  
  updateCheckboxGroupInput(session, "ggvars",
  choices = names(inFile), selected = names(inFile)[[1]], inline = TRUE
  )
  
  
  })
  
  
  
  output$plot2 <- renderPlot({
  
  inFile <- datasetInput()
  if (is.null(inFile))
  {inFile = default_dataset}
  
  ggpairs(inFile, columns=input$ggvars) + theme_light() 
  
  })
  
  
  ###################mClust###########################################
  
  observe({
  
  inFile <- datasetInput()
  if (is.null(inFile))
  {inFile = default_dataset}
  
  updateCheckboxGroupInput(session, "mclvars",
  choices = names(inFile), selected = names(inFile)[[1]], inline = TRUE
  )
  
  
  })
  
  mnoc <- reactive({
    
    inFile <- datasetInput()
    if (is.null(inFile))
    {inFile = default_dataset}
    
    
    input$noc
  })
  
  
  output$mcl <- renderPlot({
  
  inFile <- datasetInput()
  if (is.null(inFile))
  {inFile = default_dataset}
  
  #dataMclust <- Mclust(inFile, columns=input$mclvars)
  
  colsm <- input$mclvars
  colsdata<-inFile[ , colsm, drop = FALSE]
  dataMclust <- Mclust(colsdata, G=1:mnoc())
  
  if(input$sel == "BIC")
  {
  plot(dataMclust, what="BIC")
  }
  else if (input$sel == "classification")
  {
  plot(dataMclust, what="classification")
  } 
  else if (input$sel == "uncertainty")
  {
  plot(dataMclust, what="uncertainty")
  } 
  else 
  {
  plot(dataMclust, what="density")
  }
  
  
  
  
  
  
  
  })
  
  
  ##############TUTORIAL#############################################3
  
  observeEvent(input$news, {
  showModal(modalDialog(
  title = h4("News"),
  tags$p(tags$i(" xx.xx.2017 . NeuroEDA  deployed!  www.kbi.fbmi.cvut.cz/neuroeda")),
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  
  observeEvent(input$tut1, {
  showModal(modalDialog(
  title = h4("NeuroEDA . Interactive visualisation application. 
  Explore your data and use statistical models."),
  
  tags$p("Introduction"),
  tags$p("This application has begun as an inicitaive of CTU FBMI students. 
  We wanted to look at our data quickly and in a playful way, following 
  EDA principles.
  NeuroEDA can be used with any data in a data table form. Our research 
  concentrates on neurological patients, so besides common statistical methods
  we implement in this app advanced methods, that we use to analyse our data."),
  tags$p("In this tutorial you will learn how to work with NeuroEDA app, and some 
  basics about statistical methods used here, written in italic, like this:"),
  tags$br(),
  tags$p(tags$i("EDA")),
  tags$p(tags$i("Exploratory data analysis")),
  tags$p(tags$i("The first look at the data before deeper analysis. Understanding the new data, 
  determining if there are any problems with your dataset and looking at 
  connections among variables, using plots. ")),
  tags$p(tags$a(href="https://leanpub.com/artofdatascience","https://leanpub.com/artofdatascience")),
  tags$br(),
  tags$p("Uploading data"),
  tags$p("For exploring NeuroEDA, use default dataset mtcars (look at the meaning of each var, shown below). 
  For your own data, you can upload dataset in .csv or .tsv format. 
  If you've got different one, .csv can be easily exported using excel, MATLAB e.t.c.
  "),
  tags$p("Click the Browse... button and find your dataset. "),
  tags$p("When are your data uploaded correctly, look right at structure of the dataset.
  You can check number, names and types of your variables as well as number of observations.
  Below is displayed head  - first 6 rows of data. Looking at head is an easier 
  way to determine if everything is right with the data, and uploaded correctly. 
  Use options in a sidebar menu to correct commas, semicolons or quotes."),
  tags$p("Continue exploring your data on the other tabs. Look for the Tutorial button on each page. "),
  tags$br(),
  tags$p("mtcars info:"),
  tags$img(src = 'images/mtcars.png' , height = '180px', align='center'),
  
  
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  
  observeEvent(input$tut2, {
  showModal(modalDialog(
  title = h4("Summary"),
  tags$p(tags$i("5-number summary")),
  tags$br(),
  tags$p(tags$i("A set of descriptive statistics for single variables: ")),
  tags$p(tags$i("Minimum: the lowest value")),
  tags$p(tags$i("1st quartile: the middle number between minimum and median.
  Splits off the lowest 25% of data from the highest 75%")),
  tags$p(tags$i("Middle value: median, the middle number of a data sample. 
  Mean, typycally not contained in 5-number summary, shows the average value.")),
  tags$p(tags$i("3rd quartile: the middle number
  between maximum and median. Splits off the highest 25% of data from the lowest 75%")),
  tags$br(),
  tags$p("Here you can get some more information about each variable. For visualisation 
  of the 5-number summary, use Boxplot, found in Plot tab above. "),
  
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  observeEvent(input$tut3, {
  showModal(modalDialog(
  title = h4("Data table"),
  tags$p("Here you have access to all of your data in a table form. You can sort entries or search for a specific value."
  ),
  tags$br(),
  
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  observeEvent(input$tut4, {
  showModal(modalDialog(
  title = h4("Plots"),
  tags$p("Here you can look at your data using basic plots. In the sidebar menu you can choose type of plot, and afterwards pick a variable you are interested in. 
  Facet means dividing the observations according to some variable values - e.g., look at mtcars dataset. We are interested mpg - Miles/gallon variable and 
  we want to compare mpg values in cars of different cylindres count - cyl variable, which can acquire values 4, 6, or 8 cylinders. "),
  tags$img(src = 'images/bxpf.png' , height = '300px', align='center'),
  tags$p(tags$i("Boxplot")),
  tags$p(tags$i("A one-dimensional plot depicting the 5-number summary. If the data are normally distributed, the boxplot 
  is horizontally symetric. ")),
  tags$img(src = 'images/bxp.png' , height = '350px', align='center'),
  tags$p(tags$i("Histogram")),
  tags$p(tags$i("Another one-dimensional plot, showing distribution of an univariate data. X-axis shows range of the values, divided into bins - adjacent intervals. Y-axis
  shows the frequency(count) of each interval in the data. ")),
  tags$p(tags$i("Scatterplot")),
  tags$p(tags$i("Displays values of two variables. Color option can distinguish the third variable.")),
  tags$img(src = 'images/scp.png' , height = '400px', align='center'),
  tags$p(tags$i("Time-series plot")),
  tags$p(tags$i("Shows values of the chosen variable, while on X-axis are sorted indexes of rows, representing time. Color option can distinguish another variable values in the line")),
  tags$br(),
  
  
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  observeEvent(input$tut7, {
  showModal(modalDialog(
  title = h4("Cluster analysis"),
  tags$p("Multidimensional statistical methods grouping the observations into groups, while objects in the same group are more similar to each other than to objects in other groups. NeuroEDA
  app currently implements K-means algorithm and mclust algorithm."),
  tags$br(),
  tags$p(tags$i("K-means clustering")),
  tags$br(),
  tags$p(tags$i("This algorithm groups observations of multivariate data into k groups, while k is a stated number. Each cluster has a mean and each
  observation is assigned to the nearest mean. ")),
  tags$br(),
  tags$p(tags$i("K-means algorithm")),
  tags$br(),
  tags$p(tags$i("1 . k objects are randomly chosen as the initial means")),
  tags$img(src = 'images/km1.png' , height = '150px', align='center'),
  tags$br(),
  tags$p(tags$i("2 . every other object is assigned to the nearest mean ")),
  tags$img(src = 'images/km2.png' , height = '150px', align='center'),
  tags$br(),
  tags$p(tags$i("3 . among groups is calculated centroid which becomes the new mean")),
  tags$img(src = 'images/km3.png' , height = '150px', align='center'),
  tags$br(),
  tags$p(tags$i("4 . every object is again assigned to the nearest mean")),
  tags$img(src = 'images/km4.png' , height = '150px', align='center'),
  tags$br(),
  tags$p(tags$i("5 . steps 2 and 3 are repeated until there's no change between iterations (convergence reached)")),
  tags$br(),
  tags$p("Choose x and y variable of the data in the sidebar menu, and the number of clusters. You can reactively change the number. If you click or double-click on a point, you can view the observation values under the plot."),
  tags$br(),
  tags$p("For the mclust tutorial, click to mclust tab in the analysis menu."),
  tags$br(),
  
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  observeEvent(input$tut6, {
  showModal(modalDialog(
  title = h4("Regression"),
  tags$p(tags$i("Linear regression")),
  tags$p(tags$i("A mathematical method, finding the relationship between the dependend (y) variable and one or more indepentent - explanatory variables (X) by 
  fitting the line to the data represented in a cartesian coordinate system. The method assumes, that X values are correct and y values are under common error, 
  and calculates a line, using commonly the Least squares method (or other methods) to calculate the parametres of the line. NeuroEDA implements simple linear regression, 
  meaning there's only one explanatory variable.")),
  tags$br(),
  tags$p("Choose independend (x) and dependend (y) variable of the dataset. You can look at linear model (linear), 
  see the comparison of linear and robust line (robust) and non-parametric LOESS curve (loess). Below the non - facet graphs of the first two models you can 
  see parametres of each line. Choose facet variable to compare observations divided according to values of facet variable (see Facet in the Plots tutorial)."),
  tags$br(),
  tags$p(tags$i("Ordinary least squares method")),
  tags$p(tags$i("The regression line parametres are estimated by minimizing the sum of squared values of differences between the y values of observations and those predicted by a 
  linear function.")),
  tags$br(),
  tags$p(tags$i("Robust methods")),
  tags$p(tags$i("Methods to calculate the parametres in a linear model, that are less sensitive to outliers than least squares method.
  ")),
  tags$img(src = 'images/lr1.png' , height = '250px', align='center'),
  tags$br(),
  tags$p(tags$i("LOESS method")),
  tags$p(tags$i("Non-parametric regression. LOcal regrESSion method, using the Least squares method to localized subsets of the data, creating the curve fitting to the data. The method is used
  in situations, where classical methods do not perform well. More efficient in showing non-linear trends, than calculating
  high-degree polynomials.")),
  tags$img(src = 'images/lr2.png' , height = '250px', align='center'),
  tags$br(),
  tags$br(),
  
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  observeEvent(input$tut5, {
  showModal(modalDialog(
  title = h4("Correlation analysis (GGally::ggpairs)"),
  tags$p("Pairwise comparison of multivariate data. Choose variables you want to compare. Along a diagonal are density plots of numeric data, or a barplot for categorical data. 
Where the pairs meets, are scatterplots on the left side and the correlation coefficients on the right one. "),
  tags$br(),
  tags$p(tags$i("Correlation and Pearson correlation coefficient")),
  tags$p(tags$i("Any statistic relationship between two random variables is called correlation. In most cases it refers to Pearson correlation coefficient - the measure of the linear
 correlation between two variables, with value between -1 and 1.")),
  tags$br(),
  tags$p(tags$h4("correlation ⇏ causality", align="center")),
  tags$br(),
  
  tags$img(src = 'images/corr.png' , height = '250px', align='center'),
  tags$p(tags$i("Example of the correlation coefficients of various scatter diagrams. Author: Denis Boigelot")),
  tags$br(),
  tags$br(),
  
  
  footer=NULL,
  size = "l", easyClose = TRUE, fade = TRUE
  ))
  })
  
  observeEvent(input$tut8, {
    showModal(modalDialog(
      title = h4("mclust . Normal Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation"),
      tags$p("Mclust package provides models and methods to estimate number of clusters in the multivariate dataset. The algorithm uses 10 models to calculate 
defaultly 1 to 9 components (clusters) and Bayesan Information Criterion (BIC) to select final nubmer of clusters. "),
      
      tags$p("In NeuroEDA are 4 basic plots implemented: BIC plot, classification plot, uncertainty plot and density plot. 
in BIC plot you can see results of Bayesan criterion for each model and each calculated number of components. Case with highest BIC is selected and data are 
classified accordig to it, visualised on the classification plot. "),
      
      tags$p("Uncertainty plot provides view on the measure of the quality of the classification. Circles on classification and uncertainty 
plots correspond to the covariances of the components.  "),
      
      tags$p("Choose variables to classify and observe diagrams. If you assume more than 9 clusters in your data, or if you can see rising trend beyond 9 components in various
models in the BIC plot, you can select higher maximum number of components. "),
      tags$br(),
      tags$img(src = 'images/bic.png' , height = '280px', align='center'),
      tags$p(tags$i("BIC diagram example. The highest BIC is at number 4.")),
      tags$br(),
      tags$img(src = 'images/class.png' , height = '280px', align='center'),
      tags$p(tags$i("Classification diagram example.")),
      tags$br(),
      tags$br(),
      tags$p(tags$i("Table: models used to classificate data. mclust Version 4 for R: Normal Mixture Modeling for
Model-Based Clustering, Classification, and Density Estimation")),
      tags$img(src = 'images/tb.png' , height = '220px', align='center'),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$p(align="center", tags$a(href="http://www.stat.washington.edu/mclust/","http://www.stat.washington.edu/mclust/")),
      tags$br(),
      tags$br(),
      
      
      footer=NULL,
      size = "l", easyClose = TRUE, fade = TRUE
    ))
  })
  
  observeEvent(input$tutx, {
    showModal(modalDialog(
      title = h4("Summary"),
      tags$p(tags$i("5-number summary")),
      tags$br(),
      
      
      footer=NULL,
      size = "l", easyClose = TRUE, fade = TRUE
    ))
  })
  
  
  
  
  
  
  
}

