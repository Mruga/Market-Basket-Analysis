library("arules")
library(shiny)
library(visNetwork)
library(igraph)
library("arulesViz")
library(splitstackshape)
library(data.table)
train <- read.csv("train_data.csv")
train_2 <- data.frame(train$VisitNumber,train$DepartmentDescription)
train_2 <- train_2[!duplicated(train_2), ]
library(plyr)
temp_file <- ddply(train_2, .(train_2$train.VisitNumber), summarize, train.DepartmentDescription = toString(train.DepartmentDescription))
temp_file$`train_2$train.VisitNumber`<- NULL

Walmart_data <- read.transactions("Walmart_Featured_File.csv ",format = "basket", sep = ",")
server = function(input, output) {
  
  output$choose_columns <- renderUI({
    selectInput("cols", "Choose Department Products:", 
                choices  = colnames(Walmart_data),
                selected = colnames(Walmart_data)[1:5],multiple = TRUE)
  })
  
  ## Extracting and Defining arules
  rules <- reactive({
    tr <- as(Walmart_data, 'transactions')
    arAll <- apriori(tr, parameter=list(support=input$supp, confidence=input$conf))
    
    if(input$rhsv=='Subset' & input$lhsv!='Subset'){
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(Walmart_data, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=rhs %in% varsR)
      
    } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(Walmart_data, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL)
      
    } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(Walmart_data, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(Walmart_data, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)
      
    } else {
      ar <- arAll
    }
   
    print(ar)
  })
  
  # Rule length
  nR <- reactive({
    nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
  })
  
  ## Grouped Plot 
  output$groupedPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
  }, height=800, width=800)

  ## Graph Plot 
  output$graphPlot <- renderPlot({
    ar <- rules()
     plot(sort(ar, by=input$sort)[1:nR()], method='graph',control=list(type="items",cex=0.6))
    }, height=800, width=800)
  
  ## Scatter Plot 
  output$scatterPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
  }, height=800, width=800)

  ## Parallel Coordinates Plot
  output$paracoordPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='paracoord')
  }, height=800, width=800)

  ## Matrix Plot 
  output$matrixPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='matrix', control=list(reorder=T))
  }, height=800, width=800)

  ## Item Frequency Plot 
  output$itemFreqPlot <- renderPlot({
    trans <- as(Walmart_data[,input$cols], 'transactions')
    itemFrequencyPlot(trans,type="absolute",xlab="Department Products",names=TRUE,col=rainbow(4),cex.names=1.5)
  }, height=800, width=800)

  ## Print Rules
  output$rulesTable <- renderPrint({
    ar <- rules()
    inspect(sort(ar, by=input$sort))
  })

}