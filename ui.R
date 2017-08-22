
arulesApp <- function (supp, conf) {
shinyUI(pageWithSidebar(
  
  headerPanel("Walmart Trip Type Classification"),
  
  sidebarPanel(
    
    conditionalPanel(
      condition = "input.samp=='Sample'",
      numericInput("nrule", 'Number of Rules', 5), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab=='graph'",
      radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
    ),

    conditionalPanel(
       condition = "input.lhsv=='Subset'",
       uiOutput("choose_lhs"), br()
     ),
    
    conditionalPanel(
      condition = "input.rhsv=='Subset'", 
      uiOutput("choose_rhs"), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab=='grouped'",
      sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab %in%' c('grouped', 'graph', 'table',, 'scatter','itemFreq')", 
      radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
      uiOutput("choose_columns"), br(),
      sliderInput("supp", "Support:", min = 0, max = 1, value = supp ), br(),
      sliderInput("conf", "Confidence:", min = 0, max = 1, value = conf ), br(),
      selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
      numericInput("minL", "Min. items per set:", 2), br(), 
      numericInput("maxL", "Max. items per set::", 3), br()
     
    ),
    shinyjs::useShinyjs(),
    hidden(
      conditionalPanel(
        actionButton("fishButton", label = "Fish"),
        radioButtons('lhsv', label=h4("Fish:"), choices=c('All', 'Subset')), br(),
        radioButtons('rhsv', label=NULL, choices=c('All', 'Subset')), br()
        
      ))
  ),
  
  mainPanel(
    tabsetPanel(id='mytab',
                tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                tabPanel('Table', value='table', verbatimTextOutput("rulesTable"))
    )
  )
  
))
}
arulesApp(0.01,0.5)