
#--------------------------------- Load packages ----------------------------------------

library(shiny)
library(quantmod)
library(shinythemes)
library(plotly)
library(PortfolioAnalytics)
library(quantmod)
library(foreach)
library(DEoptim)
library(iterators)
library(fGarch)
library(Rglpk)
library(quadprog)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
library(pso)
library(GenSA)
library(corpcor)
library(testthat)
library(nloptr)
library(MASS)
library(robustbase)
library(tidyquant)
library(timetk)
library(dplyr)
require(zoo)
require(tseries)
library(forecast)
library(xts)

#----------------------------------------------------------------------------------------

#---------------------------------- User interface --------------------------------------

ui <- fluidPage(theme = shinytheme("yeti"),
                titlePanel("R-chitects- Portfolio Optimization"),
# Tab Set Layout
                tabsetPanel(
                  type = "tabs",
# 'Explore' Tab
                  tabPanel("Explore", sidebarLayout(
                    sidebarPanel(
                      helpText("Select a stock to examine.

                                Information will be collected from Yahoo finance."),
                      selectInput("symb", "Stock", choices=c(
                        "MSFT","AAPL","AMZN","FB","GOOG","JPM","GOOGL","JNJ","PG",
                        "V","XOM","T","HD","VZ","MA","BAC","DIS","CVX","INTC","MRK",
                        "KO","CSCO","BA","UNH","CMCSA","PFE","WFC","PEP","WMT","MCD",
                        "C","ABT","MDT","ADBE","CRM","IBM","COST","PYPL",
                        "ACN","HON","TXN","ORCL","AMGN","PM",
                        "UNP","NKE","NFLX","TMO","NEE"
                      ), multiple = F, selected = c("AMZN"),
                      selectize = TRUE),
                      dateRangeInput("dates",
                                     "Date range",
                                     start = "2013-01-01",
                                     end = as.character(Sys.Date())),
                      actionButton(inputId = "go",
                                   label = "Update", icon("refresh")),
                      br(),
                      br(),
                      radioButtons("techstat", "Technical Statistics:",
                                   c("Bollinger Bands" = "addBB",
                                     "Volume" = "addVol",
                                     "WW Directional Movement Indicator" = "addADX",
                                     "Chaiken Money Flow" = "addCM"))
                    ),
                    mainPanel(plotOutput("plot"))
                  )),
# 'Forecast' Tab
                  tabPanel("Forecast", sidebarLayout(
                    sidebarPanel(
                      helpText("ARIMA Forecast Parameters"),
                      selectInput(inputId = "frcstStock", label="Stock Options:", choices=c(
                        "MSFT","AAPL","AMZN","FB","GOOG","JPM","GOOGL","JNJ","PG",
                        "V","XOM","T","HD","VZ","MA","BAC","DIS","CVX","INTC","MRK",
                        "KO","CSCO","BA","UNH","CMCSA","PFE","WFC","PEP","WMT","MCD",
                        "C","ABT","MDT","ADBE","CRM","IBM","COST","PYPL",
                        "ACN","HON","TXN","ORCL","AMGN","PM",
                        "UNP","NKE","NFLX","TMO","NEE"
                      ), multiple = F, selected = c("AMZN"),
                      selectize = TRUE, width = NULL, size = NULL),
                      textInput("days", "Forecast Period (in days)","5"),
                      textInput("ci", "Confidence Interval","55"),
                      br(),
                      actionButton(inputId = "forecast",
                                   label = "Forecast", icon("chart-line"))
                    ),
                    mainPanel(
                      plotlyOutput("frcstplot1"),
                      br(),
                      plotlyOutput("frcstplot3"),
                      br(),
                      plotlyOutput("frcstplot2")
                      
                    )
                  )),
# 'Optimize' Tab
                  tabPanel("Optimize", sidebarLayout(
                    sidebarPanel(
                      helpText("Select portfolio optimization parameters."),
                      selectInput(inputId = "stockops", label="Stock Options:", choices=c(
                        "MSFT","AAPL","AMZN","FB","GOOG","JPM","GOOGL","JNJ","PG",
                        "V","XOM","T","HD","VZ","MA","BAC","DIS","CVX","INTC","MRK",
                        "KO","CSCO","BA","UNH","CMCSA","PFE","WFC","PEP","WMT","MCD",
                        "C","ABT","MDT","ADBE","CRM","IBM","COST","PYPL",
                        "ACN","HON","TXN","ORCL","AMGN","PM",
                        "UNP","NKE","NFLX","TMO","NEE"
                      ), multiple = TRUE, selected = c("AMZN","AAPL","PG","NKE","JPM","JNJ","NFLX",
                                                       "WMT","ADBE"),
                      selectize = TRUE, width = NULL, size = NULL),
                      textInput("amount", "Investment Amount($)","1000"),
                      radioButtons("optchoice", "Optimization Options:",
                                   c("ROI" = "roi", "Min Risk" = "minrisk", "Box ROI" = "boxroi",
                                     "Randomized Portfolios" = "rp")),
# Condition Panels to control visibility based on radio button selection
                      conditionalPanel(
                        condition = "input.optchoice == 'roi'",
                        br(),
                        helpText("Add Constraints (optional):"),
                        textInput(inputId="sumwts", label="Sum of Weights", value = "1")
                      ),
                      conditionalPanel(
                        condition = "input.optchoice == 'boxroi'",
                        br(),
                        helpText("Add Constraints (optional):"),
                        textInput(inputId="minbox", label="Minimum Boxes", value = "0"),
                        textInput(inputId="maxbox", label="Maximum Boxes", value = "0.3")
                      ),
                      conditionalPanel(
                        condition = "input.optchoice == 'minrisk'",
                        br(),
                        helpText("Add Constraints (optional):"),
                        textInput(inputId="minwts", label="Minimum Investment ($) per stock", value = "5")
                      ),
                      actionButton(inputId = "update",
                                   label = "Update", icon("refresh"))
                    ),
                    mainPanel(
                      headerPanel("Optimization Statistics"),
                      br(),
# Condition Panels to control visibility based on radio button selection                      
                      conditionalPanel(
                        condition = "input.optchoice != 'minrisk'",
                        textOutput("optstats")
                      ),
                      conditionalPanel(
                        condition = "input.optchoice != 'roi'",
                        textOutput("optstats2")
                      ),
                      br(),
                      plotlyOutput("pie"),
                      br(),
                      plotlyOutput("optplot")
                    )
                  ))
                )
)

#----------------------------------------------------------------------------------------


#------------------------------------ Server logic --------------------------------------

server <- function(input, output, session) {
  
################ Explore Tab logic ############################
  stockData <- eventReactive(input$update, {
    importData <- lapply(input$stockops, function(symb)
      get.hist.quote(instrument= symb,
                     start = "2016-01-01", 
                     quote="AdjClose", provider = "yahoo",
                     retclass="zoo"))
    
    importData <- do.call(merge, importData)
    names(importData) <- input$stockops
    importData
  })
  
  dataInput <- eventReactive(input$go, {
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    chartSeries(dataInput(), theme = chartTheme("white"),
                type = "line", TA = NULL, name = input$symb)
    stat <- switch(input$techstat,
                   addBB = addBBands,
                   addVol = addVo,
                   addADX = addADX,
                   addCM = addCMF)
    stat()
  })

############# Forecast Tab logic  ########################
  forecastData <- eventReactive(input$forecast, {
    stock_close <- lapply(input$frcstStock, function(symb)
      get.hist.quote(instrument= symb,
                     start = "2016-01-01", 
                     quote="Close", provider = "yahoo",
                     retclass="zoo"))
      stock_close
  })
  
  output$frcstplot1 <- renderPlotly({
    close_data <- as.data.frame(forecastData())
    close_data$Dates <- rownames(close_data)
    rownames(close_data) <- NULL
    p <- close_data %>%
      plot_ly(
        color = ~Close,
        x = ~Dates,
        y = ~Close, yaxis = list(type = "log"),
        mode = 'markers+lines'
      ) %>%
      layout(showlegend = F, title = "Stock Close Values")
  })
  
  output$frcstplot2 <-renderPlotly({
    stock <- as.data.frame(forecastData())
    stock <- as.xts(stock, .RECLASS=FALSE)
    stock = diff(log(stock),lag=1) 
    stock = stock[!is.na(stock)] #Removing missing values
    breakpoint = floor(nrow(xts(stock))*(0.85))
    b = breakpoint
    stock_train = stock[1:b,]
    stock_test = stock[(b+1):nrow(stock), ]
    fit = arima(stock_train, order = c(2, 0, 2),include.mean=FALSE)
    arima.forecast = forecast(fit, h = as.numeric(input$days),level=as.numeric(input$ci))
    
    d <- as.data.frame(arima.forecast)
    rownames(d)
    d[,1:3] <- exp(d[,1:3])
    d$Day <- as.numeric(1:nrow(d))
    rownames(d) <- NULL
    colnames(d) <- c("Forecast","Lo","Hi", "Day")
    p <- ggplot() +
      geom_line(data = d, aes(x = Day, y = Forecast , group = 1), color = "blue") +
      geom_line(data = d, aes(x = Day, y = Lo, group = 1), color = "red") +
      geom_line(data = d, aes(x = Day, y = Hi, group = 1), color = "Green") +
      xlab('Time (in days)') +
      ylab('Returns of stock')
    ggplotly(p)
  })
  
  output$frcstplot3 <- renderPlotly({
    stock <- as.data.frame(forecastData())
    stock <- as.xts(stock, .RECLASS=FALSE)
    stock = diff(log(stock),lag=1) 
    stock = stock[!is.na(stock)] #Removing missing values
    breakpoint = floor(nrow(xts(stock))*(0.85))
    b = breakpoint
    stock_train = stock[1:b,]
    stock_test = stock[(b+1):nrow(stock), ]
    fit = arima(stock_train, order = c(2, 0, 2),include.mean=FALSE)
    arima.forecast = forecast(fit, h = as.numeric(input$days),level=as.numeric(input$ci))
    p <- autoplot(arima.forecast, main = "ARIMA Forecast", include = 100, conf.int = TRUE,
                  conf.int.colour = "#0000FF", conf.int.linetype = "none",
                  conf.int.fill = "#000000", conf.int.alpha = 0.5,
                  ylab = "Log return of stock", xlab = "Time (in mins)")
    ggplotly(p)
  })

######### Optimize Tab logic #################  
  optData <- eventReactive({input$update 
    input$optchoice}, {
    funds <- input$stockops
    port <- portfolio.spec(assets = funds)
    port <- add.constraint(portfolio = port, type = "full_investment")
    port <- add.constraint(portfolio = port, type = "long_only")
    port <- add.objective(portfolio = port,
                          type = "return",
                          name = "mean")
    port <- add.objective(portfolio = port,
                          type = "risk",
                          name = "StdDev")
    
    choice <- switch(input$optchoice,
                     rp = "rp",
                     roi = "roi",
                     boxroi = "boxroi",
                     minrisk = "minrisk")
    
    if (choice == "rp") {
      optModel <- optimize.portfolio(
        R = stockData(),
        portfolio = port,
        optimize_method = "random",
        search_size = 2000,
        trace = TRUE
      )
    } else if (choice == "roi") {
      # Maximizing return can be formulated as a linear programming problem and
      # solved very quickly using optimize_method="ROI". We are using long_only
      # constraints so it is expected that allocation is to the portfolio with the
      # highest mean return.
      port <- add.constraint(portfolio = port, type="weight_sum", min_sum=0, 
                             max_sum=as.numeric(input$sumwts))
      optModel <-
        optimize.portfolio(
          R = stockData(),
          portfolio = port,
          optimize_method = "ROI",
          trace = TRUE
        )
    } else if (choice == "boxroi") {
      # It is more practical to impose box constraints on the weights of assets.
      # Update the second constraint element with box constraints
      port1 <- add.constraint(
        portfolio = port,
        type = "box",
        min = as.numeric(input$minbox),
        max = as.numeric(input$maxbox),
        indexnum = 2
      )
      optModel <-
        optimize.portfolio(
          R = stockData(),
          portfolio = port1,
          optimize_method = "ROI",
          trace = TRUE
        )
    } else if (choice == "minrisk") {
      x <- as.numeric(input$minwts)/as.numeric(input$amount)
      init.portf <- portfolio.spec(assets = funds)
      init.portf <-
        add.constraint(portfolio = init.portf, type = "full_investment")
      init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
      init.portf <- add.constraint(portfolio = init.portf, type="weight_sum", min_sum=x, max_sum=1)
      init.portf <-
        add.objective(portfolio = init.portf,
                      type = "risk",
                      name = "StdDev")
      optModel <-
        optimize.portfolio(
          R = stockData(),
          portfolio = init.portf,
          optimize_method = "ROI",
          trace = TRUE
        )
    }
    optModel
  })
  
  output$optstats <- renderText({
    if (!is.na(extractObjectiveMeasures(optData())['mean'])) {
      mean = round(as.numeric(extractObjectiveMeasures(optData())['mean']),2)
      paste("Expected Returns:", mean) 
    }
  })
  
  output$optstats2 <- renderText({
    if (!is.na(extractObjectiveMeasures(optData())['StdDev'])) {
      sd = round(as.numeric(extractObjectiveMeasures(optData())['StdDev']), 2)
      paste("Volatility:", sd)
    }
  })
  
  output$pie <- renderPlotly({
    wts <- as.data.frame(extractWeights(optData()))
    wts$names <- rownames(wts)
    rownames(wts) <- NULL
    names(wts) <- c("Weights","Names")
    wts <- wts %>% filter(Weights > 0)
    wts$Weights <- as.numeric(wts$Weights)
    wts$Amt <- wts$Weights*as.numeric(input$amount)
    t <- list(color='white')
    p <- wts %>%
      plot_ly(labels = ~Names, values = ~Amt) %>%
      add_pie(hole = 0.5) %>%
      layout(showlegend = T, title = "Portfolio Breakup Post-optimization",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$optplot <- renderPlotly({
      wts <- as.data.frame(extractWeights(optData()))
      wts$names <- rownames(wts)
      rownames(wts) <- NULL
      names(wts) <- c("Weights", "Names")
      p <- wts %>%
        plot_ly(
          x = ~ Names,
          y = ~ Weights,
          type='scatter',
          marker = list(
            size = 10,
            color = ~ Weights
          ),
          mode = "markers"
        ) %>%
        layout(title = "Investment Weights for Each Stock")
  })
}

#----------------------------------------------------------------------------------------

#--------------------------------------- Run the app ------------------------------------

shinyApp(ui, server)

#----------------------------------------------------------------------------------------