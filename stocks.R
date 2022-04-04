#import libraries
library(quantmod)
library(BatchGetSymbols)
library(randomForest)
library(TTR)
library(shiny)

#import the first 20 stocks
sp500 <- GetSP500Stocks()
tickers <- head(sp500$Tickers,20)

#set date
first.date <- Sys.Date() - 7500
last.date <- Sys.Date()

#pull data from tickers
datalist <- list()
for (i in 1:20){
  datalist[[i]] <- BatchGetSymbols(tickers = tickers[i],
                                   first.date = first.date,
                                   last.date = last.date,
                                   freq.data = "daily",
                                   thresh.bad.data = 0.1,
                                   do.complete.data = TRUE,
                                   do.cache = TRUE)
}

#set up list for values
tabla <- list()
for (i in 1:20){
  tabla[[i]] <- datalist[[i]]$df.tickers[c(1,2,8)]
  tabla[[i]]$MovingAverage10 <- 0
  tabla[[i]]$MovingAverage50 <- 0
  tabla[[i]]$MovingAverage200 <- 0
}

#Moving averages
i = 1
while(i <= 20){
  # Moving Average - 10
  for (j in 11:nrow(tabla[[i]])){
    tabla[[i]]$MovingAverage10[j] <- mean(tabla[[i]]$price.adjusted[(j-10):(j-1)])
  }
  # Moving Average - 50
  for (j in 51: nrow(tabla[[i]])) {
    tabla[[i]]$MovingAverage50[j] <- mean(tabla[[i]]$price.adjusted[(j-50):(j-1)])
  }
  # Moving Average - 200
  for (j in 201:nrow(tabla[[i]])){
    tabla[[i]]$MovingAverage200[j] <- mean(tabla[[i]]$price.adjusted[(j-200):(j-1)])
  }
  i <- i+1
}


#additional technical indicator
#Bollinger Bands
bollinger <- list()
for (i in 1:20){
  bollinger[[i]] <- BBands(tabla[[i]]$price.adjusted, n=20,sd=2)
}

#
ind <- list() 
training <- list()
test <- list()
for (i in 1:20){
  #es esta
  tabla[[i]] <- tabla[[i]][c(201:nrow(tabla[[i]])),]
  ind[[i]] <- sample(1:nrow(tabla[[i]]), 0.7*nrow(tabla[[i]]))
  training[[i]] <- tabla[[i]][ind[[i]],]
  test[[i]] <- tabla[[i]][!(c(1:nrow(tabla[[i]])) %in% ind[[i]]),]
}

#random forest
random_forest <- list()
for (i in 1:20){
  random_forest[[i]] <- randomForest(price.adjusted ~ MovingAverage10 + MovingAverage50 + MovingAverage200, data=test[[i]], ntree=100, mtry=2, importance=TRUE)
}

#predictions
predictions <- list() 
for (i in 1:20){
  predictions[[i]] <- predict(random_forest[[i]],tabla[[i]])
  tabla[[i]] <- cbind(tabla[[i]],predictions[[i]])
  colnames(tabla[[i]])[ncol(tabla[[i]])] <- "Prediction"
}

#R shiny app

# Define UI f
ui <- fluidPage(

  # Application title
  titlePanel("S&P 500 stocks"),

  # Sidebar with a slider input and select input
  sidebarLayout(
    sidebarPanel(

      selectInput("variablechoice", "Stock", choices=tickers,
                  selected=head(tickers,1)),

      sliderInput("date",
                  "Date:",
                  min = first.date, max = last.date, step = 1, value = c(first.date,last.date))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("myPlot")
    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  #ts function
  output$myPlot <- renderPlot({
    tickers.timeseries <- tabla[[which(tickers == input$variablechoice)]][,c(2,3,7)] 
    tickers.timeseries <- tickers.timeseries[tickers.timeseries$ref.date >= input$date[1] & tickers.timeseries$ref.date <= input$date[2],]
    MAE <- mean(abs(tickers.timeseries$Prediction - tickers.timeseries$price.adjusted))
    mean.error <- mean(tickers.timeseries$Prediction - tickers.timeseries$price.adjusted)
    
    plot(tickers.timeseries[c(1,2)],xlim = c(input$date[1],input$date[2]),type = 'l',col = "black",xlab=paste("Time :",input$date[1],"to",
      input$date[2]),ylab="Adjusted stock price",main=paste("Actual vs modeled",input$variablechoice,"Stock Prices"))
    lines(tickers.timeseries[c(1,3)],xlim = c(input$date[1],input$date[2]),type="l",col="red")
    
    legend("topleft", legend = "Stock Price", col = "black", pch = c(1,3), text.col = "black", horiz = F, inset = c(.05,.05), bty = "n")
    legend("topleft", legend = "Modeled Stock Price", col = "red", pch = c(1,3), text.col = "black", horiz = F, inset = c(.05,.1), bty = "n")
    legend("topleft", legend = paste("MAE:",round(MAE,digits=2) ,"Average error:",round(mean.error,digits=2)), col = "red", text.col = "black", horiz = F, inset = c(.05,.15), bty = "n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
