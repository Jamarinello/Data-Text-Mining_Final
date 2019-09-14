# SET WORKING DIRECTORY
setwd("E:/Documents Storage/Text and Data Mining/Final Project")


# LOAD LIBRARIES
library(dplyr)
library(tidyverse)
library(tm)
library(wordcloud)
library(rtweet)
library(ggplot2)
library(ggmap)
library(shiny)
library(caret)

mcuData <- read.csv("MCU.csv")

mcuData <- mcuData[-1,]
names(mcuData) <- c("Release.Date", "Title", "Production.Budget", "Opening.Earnings", "Domestic.BoxOffice", "Foreign.BoxOffice")

mcuData$Total.Earnings <- ((mcuData$Domestic.BoxOffice + mcuData$Foreign.BoxOffice) - mcuData$Production.Budget)
mcuData$Profitability <- (((mcuData$Domestic.BoxOffice + mcuData$Foreign.BoxOffice) - mcuData$Production.Budget) / mcuData$Production.Budget)

str(mcuData)
mcuData$Title <- as.character(mcuData$Title)
mcuData$Production.Budget <- as.numeric(mcuData$Production.Budget)
mcuData$Opening.Earnings <- as.numeric(mcuData$Opening.Earnings)
mcuData$Domestic.BoxOffice <- as.numeric(mcuData$Domestic.BoxOffice)


ui <- fluidPage(
  headerPanel("Marvel Cinematic Universe"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x", label = "X-axis", choices = c("Release.Date", "Title", "Production.Budget", "Opening.Earnings", "Domestic.BoxOffice", "Foreign.BoxOffice", "Total.Earnings", "Profitability"), selected = "Title"),
      selectInput(inputId = "y", label = "Y-axis", choices = c("Release.Date", "Title", "Production.Budget", "Opening.Earnings", "Domestic.BoxOffice", "Foreign.BoxOffice", "Total.Earnings", "Profitability"), selected = "Domestic.BoxOffice"),
      selectInput(inputId = "size", label = "Size", choices = c("Production.Budget", "Opening.Earnings", "Domestic.BoxOffice", "Foreign.BoxOffice", "Total.Earnings", "Profitability"), selected = "Opening.Earnings"),
      selectInput(inputId = "col", label = "Color", choices = c("blue", "red", "black", "white", "green", "yellow", "purple", "pink"), selected = "black"),
      selectInput(inputId = "fill", label = "Fill", choices = c("Release.Date", "Title", "Production.Budget", "Opening.Earnings", "Domestic.BoxOffice", "Foreign.BoxOffice", "Total.Earnings", "Profitability"), selected = "Foreign.BoxOffice")
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterPlot")
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    ggplot(mcuData, aes(x = mcuData[,input$x], y = mcuData[,input$y])) +
      geom_point(aes(fill = mcuData[,input$fill], size = mcuData[,input$size]), col = input$col, shape = 21, alpha = 0.8, stroke = 1.125) +
      scale_size(range = c(4,15)) +
      theme(axis.text.x = element_text(angle = 90, size = 12),
            axis.text.y = element_text(angle = 10, hjust = -0.125, vjust = 1.5, size = 12),
            axis.title = element_text(size = 24)) +
      labs(x = input$x,
           y = input$y,
           fill = input$fill,
           size = input$size)
  }, height = 900, width = 1500)
  
}

shinyApp(ui = ui, server = server)

# Predictive Model
set.seed(200)
Train_Test_Data <- mcuData[-c(1:3), -c(1:2)]
predData <- mcuData[c(1:3),]
predData$Production.Budget <- c(130000000, 200000000, 170000000)

str(Train_Test_Data)

set <- createDataPartition(Train_Test_Data$Opening.Earnings, p= 2/3, list=F)
train <- Train_Test_Data[set,]
test <- Train_Test_Data[-set,]

LM.model <- lm(Opening.Earnings ~ Production.Budget, data = train)
LM.model

pred <- predict(LM.model, test)
error <- abs(test$Opening.Earnings - pred)
RMSE <- sqrt(mean(error^2))

LM.plot <- ggplot(test, aes(x = Production.Budget, y = pred, size = error, color = error)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  ggtitle("LM Model")
LM.plot

predData$Opening.Earnings <- predict(LM.model,predData)
RMSE

save.image(file = "Predictive_Model_Shiny_App.RData")
