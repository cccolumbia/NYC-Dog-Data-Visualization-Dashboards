library(shiny)
library(ggplot2)
library(plotly)
library(wordcloud)
library(tm)
#library(extrafont)
# Define UI ----

file <- "dog.name.year.csv"
Objective <- "Name"
title1 <- "Dog Name Word Cloud"
title2 <- paste("Dog ",Objective,"s within",sep="")
linecolor <- rgb(8,48,107,maxColorValue = 255)
fillcolor <- rgb(158,202,225,maxColorValue = 255)
textcolor <- rgb(8,48,107,maxColorValue = 255)

ui <- fluidPage(
  titlePanel(title1),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("Borough", h3("Boroughs"),
                   choices = list("New York City", "Bronx" , "Brooklyn" ,
                                  "Manhattan" ,"Queens" , "Staten Island" ),
                   selected = "Bronx"),
      selectInput("Year", h3("Year"), 
                  choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
    ),
    
    mainPanel(
      plotOutput("barplot")
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$barplot <- renderPlot({
    
    data <- read.csv(file)
    if (input$Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$Borough,]
    }
    
    if (input$Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$Year,]
    }
    
    data <- data.frame(table(data[,Objective]))
    data <- data[order(data[,2],decreasing = T),]
    data <- data[1:30,]
    colnames(data)[1] <- Objective
    
    
    wordcloud_rep(data[,Objective],data[,2]/100, scale=c(4,0.2),
                  min.freq = 50,max.words=20,
                  colors=brewer.pal(8, "RdBu"))
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)