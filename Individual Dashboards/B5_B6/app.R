library(shiny)
library(ggplot2)
library(plotly)
#library(extrafont)
# Define UI ----

file <- "dog.licenses.date2.csv"
Objective <- "AnimalBirthMonth"
Objective2 <- "LicenseIssuedDate"
title1 <- "Does the license issue date correspond with the dog’s birth month?"
title2 <- paste("Dog born/license issued date within",sep="")
linecolor <- "Dark2"
fillcolor <- "Blues"
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
                  choices = as.list(c("All years",as.character(seq(2014,2016)))), selected = "2016")
    ),
    
    mainPanel(
      plotlyOutput("barplot")
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  output$barplot <- renderPlotly({
    
    data <- read.csv(file)
    data <- data[,c("Borough","BirthYear",Objective,Objective2)]
    #data <- as.character(data)
    
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
    
    data[,Objective] <- as.Date(data[,Objective],"%Y-%m-%d")
    data[,Objective2] <- as.Date(data[,Objective2],"%Y-%m-%d")
    data <- data[order(data[,Objective],decreasing = F),]
    data$index <- 1:nrow(data)
    
    data1 <- data[,c("index",Objective)]
    colnames(data1) <- c("index","Date")
    data1$Group <- Objective
    
    data2 <- data[,c("index",Objective2)]
    colnames(data2) <- c("index","Date")
    data2$Group <- Objective2
    
    data <- rbind(data1,data2)
    
    p <- ggplot(data=data)+
      
      geom_line(mapping = aes_string(x="index",y="Date",color="Group"),
               #stat="identity", 
               #position=position_dodge(),
               #color= linecolor[1],
               lwd = 0.5)+
      
      labs(title=paste(title2,input$Borough,"in",input$Year),x="Dog",y="Date")+
      theme_minimal()+
      scale_color_brewer(palette=linecolor)+
      theme(plot.title = element_text(color=textcolor, 
                                      size=10, 
                                      face="bold")
            # ,
            # axis.text.x = element_blank()
      )
    
    ggplotly(p)
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)