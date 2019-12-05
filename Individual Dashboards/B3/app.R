library(shiny)
library(ggplot2)
library(plotly)
#library(extrafont)
# Define UI ----

file <- "dog.licenses.gender.csv"
Objective <- "Gender"
title1 <- "Are there more male dogs born in certain months than female dogs?"
title2 <- paste("Dog born within",sep="")
linecolor <- rgb(8,48,107,maxColorValue = 255)
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
                  choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
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
    data <- data[,c("Borough","BirthYear","BirthMonth","Gender")]
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
    
    data1 <- data[data[,Objective]=="Female",]
    data1 <- data.frame(table(data1[,"BirthMonth"]))
    colnames(data1) <- c("Month","Freq")
    data1[,Objective] <- "Female"
    data2 <- data[data[,Objective]=="Male",]
    data2 <- data.frame(table(data2[,"BirthMonth"]))
    colnames(data2) <- c("Month","Freq")
    data2[,Objective] <- "Male"
    
    data <- rbind(data1,data2)
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x="Month",y="Freq",fill=Objective),
               stat="identity", 
               #position=position_dodge(),
               color= linecolor,
               width = 0.5)+
      labs(title=paste(title2,input$Borough,"in",input$Year),x="Month",y="Number of Dog born")+
      theme_minimal()+
      scale_fill_brewer(palette=fillcolor)+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold")
            # ,
            # axis.text.x = element_blank()
      )
    
    ggplotly(p)
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)