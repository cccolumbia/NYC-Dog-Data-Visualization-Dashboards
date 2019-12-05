library(shiny)
library(ggplot2)
library(plotly)
#library(extrafont)
# Define UI ----

file <- "dog.licenses.date.csv"
Objective <- "LicenseIssuedYear"
Objective2 <- "LicenseIssuedMonth"
title1 <- "Which month and year had the most number of dog licenses issued?"
title2 <- paste("Dog licenses",sep="")
linecolor <- rgb(8,48,107,maxColorValue = 255)
fillcolor <- "Blues"
textcolor <- rgb(8,48,107,maxColorValue = 255)

ui <- fluidPage(
  titlePanel(title1),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput("Borough", h3("Boroughs"),
                         choices = list("New York City", "Bronx" , "Brooklyn" ,
                                        "Manhattan" ,"Queens" , "Staten Island"),
                         selected = "Bronx"),
      
      
      selectInput("Year", h3("Year"), 
                  choices = as.list(c(as.character(seq(2015,2016)))), selected = "2016")
      
      # radioButtons("Group", h3("By"),
      #              choices = list("Month","Weekday"),
      #              selected = "Month")
      
      
    ),
    
    mainPanel(
      plotlyOutput("barplot")
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  output$barplot <- renderPlotly({
    
    data1 <- read.csv(file)
    data1 <- data1[,c("Borough", Objective, Objective2)]
    data1 <- data1[data1[,Objective]==input$Year,]
    data2 <- data.frame()
    data <- data.frame()
    
    
    if ("New York City" %in% input$Borough){
      data2 <- data.frame(table(data1[,Objective2]))
      colnames(data2) <- c(Objective2,"Freq")
      data2$Borough <- "New York City"
    }
    
    data4 <- data1[data1$Borough %in% input$Borough,]
    
    if (nrow(data4) != 0){
      for (i in unique(data4$Borough)){
        data5 <- data4[data4$Borough == i,]
        data5 <- data.frame(table(data5[,Objective2]))
        colnames(data5) <- c(Objective2,"Freq")
        data5$Borough <- i
        data <- rbind(data,data5)
      }
    }
    
    data <- rbind(data,data2)
    
    # if(input$Group == "Month"){
    #   data[,input$Group] <- factor(data[,input$Group],levels=month.abb)
    # }
    # else{
    #   weekday.abb <- c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    #   data[,input$Group] <- factor(data[,input$Group],levels=weekday.abb)
    # }
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x=Objective2,y="Freq",fill="Borough"),
               stat="identity", 
               position=position_dodge(),
               color= linecolor,
               width = 0.5)+
      labs(title=paste(title2,"in",input$Year),x=Objective2,y="Number of Dog Licences Issued")+
      theme_minimal()+
      scale_fill_brewer(palette=fillcolor)+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold")
            # ,
            # axis.text.x = element_blank()
      )
    #theme_xkcd
    ggplotly(p)
    
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)