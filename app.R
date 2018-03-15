# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
shadata <- read.csv("~/shadata.csv")
data2<- shadata %>% group_by(Year) %>% summarise(value = sum(sales))
data4<- shadata %>% group_by(WEEK,Year) %>% summarise(value = sum(sales))
header <- dashboardHeader(title = "Dominicks Shampoos data")  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Sales per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("salesbyYear", height = "300px")
  )
  
  ,box(
    title = "Sales per Week"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("salesbyWeek", height = "300px")
  ) 
  
)
body <- dashboardBody(frow1, frow2)
ui <- dashboardPage(title = 'Dominicks Shampoos category', header, sidebar, body, skin='red')

server <- function(input, output) { 
  total.sales <- sum(shadata$sales)
  sales.UPC <- shadata %>% group_by(UPC) %>% summarise(value = sum(sales)) %>% filter(value==max(value))
  prof.prod <- shadata %>% group_by(Year) %>% summarise(value = sum(sales)) %>% filter(value==max(value))
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.UPC$value, format="d", big.mark=',')
      ,paste('Top UPC:',sales.UPC$UPC)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(total.sales, format="d", big.mark=',')
      ,'Total sales'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Best Year:',prof.prod$Year)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })
  output$salesbyYear<- renderPlot({
    
    ggplot(data = data2, aes(x=Year, y=value, fill=factor(Year))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("sales (in Dollars)") + 
      xlab("Year") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Sales per Year") + labs(fill = "Year")+geom_text(aes(label=value))
   
  
  })
  output$salesbyWeek <- renderPlot({
    ggplot(data = data4, aes(x=WEEK, y=value, fill=factor(Year))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("sales (in Dollars)") + 
      xlab("WEEK") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Sales per Week and Year") + labs(fill = "Year")
  })
}

shinyApp(ui, server)

