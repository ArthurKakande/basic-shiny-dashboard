library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

Salesdetails <- read.csv("sales_data_sample.csv", stringsAsFactors = F, header = T)

################################################################################
ui <- shinyUI(dashboardPage(
  title = 'This is my page title',
  #Dashboard header
  header <- dashboardHeader(title = "Sales Dashboard"),
  
  
  #sidebar content
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
      menuItem("Detailed", tabName = "Detailed", icon = icon("send", lib = 'glyphicon'))
    )
  ),
  
  
  #body content
  body <- dashboardBody(tabItems(
    tabItem(tabName = "Overview", h2("Sales overview"),
            
            #contents section
            frow1 <- fluidRow(
              valueBoxOutput("Value1"),
              valueBoxOutput("Value2"),
              valueBoxOutput("Value3")
            ),
            
            frow2 <- fluidRow(
              
              box(
                title = "sales per product", status = "primary", solidHeader = T, 
                collapsible = T, plotOutput("salesbyProd", height = "300px")
              ),
              
              box(
                title = "Sales per orderstatus", status = "primary", solidHeader = T,
                collapsible = T, plotOutput("salesbyorderstatus", height = "300px")
              )
              
            )
    ),
    tabItem(tabName = "Detailed",
            h2("Sales Detailed Analysis"),
            box(
              title = "inputs", status = "warning","Box content here", br(),"More box content",
              selectInput(inputId = "var1", label = "select the x variable", 
                          choices = c("Salesdetails.QUANTITY"=1,"Salesdetails.PRICEEACH"=2,"Salesdetails.SALES"=3),selected = 1),
              selectInput(inputId = "var2",label = "select the y variable",
                          choices = c("Salesdetails.QUANTITY"=1,"Salesdetails.PRICEEACH"=2,"Salesdetails.SALES"=3),selected = 1)
            )
    )
  )
  ),
  skin='purple'
)
)


###############################################################################
server <- function(input,output){
  #value box data
  total.sale <-sum(Salesdetails$SALES)
  sales.country <- Salesdetails %>% group_by(COUNTRY) %>% summarise(value = sum(SALES))%>% filter(value==max(value))
  sales.prod <- Salesdetails %>% group_by(PRODUCTLINE) %>% summarise(value = sum(SALES)) %>% filter(value==max(value))
  
  #Valuebox content
  output$Value1 <- renderValueBox({
    valueBox(
      formatC(sales.country$value, format = "d", big.mark = ','),
      paste('Top Country:',sales.country$COUNTRY),
      icon = icon("stats",lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$Value2 <- renderValueBox({
    valueBox(
      formatC(total.sale, format = "d", big.mark = ','), 'Total Sales', 
      icon = icon("usd",lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$Value3 <- renderValueBox({
    valueBox(
      formatC(sales.prod$value, format = "d", big.mark = ','),
      paste('Top Product:',sales.prod$PRODUCTLINE),
      icon = icon("menu-hamburger",lib = "glyphicon"),
      color = "green"
    )
  })
  
  #Creating the plot content
  output$salesbyProd <- renderPlot({
    ggplot(data = Salesdetails,
           aes(x=PRODUCTLINE,y=SALES, fill=factor(YEAR_ID)))+geom_bar(position = "dodge", stat="identity")+
      ylab("sales (USD)")+ xlab("Product") + theme(legend.position = "bottom",plot.title = element_text(size = 15, face = "bold"))+
      ggtitle("sales by product") + labs(fill="Year")
  })
  output$salesbyorderstatus <- renderPlot({
    ggplot(data = Salesdetails, aes(x=STATUS, y=SALES, fill=factor(PRODUCTLINE))) +
      geom_bar(position = "dodge", stat = "identity")+ ylab("Sales (USD)")+ xlab("status") + theme(legend.position = "bottom", plot.title = element_text(size = 15,face="bold"))+
      ggtitle("Sales by order status") + labs(fill="Products")
  })
}


##############################################################################
shinyApp(ui, server)