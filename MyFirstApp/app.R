#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# this code will only be run at start up
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)

mortality_zaf <- read_csv("~/Data science/R/shiny-tutorial/mortality_zaf.csv")
population_zaf <- read_csv("~/Data science/R/shiny-tutorial/population_zaf.csv")

mortality <- mortality_zaf %>% filter(indicator != "All causes")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("SA death statistics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "province",
                     label = "Choose a province:",
                     choices = unique(mortality_zaf$province),
                     selected = "Gauteng",
                     multiple = TRUE
                     ),
         
         checkboxInput(inputId = "showTable",
                       label = "Show data table?",
                       value = FALSE #opening default value of checkbox
                       )
         ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("LinePlot"),
         dataTableOutput("mortalityTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   # Note, BarPlot is the name of the plot
   output$LinePlot <- renderPlot({
     mortality %>%
       filter(province %in% input$province) %>%
       ggplot(aes(year, deaths, color=indicator)) +
       facet_wrap(~province) + # using column 'province' to decide what does into which facet, can add scales = "free" to give each one it's own y axis scaling
       # facet_wrap(~province, scales = "free") +
       geom_line(alpha = 0.8, size = 2) +
       theme_minimal(base_size = 18)
       
   })
   
   output$mortalityTable <- renderDataTable({
     if(input$showTable){
       datatable(mortality %>%  #cppied same data set from above output.  Actually need to make reusable objects for this
         filter(province %in% input$province))
     }
       
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

