#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gsheet)
library(ggplot2)
library(lubridate)
library(dplyr)


### load the data ----
data = gsheet2tbl("https://docs.google.com/spreadsheets/d/13NyYu1pvpfA3Sy3hV_a9qa2Rd3Ls9CZO22KvPsrTbaY")

# NOTES: find useful info about spreadsheets queries at : construct_download_url {gsheet}


### Add new variables to the data ----
# Date : create date, create hour.

data$date = gsub(' .*', '', data$Horodateur)
data$hour = gsub('.* ', '', data$Horodateur)

# TODO : transforme horodateur in date variables (i.e. with lubridate)

### Create a categorical variable for time by hours ----
# time variable will be considered as factor

hcat <- NA 
for (i in 1:dim(data)[1]) {
  if (data$Hour[i] >= 8 & data$Hour[i] < 12) {
    hcat[i] <- 'Matin [08:12['
  } else if (data$Hour[i] >= 12 & data$Hour[i] < 16) {
    hcat[i] <- 'Midi [12:16['
  } else if (data$Hour[i] >= 16 & data$Hour[i] < 20) {
    hcat[i] <- 'Soir [16:20['
  } else if (data$Hour[i] >= 20 & data$Hour[i] < 23) {
    hcat[i] <- 'Nuit [20:23['
  }
}

data$hcat = factor(hcat, levels = c("Matin [08:12[", "Midi [12:16[", "Soir [16:20[", "Nuit [20:23["))
data$Discontinuous = as.numeric(data$hcat)

### Create a continue time variable ----

sdh = strsplit(data$hour, split = ':')
sdh = do.call(rbind, lapply(sdh, as.numeric))
data$Continuous = c(sdh[,1] + sdh[,2] / 60 + sdh[,3] / 3600)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Questionnaire"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = 'yvar', 
                     label = 'Y var.', 
                     choices = c('Humeur_NUM', '`Anxiété (en ce moment-ci)`'), 
                     selected = 'Humeur_NUM'),
         selectInput(inputId = 'time', 
                     label = 'Time', 
                     choices = c('Continuous', 'Discontinuous'), 
                     selected = 'continuousTime')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("aggTimePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### Define jitter accordings to x variables used
  # to define wheter it is useful
  jitterValue = reactive({
    if (input$yvar == '`Anxiété (en ce moment-ci)`') 0 else 0.2
  })
  
  
   output$aggTimePlot <- renderPlot({
     p = ggplot(data, aes_string(x = input$time, y = input$yvar), color = 'date') +
       theme_minimal()
     
     if (input$time == 'Discontinuous') {
       p = p + 
         geom_boxplot(aes_string(x = factor(hcat))) +
         geom_point(shape = 21, fill = 'white', 
                    position = position_jitter(w = jitterValue(), h = 0)) +
         # scale_shape(solid = FALSE) +
         geom_smooth(aes_string(x = input$time, y = input$yvar)) +
         theme(axis.text.x = element_text(angle = 60, hjust = 1))
     } else {
       p = p + 
         geom_point(shape = 21, fill = 'white') +
         geom_smooth(method = 'loess')
     }
     
     p = p + 
       coord_cartesian(ylim = c(0, 10)) +
       geom_hline(yintercept = 5, col = 'red')
     
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

