rm(list = ls())
library(shiny)
library(tidyverse)
# rsconnect::setAccountInfo(name='803etu-viktoriya-brazhenko', token='E194672739EEA37517EFC81C0A69EBD3', secret='scfcToB8V/r/eqOiju9NbwJOTp2zTq3iOEXDVX6l')
sodra <- read.csv("lab_sodra.csv")
eAC <- 494100
DF <- sodra %>% filter(ecoActCode==eAC)

shinyApp(ui =fluidPage(
  titlePanel("Krovininis kelių transportas"),

  sidebarLayout(
    sidebarPanel(
      selectizeInput("Kodas","Įveskite įmonės kodą",DF$jarCode)
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
), server = function(input, output) {
  
    output$distPlot <- renderPlot({
    
    DF <- DF %>% filter(jarCode==input$Kodas)
    ggplot(DF, aes(x=month, y=avgWage,group=name,color=name))+geom_line(size=0.7)+ theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  })
}
)
