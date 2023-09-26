#-------------------------------------------------------------------------------
# EXEMPLE APP SHINY UI + SERVER
#-------------------------------------------------------------------------------

# Permet de charger les différents packages et de les télécharger si inexistants
require(shiny)


# Définition du UI de l’application
ui <- fluidPage(
          #Input
          sliderInput(inputId = "slider1",label = "Nombre d'individus",min= 1, max=50,value=30),
          textInput("text1","Titre Figure", value ="Histogramme"),
          selectInput("select1", "Couleur", choices=c("Red","Green","Blue"),selected="Red"),
          #Output
          plotOutput("plot")


)

# Définition du server de l’application
server <- function(input, output) {
  
  # Output
  output$plot <- renderPlot({
    plot(x= runif(input$slider1),y =runif(input$slider1), 
         main = input$text1, col=input$select1, type ="h",lwd=10)
  })
  
}

# Création et exécution de l’application
shinyApp(ui = ui, server = server)