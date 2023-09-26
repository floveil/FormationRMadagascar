#-------------------------------------------------------------------------------
# TEMPLATE SHINY 
#-------------------------------------------------------------------------------

# Permet de charger les différents packages et de les télécharger si inexistants
require(shiny)


# Définition du UI de l’application
ui <- fluidPage(
)

# Définition du server de l’application
server <- function(input, output) {
}

# Création et exécution de l’application
shinyApp(ui = ui, server = server)


