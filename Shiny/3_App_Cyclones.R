#-------------------------------------------------------------------------------
# APPLICATION SHINY : CYCLONES
#-------------------------------------------------------------------------------

# Permet de charger les différents packages et de les télécharger si inexistants
require(shiny)
require(sf)
require(DT)
require(leaflet)
require(plotly)

# Import des données
name_cyclone <- read.csv("./data/name_date_cyclone.txt",sep=',', header = TRUE )
name_cyclone <- name_cyclone[order(as.Date(name_cyclone$start),decreasing = TRUE),]
mada_cycl_p <- st_read("./data/IBTrACS.SI.list.v04r00.points_Mada_2015_present.shp")

#-------------------------------------------------------------------------------
# UI
#-------------------------------------------------------------------------------

ui <- fluidPage(
  # Création d'une ligne composée de plusieurs colonnes (12 max)
  fluidRow(
    # Espace de 3 colonnes
    column(width = 3,
                  selectInput(inputId = "picker_cyclone",
                  label = "Select a cyclone",
                  width = "100%",
                  choices = c(" ", name_cyclone$name),
                  selected = " ",
                  multiple = FALSE),
              radioButtons(inputId ="radio_button", label = NULL,
               choices = list("Wind (km/h)" = "wind_kmh", "Pressure (hPa)" = "USA_PRES"),
               selected = "wind_kmh")
              ),
    # Espace de 4 colonnes
    column(width = 4,
           plotlyOutput(outputId = "plot")),
    # Espace de 4 colonnes
    column(width = 4,
           dataTableOutput(outputId = "dt_table"))),
  # Création d'une nouvelle ligne
  fluidRow(
    # Espace de 3 colonnes
    column(width = 3),
    # Espace de 9 colonnes
    column(width = 9,
           leafletOutput(outputId = "map")))
)


#-------------------------------------------------------------------------------
# SERVER
#-------------------------------------------------------------------------------

server <- function(input, output) {
  
  
  #Sortie Graphique
  output$plot <- renderPlotly({
    # Ici, nous séléctionnons les points du cyclone séléctionné par l'utilisateur
    select <- mada_cycl_p[mada_cycl_p$NAME == input$picker_cyclone,]
    # Transformation en dataframe (format lignes-colonnes)
    df <- data.frame(select)
    # Ajout d'un nouveau champs "time" comprenant la variable "ISO_TIME" en format temps
    df$time <- as.POSIXct(df$ISO_TIME)
    
    # Condition :
    # Si l'input$radio_button est égale à "wind_kmh" alors les variables 
    # yl = "Wind Speed (km/h)" et colo = red 
    if (input$radio_button== "wind_kmh"){
      yl <- "Wind Speed (km/h)"
      colo <- "red"
    }
    # Si non
    # yl = "Pressure (hPa)" et colo = blue
    else{
      yl <- "Pressure (hPa)"
      colo <- "blue"
    }
    
    # Configuration d'un graphique intéractif via le package plotly
    plot_ly()%>% add_trace(data = df ,
                           x = df$time,
                           y = df[,input$radio_button],
                           line = list(color = colo),
                           marker = list(color = colo),
                           type = 'scatter', mode = 'lines+markers')%>%
      layout(title = " " ,
             xaxis = list(title = list(text="<b>Time</b>",font=list(size=20))),
             yaxis = list(title=list(text=paste0("<b>",yl,"</b>"), font=list(size=20))),
             showlegend = FALSE)
  })
  
  
  # Sortie tableau
  output$dt_table <- renderDataTable({
    
    # Traitements identiques 
    select <- mada_cycl_p[mada_cycl_p$NAME == input$picker_cyclone,]
    df <- data.frame(select)
    df$time <- as.POSIXct(df$ISO_TIME)
    
    # Création d'une table de sortie
    datatable(df[,c('ISO_TIME','USA_PRES',"wind_kmh")],
              colnames = c('Time' = 1, 'Pressure (hPa)' = 2,"Wind (km/h)"=3),
              rownames=FALSE,
              extensions = "Buttons",
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })
  
  
  
  
  # Sortie Carte
  output$map <- renderLeaflet({
    
    # Ici, nous séléctionnons les points du cyclone séléctionné par l'utilisateur
    select <- mada_cycl_p[mada_cycl_p$NAME == input$picker_cyclone,]
    
    # Ajout des points de mesure du cyclone
    leaflet(options = leafletOptions(wheelPxPerZoomLevel=150, zoomSnap= 0.05) )%>%
            addMarkers(data=select, group = "Trajectoire cyclone",layerId = paste(substr(select$ISO_TIME, 1,10),substr(select$ISO_TIME, 12,19)),
                       popup= select$ISO_TIME,
                       popupOptions = popupOptions(style = list("font-weight" = "normal",padding = "3px 50px"),
                                             direction = "auto", maxWidth = 500))%>%
            addProviderTiles(providers$Esri.WorldImagery,group = "Satellite")%>%
            addProviderTiles(providers$OpenStreetMap.Mapnik,group = "OSM")%>%
            addLayersControl(baseGroups = c("Satellite","OSM"),
                             overlayGroups = c("Trajectoire cyclone"),
                             options = layersControlOptions(collapsed = TRUE))%>%
            addMeasure(position = "bottomleft",
                       primaryLengthUnit = "meters",
                       primaryAreaUnit = "sqmeters")
  })
  
  
  
}


#-------------------------------------------------------------------------------
# RUNAPP
#-------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)



