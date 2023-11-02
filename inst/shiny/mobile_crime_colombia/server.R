server <- function(input, output, session){
  # Mapa base
  coord_city <- function(x){
    if (x == 'Bogotá'){
      lng = -74.08175
      lat = 4.60971
      zoom = 11
    } else if (x == 'Medellín'){
      lng = -75.56359
      lat = 6.25184
      zoom = 12
    } else if (x == 'Cartagena'){
      lng = -75.51444
      lat = 10.39972
      zoom = 13
    }
    return(list(lng = lng, lat = lat, zoom = zoom))
  }
  base_map <- function(x){
    leaflet() %>%
      clearShapes() %>%
      clearControls() %>%
      clearMarkers() %>%
      removeLayersControl() %>%
      addTiles() %>%
      setView(lng = x$lng, lat = x$lat, zoom = x$zoom)
  }
  # Base de datos
  db_call <- function(city, crime){
    to_delete <- " a | de | en | "
    paste(tolower(substr(city,1,3)),
          gsub(to_delete,"_", tolower(crime)), sep = "_")
  }
  # Mejores leyendas
  pretty_legend <- function(x){
    if (is.numeric(x)){
      y <- c(0,x[1:(length(x)-1)])
      paste(round(y,0),"a",round(x,0))
    } else {
      y <- c("0",as.character(x[1:(length(x)-1)]))
      paste(y,"a",x)
    }
  }
  pretty_legend_2 <- function(x){
    if (is.numeric(x)){
      y1 <- c(x[2:(length(x)-1)])
      y2 <- c(x[3:length(x)])
      c("Ninguno", paste("Entre",round(y1,0),"a",round(y2,0)))
    } else {
      y1 <- as.character(x[2:(length(x)-1)])
      y2 <- as.character(x[3:length(x)])
      c("Ninguno", paste("Entre",y1,"a",y2))
    }
  }
  # Equivalente de PROPER en Excel o Capitalize en Python
  proper <- function(x){
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  }
  # Parametros globales - cambiar la base de datos en versiones futuras
  main_db <- reactive({get(db_call(input$city_sel, input$crime))})
  # Paletas de colores - Heatmap
  # pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  # Paleta de colores - Hexmap
  pal_fun_hex <- function(x, FUN){
    sapply(x,
           function(y){
             if(y == 0){
               "transparent"
             } else {
               FUN(y)
             }
           })
  }
  # Parametros reactivos - mapa
  coords <- reactive(coord_city(input$city_sel))
  rv <- reactiveValues(report_location = NULL, u_click = NULL)
  # Output de mapa base
  output$map <- renderLeaflet({
    base_map(coords())
  })
  ## Localizador GPS - solo funciona en navegador
  observeEvent(input$locate,{
    leafletProxy("map") %>%
      leaflet.extras::addControlGPS(
        options = leaflet.extras::gpsOptions(
          position = "topright",
          activate = TRUE,
          autoCenter = TRUE,
          maxZoom = 17,
          setView = TRUE
        )
      ) %>%
      leaflet.extras::activateGPS()
  })
  observeEvent(input$map_gps_located,{
    u_coord <- input$map_gps_located$coordinates
    rv$report_location <- as.numeric(u_coord)
    rv$u_click <- NULL
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lat = u_coord$lat, lng = u_coord$lng)
  })
  observeEvent(input$map_click,{
    rv$u_click <- input$map_click
    rv$report_location <- as.numeric(rv$u_click[-3])
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lat = rv$u_click$lat, lng = rv$u_click$lng)
    if(!is.null(input$map_gps_located)){
      leafletProxy("map") %>%
        leaflet.extras::deactivateGPS()
    }
  })
}
