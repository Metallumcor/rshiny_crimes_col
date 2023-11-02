#' @import leaflet
server <- function(input, output, session){
  # Pre-procesamiento
  df_pre <- test2021:::london_prerend
  # Paleta de colores
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  # Bolean de cloropleta
  # Variable para guardar df_crime
  rv <- reactiveValues(df_crime = NULL, sp_obj = NULL,
                       clorop_exist = FALSE, district_f = NULL,
                       proxy_map = NULL, click_sel = NULL)
  # Mapa base
  base_map <- function(){
    test2021:::london_map
  }
  # Mapa reactivo
  react_map <- reactiveVal(base_map())
  # Mapa renderizado sobre Londres
  output$map <- renderLeaflet({
    react_map()
  })

  # Boton para retornar
  output$controls <- renderUI({
    req(input$map_shape_click)
    absolutePanel(id = "controls", top = 100, right = 50,
                  left = "auto", bottom = "auto", width = "auto", height = "auto",
                  actionButton(inputId = "reset",
                               label = "Regresar a vista general",
                               class = "btn-primary")
    )
  })

  # Observador de eventos asociados al boton
  observeEvent(input$send,{
    rv$district_f <- NULL
    withProgress(
      message = 'Cargando datos...',
      value = 1/5,
      expr = {
        validate(
          need(input$crime, message = 'Se requiere un Distrito')
        )

        crime <- input$crime
        date_sel <- format(input$date,'%Y-%m')

        incProgress(1/5)

        tryCatch(
          expr = {
            r_df <- test2021:::list_to_df(db_crime_london_a)
            rv$df_crime <- as.data.frame(
              test2021:::dt_filter_crime(x = as.data.table(r_df),
                                         list('category' = crime,
                                              'month' = date_sel))
            )
            rv$df_crime$latitude <- as.numeric(rv$df_crime$latitude)
            rv$df_crime$longitude <- as.numeric(rv$df_crime$longitude)
            obs_df <- sp_crime_cat(crime_s = crime, date_s = date_sel,
                                   sp_pre = df_pre, df = rv$df_crime)
            rv$sp_obj <- obs_df
              },
          error = function(x){
            obs_df <- NULL
          }
        )

        incProgress(1/5)

        tryCatch(
          expr = {
            # Pop-up con cifras
            #p_popup <- paste0("<strong>Densidad: </strong>", obs_df$STAT)
            # breaks para leyenda
            delta <- 0.0001
            br_stat <- test2021::classIntervals(c(min(obs_df$STAT)-delta,
                                                  obs_df$STAT),
                                                n = 5, style = 'quantile')

            rv$proxy_map <- leafletProxy('map', data = obs_df) %>%
              clearShapes() %>%
              clearControls() %>%
              clearMarkers() %>%
              clearMarkerClusters() %>%
              setView(lng = -0.135278,lat = 51.494720, zoom = 10) %>%
              addPolygons(
                color = 'black',
                weight = 1,
                fillColor = ~pal_fun(STAT),
                fillOpacity = 0.4, smoothFactor = 0.5,
                #popup = p_popup,
                highlight = highlightOptions(
                  weight = 3,
                  color = "black",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE
                ),
                label = ~DISTRICT,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
              addLegend("bottomright",
                        colors = test2021::brewer.pal(5,'YlOrRd'),
                        title = paste0("Concentración de '",input$crime,
                                      "' por hectarea"),
                        labels = paste0("Hasta ",format(br_stat$brks[-1],
                                                        digits = 4),
                                        " por hect"))
            rv$click_sel <- NULL
            rv$clorop_exist <- TRUE

            incProgress(1/5)
          },
          error = function(e){
            showModal(modalDialog(title = 'Lo sentimos!',
                                  tags$p('La solicitud no puede ser procesada de momento')))
          }
        )

        incProgress(1/5)
      }
    )
  })

  observeEvent(input$map_shape_click,{
    rv$click_sel <- input$map_shape_click
    shinyjs::show('controls')
  })

  #observeEvent(input$map_click,{
  #  data$clickedMarker <- NULL
  #  print(data$clickedMarker)
  #  })
  # Observador de eventos asociados a click en poly
  observe({
    if(!is.null(rv$click_sel) & rv$clorop_exist){
      rv$district_f <- test2021:::get_location(c(rv$click_sel$lng,
                                   rv$click_sel$lat),
                                 rv$sp_obj)

      if(length(rv$district_f)!=0){
        withProgress(
          message = 'Cargando datos...',
          value = 1/4,
          expr = {
            df_filt <- rv$df_crime %>%
              dplyr::filter(DISTRICT == rv$district_f$DISTRICT)

            incProgress(1/4)

            centroid <- test2021:::get_centroid(unit = rv$district_f$DISTRICT,
                                     data = lon_poly_ward_b,
                                     geo_div = 'DISTRICT')

            incProgress(1/4)

            leafletProxy('map', data = df_filt) %>%
              clearMarkers() %>%
              addPolygons(
                data = rv$sp_obj,
                color = 'black',
                weight = 1,
                fillColor = 'transparent'
              ) %>%
              setView(lng = centroid[1], lat = centroid[2], zoom = 13) %>%
              addCircleMarkers(color = "blue",
                               label = ~category, radius = 2,
                               opacity = 0.8,
                               fillOpacity = 0.6) %>%
              highlightOptions()

            incProgress(1/4)

          }
        )
      }
    }
  })

  # Resetear vista
  observeEvent(input$reset, {

    # Ocultar reset
    shinyjs::hide('controls')

    # Debug
    rv$click_sel <- NULL

    # Resetear mapa

    react_map(base_map())

  })
}
