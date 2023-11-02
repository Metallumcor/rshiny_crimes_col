#' @import leaflet
server <- function(input, output, session){

  # Paleta de colores
  col_palette <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")

  # Mapa renderizado sobre Londres
  output$map <- renderLeaflet({
    test2021:::london_map
  })

  # Observador de eventos asociados al boton
  observeEvent(input$send,{
    withProgress(
      message = 'Cargando datos...',
      value = 1/5,
      expr = {
        validate(
          need(input$district, message = 'Se requiere un Distrito')
        )

        district <- input$district
        date_sel <- format(input$date,'%Y-%m')

        incProgress(1/5)

        tryCatch(
          expr = {
            obs_df <- get_crime_london_unit(unit = district,
                                            data = lon_poly_ward_b,
                                            date = date_sel,
                                            geo_div = 'DISTRICT',
                                            lang = 'ESP')
            obs_df <- obs_df %>%
              dplyr::mutate(top_n = forcats::fct_lump(factor(category), n = 5,
                                                      other_level = 'other'),
                            latitude = as.numeric(latitude),
                            longitude = as.numeric(longitude))
            centroid <- test2021:::get_centroid(unit = district,
                                                data = lon_poly_ward_b,
                                                geo_div = 'DISTRICT')
           #obs_df_rank <- obs_df %>%
            #  dplyr::count(category) %>%
            #  dplyr::arrange(desc(n))
          },
          error = function(x){
            obs_df <- NULL
          }
        )

        incProgress(1/5)

        tryCatch(
          expr = {

            crim_pal <- colorFactor(col_palette, obs_df$top_n)

            leafletProxy('map', data = obs_df) %>%
              clearMarkers() %>%
              clearMarkerClusters() %>%
              clearControls() %>%
              setView(lng = centroid[1], lat = centroid[2], zoom = 13) %>%
              addCircleMarkers(clusterOptions = markerClusterOptions(),
                               stroke = FALSE, color = ~crim_pal(top_n),
                               label = ~category, radius = 15) %>%
              addLegend("bottomright", pal = crim_pal, values = ~top_n,
                        title = "Categoría")

            incProgress(1/5)
          },
          error = function(e){
            showModal(modalDialog(title = 'Lo sentimos!',
                                  tags$p('La localización y fechas ingresadas no devuelven dato alguno'),
                                  tags$p('Intenta con otra combinación.')))
          }
        )

        incProgress(1/5)
      }
    )
  })
}
