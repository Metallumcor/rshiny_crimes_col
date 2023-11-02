#' @import leaflet
server <- function(input, output, session){

  # Paleta de colores
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  # Bolean de cloropleta
  # Variable para guardar df_crime
  rv <- reactiveValues(db_crime = NULL, sp_obj = NULL, dt_dpto = NULL,
                       dt_mpio = NULL, click_sel = NULL, coroplet = FALSE,
                       sp_obj2 = NULL)

  # Mapa renderizado sobre Londres
  output$map <- renderLeaflet({
    test2021:::colombia_map
  })

  # Observador para la primera layer de graficas
  observeEvent(input$send,{

    withProgress(
      message = 'Cargando datos...',
      value = 1/4,
      expr = {

        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]),
                        "Error: Ingrese ambas fechas iniciales y finales."))
        validate(need(input$date[1] <= input$date[2],
                        "Error: Fecha inicial debe ser menor a final."))

        crime <- input$crime
        date_sel <- input$date
        m_type <- input$kind

        #print(crime)
        #print(date_sel)

        incProgress(1/4)

        tryCatch(
          expr = {
            rv$db_crime <- test2021:::col_selector_filter(crime, date_sel)
            sp_dpto <- db_sp_merge(col_coor_dept, col_dept,
                                        'DPTO_CCDGO')
            #print(str(sp_dpto))
            rv$dt_dpto <- rv$db_crime[, .N, by = list(DPTO_CCDGO)]
            #print(str(rv$dt_dpto))
            rv$dt_mpio <- rv$db_crime[, .N, by = list(DPTO_CCDGO, MPIO_CCDGO)]
            #print(str(rv$db_crime))
            rv$sp_obj <- db_sp_merge(rv$dt_dpto, sp_dpto, 'DPTO_CCDGO')
            #print(str(rv$sp_obj))
            test2021:::fillna0(rv$sp_obj@data)
            obs_df <- rv$sp_obj
          },
          error = function(x){
            obs_df <- NULL
          }
        )

        incProgress(1/4)

        tryCatch(

          expr = {
            if(input$kind == 'Calor'){
              # Creacion de lineas de contorno
              cl_df <- db_sp_merge(rv$db_crime, col_mpio,
                                   c('DPTO_CCDGO','MPIO_CCDGO'))@data
              cl_df <- na.omit(cl_df, 'fecha_hecho')
              cl_obj <- get_cl(cl_df, grid_size = c(100,100))

              # Proxy para mapa de calor
              leafletProxy('map', data = cl_obj$sp_obj) %>%
                clearShapes() %>%
                clearControls() %>%
                clearMarkers() %>%
                clearMarkerClusters() %>%
                setView(lat = 4.570868,lng = -74.297333, zoom = 7) %>%
                addPolygons(
                  color = heat.colors(cl_obj$cl_nlev, 0.5)[cl_obj$cl_lev],
                  weight = 2,
                  fillOpacity = 0.4, smoothFactor = 0.5
                ) %>%
                addLegend("bottomright",
                          colors = heat.colors(cl_obj$cl_nlev, NULL),
                          title = paste0('Ocurrencias de "',
                                         input$crime, '"'),
                          labels = paste0("Hasta ",unique(cl_obj$cl_lev))
                )

              rv$click_sel <- NULL
              rv$coroplet <- FALSE

            } else if (input$kind == 'Colores'){
              # factor para estadistica
              fc <- 10000
              # estadistica a graficar
              obs_df$STAT <- fc*obs_df$N/obs_df$TOTPOP
              # breaks para leyenda
              delta <- 0.001
              #print(obs_df@data)
              br_stat <- test2021::classIntervals(c(min(obs_df$STAT)-delta,
                                                    obs_df$STAT),
                                                  n = 5, style = 'jenks')
              # Proxy para mapa de coropletas
              leafletProxy('map', data = obs_df) %>%
                clearShapes() %>%
                clearControls() %>%
                setView(lat = 4.570868,lng = -74.297333, zoom = 6) %>%
                addPolygons(
                  group = obs_df$DPTO_CCDGO,
                  color = 'black',
                  weight = 1,
                  fillColor = ~pal_fun(STAT),
                  fillOpacity = 0.4, smoothFactor = 0.5,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "black",
                    opacity = 1.0,
                    bringToFront = TRUE,
                    sendToBack = TRUE
                  ),
                  label = ~DEP_NOM,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
                addLegend("bottomright",
                          colors = test2021::brewer.pal(5,'YlOrRd'),
                          title = paste0("Cantidad de '",input$crime,
                                         "' por cada ",fc," habitantes"),
                          labels = paste0("Hasta ",format(br_stat$brks[-1],
                                                          digits = 0)))
              # Cambio de valores reactivos
              rv$click_sel <- NULL
              rv$coroplet <- TRUE
              if(!is.null(rv$dpto_sel)){
                leafletProxy('map') %>%
                  showGroup(group = rv$dpto_sel$DPTO_CCDGO)
              }
            } else if (input$kind == 'Hexágonos'){
              # Merge de delitos con municipios
              sp_obj <- db_sp_merge(rv$dt_mpio, col_mpio,
                                    c('DPTO_CCDGO', 'MPIO_CCDGO'))
              sp_df <- sp_obj@data
              sp_df <- sp_df[complete.cases(sp_df),]
              # factor para estadistica
              fc <- 10000
              sp_df$STAT <- fc*sp_df$N/sp_df$TOTPOP
              # Hexagonos
              sp_hex <- hex_bins(sp_df, crs = 3118, n_hex = 250,
                                 agg_col = 'STAT', agg_fun = 'sum')
              # Colores
              delta <- 0.001
              br_stat <- test2021::classIntervals(c(min(sp_hex$sum)-delta,
                                                    sp_hex$sum),
                                                  n = 5, style = 'jenks')
              pal_fun2 <- colorBin("YlOrRd",bins = br_stat$brks, n = 5)
              # Grafica
              leafletProxy('map', data = sp_hex) %>%
                clearShapes() %>%
                clearControls() %>%
                setView(lat = 4.570868,lng = -74.297333, zoom = 6) %>%
                addPolygons(
                  color = 'black',
                  weight = 1,
                  fillColor = ~pal_fun2(sum),
                  fillOpacity = 0.25, smoothFactor = 0.5,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "black",
                    opacity = 1.0,
                    bringToFront = TRUE,
                    sendToBack = TRUE
                  ),
                  label = ~paste(round(sum,0),crime,'x10.000 hbtnts'),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
              # Cambio de valores reactivos
              rv$click_sel <- NULL
              rv$coroplet <- FALSE
            }

          },
          error = function(e){
            showModal(modalDialog(title = 'Lo sentimos!',
                                  tags$p('No se disponen suficientes datos para esta consulta')))
          }
        )

        incProgress(1/4)
      }
    )
  })

  ### Lo de aca corresponde a la segunda layer para coropletas
  # Controlador del evento click
  observeEvent(input$map_shape_click,{
    rv$click_sel <- input$map_shape_click
    if(!is.null(rv$dpto_sel)){
      leafletProxy('map') %>%
        removeShape(layerId = paste0('mun', rv$sp_obj2$DPTO_CCDGO,
                                     rv$sp_obj2$MPIO_CCDGO) ) %>%
        showGroup(group = rv$dpto_sel$DPTO_CCDGO)
    }
  })
  # Observador de eventos asociados a click en shape
  observe({
    if(!is.null(rv$click_sel) & rv$coroplet){
      rv$dpto_sel <- test2021:::get_location(c(rv$click_sel$lng,
                                               rv$click_sel$lat),
                                             rv$sp_obj)

      if(length(rv$dpto_sel)!=0){
        withProgress(
          message = 'Cargando datos...',
          value = 1/4,
          expr = {

            # Filtro
            dt_filt <- rv$dt_mpio[DPTO_CCDGO == rv$dpto_sel$DPTO_CCDGO,,]
            # Centroide
            centroid <- test2021:::get_centroid(unit = rv$dpto_sel$DPTO_CCDGO,
                                                data = col_dept,
                                                geo_div = 'DPTO_CCDGO')
            # Objeto espacial filtrado
            sp_filt <- subset(col_mpio,
                              col_mpio$DPTO_CCDGO == rv$dpto_sel$DPTO_CCDGO)

            incProgress(1/4)

            # merge entre dt y sp filtrados
            sp_obj <- db_sp_merge(dt_filt, sp_filt,
                                  c('DPTO_CCDGO', 'MPIO_CCDGO'))
            # llenar nulos
            test2021:::fillna0(sp_obj@data)
            # factor para estadistica
            fc <- 10000
            # estadistica a graficar
            sp_obj$STAT <- fc*sp_obj$N/sp_obj$TOTPOP
            rv$sp_obj2 <- sp_obj

            incProgress(1/4)
            #print(dt_filt)
            print(sp_obj@data)
            print(sp_filt@data)

            leafletProxy('map', data = sp_obj) %>%
              hideGroup(group = rv$dpto_sel$DPTO_CCDGO) %>%
              setView(lng = centroid[1], lat = centroid[2], zoom = 8) %>%
              addPolygons(
                layerId = ~paste0('mun',DPTO_CCDGO,MPIO_CCDGO),
                color = 'black',
                weight = 2,
                fillColor = 'black',
                fillOpacity = 0.3, smoothFactor = 0.5,
                highlight = highlightOptions(
                  weight = 4,
                  color = "white",
                  opacity = 0.7,
                  fillColor = 'white',
                  bringToFront = TRUE,
                  sendToBack = TRUE
                ),
                label = ~paste0(MPIO_CNMBR,", ",round(STAT,0),
                                " por cada ",fc," habitantes"),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))

            incProgress(1/4)

          }
        )
      }
    }
  })

}
