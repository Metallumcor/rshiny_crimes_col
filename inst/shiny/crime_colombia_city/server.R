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
  # Parametros globales - cambiar la base de datos en versiones futuras
  main_db <- reactive(get(db_call(input$city_sel, input$crime)))
  # Paletas de colores - Discreta
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
  # Parametros reactivos - mapa
  coords <- reactive(coord_city(input$city_sel))
  rv <- reactiveValues(db_crime = NULL, db_crime2 = NULL,
                       sp_obj1 = NULL, sp_obj2 = NULL, sp_obj3 = NULL,
                       sp_obj4  = NULL, click_sel = NULL, zoom_click = FALSE,
                       div1_sel  = NULL, zoom_save = NULL)
  # Output de mapa base
  output$map <- renderLeaflet({
    base_map(coords())
  })
  ## Funcionalidad principal
  # Observador de primer layer - mapa
  observeEvent(input$send,{

    withProgress(
      message = 'Cargando datos...',
      value = 1/4,
      expr = {

        #validate(need(!is.na(input$date[1]) & !is.na(input$date[2]),
        #              "Error: Ingrese ambas fechas iniciales y finales."))
        #validate(need(input$date[1] <= input$date[2],
        #              "Error: Fecha inicial debe ser menor a final."))

        crime <- input$crime
        date_sel <- format(input$date,'%Y-%m')
        m_type <- input$kind

        incProgress(1/4)

        tryCatch(
          expr = {
            pre_pro <- test2021:::prepro_colombia(db = main_db(),
                                       city_sel = input$city_sel,
                                       date_sel = date_sel)
            rv$db_crime <- pre_pro$db_crime
            rv$db_crime2 <- pre_pro$db_crime2
            rv$sp_obj1 <- pre_pro$sp_obj1
            rv$sp_obj2 <- pre_pro$sp_obj2
            rv$sp_obj3 <- pre_pro$sp_obj3
            rv$sp_obj4 <- pre_pro$sp_obj4
            #print(str(rv$sp_obj2@data))
            obs_df <- rv$sp_obj4
            #str(obs_df@data)
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
              cl_df <- rv$sp_obj3@data
              #print(cl_df)
              cl_df <- cl_df[complete.cases(cl_df),]
              cl_obj <- get_cl(cl_df, grid_size = c(100,100))
              # Paleta de colores - Continua
              pal_cont <- colorNumeric(
                palette = "YlOrRd",
                reverse = TRUE,
                domain = as.numeric(levels(cl_obj$cl_lev))[cl_obj$cl_lev])
              # Proxy para mapa de calor
              leafletProxy('map', data = cl_obj$sp_obj) %>%
                clearShapes() %>%
                clearControls() %>%
                clearMarkers() %>%
                clearMarkerClusters() %>%
                setView(lat = coords()$lat,lng = coords()$lng, zoom = coords()$zoom) %>%
                addPolygons(
                  color = heat.colors(cl_obj$cl_nlev, 0.5)[cl_obj$cl_lev],
                  weight = 2,
                  fillOpacity = 0.4, smoothFactor = 0.5
                ) %>%
                addLegend("bottomright",
                          pal = pal_cont,
                          title = paste0('Ocurrencias de "',
                                         input$crime, '"'),
                          bins = 5,
                          values = as.numeric(levels(cl_obj$cl_lev))[cl_obj$cl_lev]
                )

              rv$click_sel <- NULL

            } else if (input$kind == 'Colores'){
              # factor para estadistica
              fc <- 100
              # estadistica a graficar
              obs_df$STAT <- fc*obs_df$N/obs_df$AGG
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
                setView(lat = coords()$lat,lng = coords()$lng, zoom = coords()$zoom) %>%
                addPolygons(
                  # Ojo con los grupos aca, se necesita algo independiente
                  group = obs_df$CODIGO_DIV1,
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
                  # Ojo con el nombre
                  label = ~NOMBRE_DIV1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
                addLegend("bottomright",
                          colors = test2021::brewer.pal(5,'YlOrRd'),
                          title = paste0("Cantidad de '",input$crime,
                                         "' por cada ",fc," hectarea"),
                          labels = paste0("Entre ",
                                          pretty_legend(br_stat$brks[-1])))
              # Cambio de valores reactivos
              rv$click_sel <- NULL
              rv$zoom_click <- TRUE
              #if(!is.null(rv$dpto_sel)){
              #  leafletProxy('map') %>%
              #    showGroup(group = rv$dpto_sel$DPTO_CCDGO)
              #}

              #if(!is.null(rv$dpto_sel)){
              #  leafletProxy('map') %>%
              #    showGroup(group = rv$dpto_sel$DPTO_CCDGO)
              #}
            } else if (input$kind == 'Hexágonos'){
              # Merge de delitos con municipios
              sp_df <- rv$sp_obj4@data
              sp_df <- sp_df[complete.cases(sp_df),]
              # factor para estadistica
              fc <- 100
              sp_df$STAT <- fc*sp_df$N/sp_df$AGG
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
                setView(lat = coords()$lat,lng = coords()$lng, zoom = coords()$zoom) %>%
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
                  label = ~paste(round(sum,0),crime,'x km2'),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
                addLegend("bottomright",
                          colors = test2021::brewer.pal(5,'YlOrRd'),
                          title = paste0(input$city_sel,"- ocurrencias x km2"),
                          labels = paste0("Entre ",
                                          pretty_legend(br_stat$brks[-1])))
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
    if(!is.null(rv$div1_sel)){
      leafletProxy('map') %>%
        removeShape(layerId = paste0('DIV2', rv$zoom_save$CODIGO_DIV2,
                                     rv$zoom_save$CODIGO_DIV1) )
      #showGroup(group = rv$div1_sel$CODIGO_DIV1)
    }
  })
  # Observador de eventos asociados a click en shape - mapa
  observe({
    if(!is.null(rv$click_sel) & rv$zoom_click){
      rv$div1_sel <- test2021:::get_location(c(rv$click_sel$lng,
                                               rv$click_sel$lat),
                                             rv$sp_obj1)

      if(length(rv$div1_sel)!=0){
        withProgress(
          message = 'Cargando datos...',
          value = 1/4,
          expr = {

            # Filtro - FALTA INFORMACION FILTRADA
            # Centroide
            centroid <- test2021:::get_centroid(unit = rv$div1_sel$CODIGO_DIV1,
                                                data = rv$sp_obj2,
                                                geo_div = 'CODIGO_DIV1')
            #print(centroid)
            #print(rv$div1_sel)
            # Objeto espacial filtrado
            sp_filt <- subset(rv$sp_obj1,
                              rv$sp_obj1$CODIGO_DIV1 == rv$div1_sel$CODIGO_DIV1)
            #print(sp_filt)
            incProgress(1/5)

            # merge entre dt y sp filtrados - FALTA
            # llenar nulos - FALTA
            # factor para estadistica - FALTA
            # estadistica a graficar - FALTA
            rv$zoom_save <- sp_filt
            incProgress(1/4)
            #print(dt_filt)

            leafletProxy('map', data = sp_filt) %>%
              #hideGroup(group = rv$div1_sel$CODIGO_DIV1) %>%
              setView(lng = centroid[1], lat = centroid[2], zoom = coords()$zoom+1) %>%
              addPolygons(
                layerId = ~paste0('DIV2', CODIGO_DIV2, CODIGO_DIV1),
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
                label = ~CODIGO_DIV2,
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

  ## Funcionalidad secundaria
  # Reactivos para graficas
  db_plt <- reactive(
    data.table::as.data.table(get(db_call(input$city_sel2, input$crime2)))
  )
  sp_obj_plot <- reactive(
    test2021:::city_selector(input$city_sel2,'RAW')
  )
  # Graficas
  output$plot_1 <- plotly::renderPlotly({
    result <- db_plt()
    db_agg <- result[,.("N" = sum(NUM_HECHOS)), by = FECHA_MES]
    db_agg$Fecha <- lubridate::ym(db_agg$FECHA_MES)
    p <- ggplot2::ggplot(db_agg,
                    mapping = ggplot2::aes(
                      Fecha, N
                    )) +
      ggplot2::geom_line(col = 'blue') +
      ggplot2::geom_point(col = 'orange') +
      ggplot2::theme_bw() +
      ggplot2::xlab('Año') +
      ggplot2::ylab('Cantidad de ocurrencias')

    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = F, locale = "es")
  })
  #
  output$plot_2 <- plotly::renderPlotly({
    result <- db_plt()
    max_date <- max(result$FECHA_MES)
    min_date <- substr(
      lubridate::ym(max_date)-months(6),1,7
    )
    db_agg <- result[FECHA_MES >= min_date & FECHA_MES <= max_date,
                .("Avg" = sum(NUM_HECHOS)/6), by = CODIGO_DIV1]
    db_agg <- setorder(db_agg,-Avg)[,head(.SD, 5)]
    db_agg <- merge(unique(sp_obj_plot()@data[,c('CODIGO_DIV1', 'NOMBRE_DIV1')]),
                    db_agg, by = 'CODIGO_DIV1')
    db_agg$NOMBRE_DIV1 <- factor(db_agg$NOMBRE_DIV1,
                                 levels = db_agg$NOMBRE_DIV1[order(db_agg$Avg)])
    # Grafica
    plotly::plot_ly(db_agg, y = ~NOMBRE_DIV1, x = ~Avg, type = "bar",
                    orientation = "h",
                    hoverinfo = 'text',
                    text = ~paste(round(Avg,1),"x mes")) %>%
      plotly::layout(xaxis = list(title = "Promedio mensual (últimos 6 meses)"),
             yaxis = list(title = "")) %>%
      plotly::config(displayModeBar = F, locale = "es")
  })
  #
  output$plot_3 <- plotly::renderPlotly({
    result <- db_plt()
    max_date <- max(result$FECHA_MES)
    min_date <- substr(
      lubridate::ym(max_date)-months(6),1,7
    )
    db_agg <- result[FECHA_MES >= min_date & FECHA_MES <= max_date,
                     .("Agg" = sum(NUM_HECHOS)), by = SEXO]
    db_agg$SEXO <- as.factor(db_agg$SEXO)
    levels(db_agg$SEXO) <- sub("-","NO REPORTADO",levels(db_agg$SEXO))
    # Grafica
    plotly::plot_ly(db_agg, labels = ~SEXO, values = ~Agg) %>%
      plotly::add_pie(hole = 0.5) %>%
      plotly::layout(showlegend = FALSE,
                     xaxis = list(showgrid = FALSE,
                                  zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE,
                                  zeroline = FALSE, showticklabels = FALSE)) %>%
      plotly::config(displayModeBar = F, locale = "es")
    })
  #
  output$plot_4 <- plotly::renderPlotly({
    result <- db_plt()
    max_date <- max(result$FECHA_MES)
    min_date <- substr(
      lubridate::ym(max_date)-months(6),1,7
    )
    db_agg <- result[FECHA_MES >= min_date & FECHA_MES <= max_date,
                     .("Agg" = sum(NUM_HECHOS)), by = RANG_DIA]
    db_agg$RANG_DIA <- factor(db_agg$RANG_DIA,
                                 levels = db_agg$RANG_DIA[order(db_agg$Agg)])
    # Grafica
    plotly::plot_ly(db_agg, labels = ~RANG_DIA, values = ~Agg) %>%
      plotly::add_pie(hole = 0.5) %>%
      plotly::layout(showlegend = FALSE,
                     xaxis = list(showgrid = FALSE,
                                  zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE,
                                  zeroline = FALSE, showticklabels = FALSE)) %>%
      plotly::config(displayModeBar = F, locale = "es")
  })
}
