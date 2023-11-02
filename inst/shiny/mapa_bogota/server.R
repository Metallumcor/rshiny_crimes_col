server <- function(input, output, session){
  # Funciones de test2021
  coord_city <- test2021:::coord_city
  base_map <- test2021:::base_map
  pretty_legend <- test2021:::pretty_legend
  pretty_legend_2 <- test2021:::pretty_legend_2
  proper <- function(x){     # Equivalente de PROPER en Excel o Capitalize en Python
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  }
  pal_fun_hex <- test2021:::pal_fun_hex
  db_call <- test2021:::db_call
  weekend <- test2021:::weekend
  #color_assign <- test2021:::color_assign
  # Parametros globales - cambiar la base de datos en versiones futuras
  blue2red <- grDevices::colorRampPalette(c('blue','cyan','white','orange','red'))
  main_db <- reactive(db_call(input$city_sel))
  main_db2 <- reactive(db_call(input$city_sel2))
  main_db3 <- reactive(db_call(input$city_sel3))
  main_db4 <- reactive(db_call(input$city_sel4))
  # Parametros reactivos - mapa
  coords <- reactive(coord_city(input$city_sel))
  coords_2 <- reactive(coord_city(input$city_sel3))
  coords_corr <- reactive(coord_city(input$city_sel2))
  n_hex_city <- 250
  rv <- reactiveValues(db_crime = NULL, db_crime2 = NULL, db_crime3 = NULL,
                       sp_obj1 = NULL, sp_obj2 = NULL, sp_obj3 = NULL,
                       click_sel = NULL, zoom_click = FALSE,
                       div1_sel  = NULL, zoom_save = NULL,
                       report_location = NULL)
  # El siguiente observador ayuda a limpiar el mapa al cambiar de ciudad
  observeEvent(input$city_sel,{
    rv$click_sel <- NULL
    rv$zoom_click <- FALSE
  })
  # Lista desplegable de delitos
  observe({
    x <- main_db() %>%
      eval() %>%
      dplyr::group_by(Delito) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(Porcentaje = 100*n/sum(n)) %>%
      dplyr::filter(Porcentaje > 0.1) %>%
      dplyr::select(Delito) %>%
      unlist() %>%
      tolower() %>%
      proper()
    updateSelectizeInput(session, inputId = "crime_list", choices = x, server = TRUE)
  })
  observe({
    x <- main_db2() %>%
      eval() %>%
      dplyr::group_by(Delito) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(Porcentaje = 100*n/sum(n)) %>%
      dplyr::filter(Porcentaje > 0.1) %>%
      dplyr::select(Delito) %>%
      unlist() %>%
      tolower() %>%
      proper()
    updateSelectizeInput(session, inputId = "crime_list2", choices = x, server = TRUE)
  })
  observe({
    x <- main_db3() %>%
      eval() %>%
      dplyr::group_by(Delito) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(Porcentaje = 100*n/sum(n)) %>%
      dplyr::filter(Porcentaje > 0.1) %>%
      dplyr::select(Delito) %>%
      unlist() %>%
      tolower() %>%
      proper()
    updateSelectizeInput(session, inputId = "crime_list3", choices = x, server = TRUE)
  })
  observe({
    x <- main_db4() %>%
      eval() %>%
      dplyr::group_by(Delito) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(Porcentaje = 100*n/sum(n)) %>%
      dplyr::filter(Porcentaje > 0.1) %>%
      dplyr::select(Delito) %>%
      unlist() %>%
      tolower() %>%
      proper()
    updateSelectizeInput(session, inputId = "crime_list4", choices = x, server = TRUE)
  })
  # Lista desplegable de unidades superiores
  observe({
    city_obj <- test2021:::city_selector(input$city_sel4,'RAW')
    updateSelectizeInput(session, inputId = "units_list4",
                         choices = city_obj$NOMBRE_DIV1, server = TRUE)
  })
  # Output de mapa base
  output$map <- renderLeaflet({
    base_map(coords())
  })
  # Output de mapa para entradas de usuario
  output$map_user <- renderLeaflet({
    base_map(coords_2())
  })
  # Output de mapa de correlacion
  output$map_corr <- renderLeaflet({
    base_map(coords_corr())
  })
  ## Funcionalidad principal
  # Observador de primer layer - mapa
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
        date_sel <- as.POSIXct(input$date)
        m_type <- input$kind

        incProgress(1/4)

        tryCatch(
          expr = {
            proc_db <- eval(main_db())[Delito == toupper(input$crime_list),,]
            pre_pro <- test2021:::prepro_points(db = proc_db,
                                       city_sel = input$city_sel,
                                       date_sel = date_sel)
            rv$db_crime <- pre_pro$db_crime
            rv$db_crime2 <- pre_pro$db_crime2
            rv$db_crime3 <- pre_pro$db_crime3
            rv$sp_obj1 <- pre_pro$sp_obj1
            rv$sp_obj2 <- pre_pro$sp_obj2
            rv$sp_obj3 <- pre_pro$sp_obj3
            obs_df <- rv$sp_obj3
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
              cl_df <- rv$db_crime
              cl_df <- cl_df[complete.cases(cl_df),]
              cl_obj <- get_cl(as.data.frame(cl_df), grid_size = c(100,100))
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
                leaflet.extras::clearHeatmap() %>%
                removeLayersControl() %>%
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
                ) %>%
                addMarkers(
                  data = cl_df,
                  group = "Marcadores",
                  popup = ~paste("Fecha:",format(FECHA,"%d/%m/%y"),"<br>",
                                 "Hora del día:",proper(RANG_DIA),"<br>",
                                 "Sexo:",proper(SEXO)),
                  clusterOptions = markerClusterOptions()
                ) %>%
                addLayersControl(
                  overlayGroups = "Marcadores",
                  options = layersControlOptions(collapsed = FALSE)
                ) %>%
                hideGroup("Marcadores")

              rv$click_sel <- NULL
              rv$zoom_click <- FALSE
              rv$div1_sel <- NULL

            } else if (input$kind == 'Colores'){
              # factor para estadistica
              fc <- 100
              # estadistica a graficar
              obs_df$STAT <- fc*obs_df$N/obs_df$AGG
              # breaks para leyenda
              delta <- 0.001
              br_stat <- test2021::classIntervals(c(min(obs_df$STAT)-delta,
                                                    obs_df$STAT),
                                                  n = 5, style = 'jenks')
              # Paleta
              pal_fun <- colorBin("YlOrRd",bins = br_stat$brks, n = 5)
              # Proxy para mapa de coropletas
              leafletProxy('map', data = obs_df) %>%
                clearShapes() %>%
                clearMarkers() %>%
                clearControls() %>%
                leaflet.extras::clearHeatmap() %>%
                removeLayersControl() %>%
                setView(lat = coords()$lat,lng = coords()$lng, zoom = coords()$zoom) %>%
                addPolygons(
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
                          title = paste0(input$city_sel,"- ocurrencias x km2"),
                          labels = paste0("Entre ",
                                          pretty_legend(br_stat$brks[-1])))
              # Cambio de valores reactivos
              rv$click_sel <- NULL
              rv$zoom_click <- TRUE
              if(!is.null(rv$div1_sel)){
                leafletProxy('map') %>%
                  showGroup(group = rv$div1_sel$CODIGO_DIV1)
              }
            } else if (input$kind == 'Hexágonos'){
              # Merge de delitos con municipios
              sp_df <- rv$db_crime
              # Hexagonos
              sp_hex <- hex_bins(sp_df, crs = 4326, n_hex = n_hex_city)
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
                clearMarkers() %>%
                leaflet.extras::clearHeatmap() %>%
                removeLayersControl() %>%
                setView(lat = coords()$lat,lng = coords()$lng, zoom = coords()$zoom) %>%
                addPolygons(
                  color = 'black',
                  weight = 0.3,
                  fillColor = ~pal_fun_hex(sum, pal_fun2),
                  fillOpacity = 0.4, smoothFactor = 0.5) %>%
                addPolygons(
                  group = "Resaltador",
                  color = 'black',
                  weight = 0.3,
                  fillColor = "transparent",
                  highlight = highlightOptions(
                    weight = 1,
                    color = "black",
                    opacity = 1.0,
                    bringToFront = TRUE
                  ),
                  label = ~paste(round(sum,0),'x km2'),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
                addMarkers(
                  data = sp_df,
                  group = "Marcadores",
                  popup = ~paste("Fecha:",format(FECHA,"%d/%m/%y"),"<br>",
                                 "Hora del día:",proper(RANG_DIA),"<br>",
                                 "Sexo:",proper(SEXO)),
                  clusterOptions = markerClusterOptions()
                ) %>%
                addLayersControl(
                  overlayGroups = c("Marcadores", "Resaltador"),
                  options = layersControlOptions(collapsed = FALSE)
                ) %>%
                hideGroup(c("Marcadores", "Resaltador")) %>%
                addLegend("bottomright",
                          colors = test2021::brewer.pal(5,'YlOrRd'),
                          title = paste0(input$city_sel,"- ocurrencias x km2"),
                          labels = paste0("Entre ",
                                          pretty_legend(br_stat$brks[-1])))

              # Cambio de valores reactivos
              rv$click_sel <- NULL
              rv$zoom_click <- FALSE
              rv$div1_sel <- NULL
            } else if (input$kind == 'Calor 2'){
              # Creacion de lineas de contorno
              cl_df <- rv$db_crime
              # Proxy para mapa de calor
              leafletProxy('map', data = cl_df) %>%
                clearShapes() %>%
                clearControls() %>%
                clearMarkers() %>%
                leaflet.extras::clearHeatmap() %>%
                removeLayersControl() %>%
                setView(lat = coords()$lat,lng = coords()$lng,
                        zoom = coords()$zoom) %>%
                leaflet.extras::addHeatmap(radius = 12,
                                           blur = 20) %>%
                addMarkers(
                  data = cl_df,
                  group = "Marcadores",
                  popup = ~paste("Fecha:",format(FECHA,"%d/%m/%y"),"<br>",
                                 "Hora del día:",proper(RANG_DIA),"<br>",
                                 "Sexo:",proper(SEXO)),
                  clusterOptions = markerClusterOptions()
                ) %>%
                addLayersControl(
                  overlayGroups = "Marcadores",
                  options = layersControlOptions(collapsed = FALSE)
                ) %>%
                hideGroup("Marcadores")

              rv$click_sel <- NULL
              rv$zoom_click <- FALSE
              rv$div1_sel <- NULL
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
        clearMarkers() %>%
        clearMarkerClusters() %>%
        removeShape(layerId = paste0('DIV2', rv$zoom_save$CODIGO_DIV2,
                                     rv$zoom_save$CODIGO_DIV1) ) %>%
        removeShape(layerId = paste0('DIV2_B', rv$zoom_save$CODIGO_DIV2,
                                     rv$zoom_save$CODIGO_DIV1) ) %>%
        removeControl(layerId = paste0('DIV2', rv$zoom_save$CODIGO_DIV1)) %>%
        showGroup(group = rv$div1_sel$CODIGO_DIV1)
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

            # Filtro
            dt_filt <- rv$db_crime2[CODIGO_DIV1 == rv$div1_sel$CODIGO_DIV1]
            dt_filt2 <- rv$db_crime[CODIGO_DIV1 == rv$div1_sel$CODIGO_DIV1]
            # Centroide
            centroid <- test2021:::get_centroid(unit = rv$div1_sel$CODIGO_DIV1,
                                                data = rv$sp_obj2,
                                                geo_div = 'CODIGO_DIV1')
            # Objeto espacial filtrado
            sp_filt <- subset(rv$sp_obj1,
                              rv$sp_obj1$CODIGO_DIV1 == rv$div1_sel$CODIGO_DIV1)
            incProgress(1/5)

            # merge entre dt y sp filtrados
            sp_obj <- db_sp_merge(dt_filt, sp_filt,
                                  c('CODIGO_DIV1', 'CODIGO_DIV2'))
            # llenar nulos
            test2021:::fillna0(sp_obj@data)
            # factor para estadistica
            fc <- 100
            # estadistica a graficar
            sp_obj$STAT <- fc*sp_obj$N/sp_obj$AREA_HECTA
            rv$zoom_save <- sp_obj
            incProgress(1/4)
            # paleta
            br_stat <- c(0, seq(1, max(sp_obj$STAT), length.out = 5))
            pal_fun3 <- colorBin("plasma", bins = br_stat, n = 5)

            leafletProxy('map', data = sp_obj) %>%
              clearMarkers() %>%
              hideGroup(group = rv$div1_sel$CODIGO_DIV1) %>%
              setView(lng = centroid[1], lat = centroid[2], zoom = coords()$zoom+2) %>%
              addPolygons(
                group = ~paste0('DIV2', CODIGO_DIV2, CODIGO_DIV1),
                layerId = ~paste0('DIV2', CODIGO_DIV2, CODIGO_DIV1),
                color = 'black',
                weight = 2,
                fillColor = ~pal_fun3(STAT),
                fillOpacity = 0.3,
                smoothFactor = 0.5) %>%
              addPolygons(
                group = "Resaltador",
                layerId = ~paste0('DIV2_B', CODIGO_DIV2, CODIGO_DIV1),
                color = 'black',
                weight = 2,
                fillColor = 'transparent',
                highlight = highlightOptions(
                  weight = 4,
                  color = "white",
                  opacity = 0.7,
                  bringToFront = TRUE
                ),
                label = ~paste0(round(STAT,0)," x km2"),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
              addLegend(
                "topright",
                layerId = ~paste0('DIV2',CODIGO_DIV1),
                colors = test2021::plasma(5),
                title = paste0(rv$div1_sel$NOMBRE_DIV1,"- ocurrencias x km2"),
                          labels = pretty_legend_2(br_stat)) %>%
              addMarkers(
                data = dt_filt2,
                group = "Marcadores",
                popup = ~paste("Fecha:",format(FECHA,"%d/%m/%y"),"<br>",
                               "Hora del día:",proper(RANG_DIA),"<br>",
                               "Sexo:",proper(SEXO)),
                clusterOptions = markerClusterOptions()
              ) %>%
              addLayersControl(
                overlayGroups = c("Marcadores", "Resaltador"),
                options = layersControlOptions(collapsed = FALSE)
              ) %>%
              hideGroup(c("Marcadores", "Resaltador"))


            incProgress(1/4)

          }
        )

      }
    }
  })

  ## Funcionalidad secundaria
  # Reactivos para graficas
  db_plt <- reactive(
    eval(main_db2())[Delito == toupper(input$crime_list2),,]
  )
  sp_obj_plot <- reactive(
    test2021:::city_selector(input$city_sel2,'RAW')
  )
  observe({
    x <- c("Todo el día",proper(tolower(unique(db_plt()$RANG_DIA))))
    updateSelectizeInput(session,
                         inputId = "rango_dia", choices = x, server = TRUE)
  })
  observe({
    x <- unique(db_plt()$NOM_DIV1)
    updateSelectizeInput(session,
                         inputId = "div_pca", choices = x, server = TRUE)
  })
  rang_day <- reactive({
    req(db_plt())
    if(input$rango_dia != "Todo el día")
      toupper(input$rango_dia)
    else
      unique(db_plt()$RANG_DIA)
  })
  dia_sem <- reactive({
    req(db_plt())
    if(input$dia_sem == "Toda la semana")
      x <- rep(TRUE,length(db_plt()$FECHA))
    if(input$dia_sem == "Fin de semana")
      x <- weekend(db_plt()$FECHA)
    if(input$dia_sem == "Entre semana")
      x <- !weekend(db_plt()$FECHA)
    x
  })
  observe({
    x <- unique(db_plt()$NOM_DIV1)
    updateSelectizeInput(session,
                         inputId = "sel_div", choices = x, server = TRUE)
  })
  ### Graficas
  # Serie de tiempo
  output$plot_1 <- plotly::renderPlotly({
    req(dia_sem())
    req(rang_day())
    result <- db_plt()
    if(!inherits(result$FECHA,"POSIXct")){
      result$FECHA <- as.POSIXct(paste0(result$FECHA,"-01"))
    }
    db_agg <- result[dia_sem() & RANG_DIA %in% rang_day(),
                     .(.N,"Fecha" = as.POSIXct(paste(yr,mon,"01",sep="-"))),
                     by = .(yr = year(FECHA), mon = month(FECHA))]
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
  # Top de lugares con mas reportes
  output$plot_2 <- plotly::renderPlotly({
    req(dia_sem())
    req(rang_day())
    result <- db_plt()
    if(!inherits(result$FECHA,"POSIXct")){
      result$FECHA <- as.POSIXct(paste0(result$FECHA,"-01"))
    }
    max_date <- max(result$FECHA)
    min_date <- seq(max_date, length = 2, by = "-6 months")[2]
    db_agg <- result[dia_sem() & FECHA >= min_date & FECHA <= max_date  &
                       (RANG_DIA %in% rang_day()),
                .("Avg" = .N/6), by = CODIGO_DIV1]
    db_agg <- setorder(db_agg,-Avg)[,head(.SD, 5)]
    db_agg <- merge(unique(sp_obj_plot()@data[,c('CODIGO_DIV1', 'NOMBRE_DIV1')]),
                    db_agg, by = 'CODIGO_DIV1')
    db_agg$NOMBRE_DIV1 <- factor(db_agg$NOMBRE_DIV1,
                                 levels = db_agg$NOMBRE_DIV1[order(db_agg$Avg)])

    plotly::plot_ly(db_agg, y = ~NOMBRE_DIV1, x = ~Avg, type = "bar",
                    orientation = "h",
                    hoverinfo = 'text',
                    text = ~paste(round(Avg,1),"x mes")) %>%
      plotly::layout(xaxis = list(title = "Promedio mensual (últimos 6 meses)"),
             yaxis = list(title = "")) %>%
      plotly::config(displayModeBar = F, locale = "es")
  })
  # Donnut sexo
  output$plot_3 <- plotly::renderPlotly({
    result <- db_plt()
    if(!inherits(result$FECHA,"POSIXct")){
      result$FECHA <- as.POSIXct(paste0(result$FECHA,"-01"))
    }
    max_date <- max(result$FECHA)
    min_date <- seq(max_date, length = 2, by = "-6 months")[2]
    db_agg <- result[FECHA >= min_date & FECHA <= max_date,
                     .("Agg" = .N), by = SEXO]
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
  # Donnut rango horario
  output$plot_4 <- plotly::renderPlotly({
    result <- db_plt()
    if(!inherits(result$FECHA,"POSIXct")){
      result$FECHA <- as.POSIXct(paste0(result$FECHA,"-01"))
    }
    max_date <- max(result$FECHA)
    min_date <- seq(max_date, length = 2, by = "-6 months")[2]
    db_agg <- result[FECHA >= min_date & FECHA <= max_date,
                     .("Agg" = .N), by = RANG_DIA]
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
  # Corrplot
  output$plot_5 <- renderPlot({
    result <- db_plt()
    req(input$sel_div)
    # PROC
    validate(need(!is.na(input$date_cor[1]) & !is.na(input$date_cor[2]),
                  "Error: Ingrese ambas fechas iniciales y finales."))
    validate(need(input$date_cor[1] <= input$date_cor[2],
                  "Error: Fecha inicial debe ser menor a final."))
    cr_result <- ctab_period(result, date_range = input$date_cor,
                             div_name = "NOM_DIV1", var_agg = "Numero Hechos")
    cr_lag <- ctab_lag(cr_result, selection = input$sel_div,
                       n_lags = input$n_lags)
    ctab_corplot(cr_lag,selection = input$sel_div, n_show = 5,
                 cor_method = "pearson")
  })
  ### Mapa de correlaciones
  #
  df_map_cor <- reactive(city_selector(input$city_sel2,
                              c("CODIGO_DIV1","NOMBRE_DIV1"),
                                  "AREA_HECTA")[[1]])
  #
  observe({
    req(input$sel_div)
    req(input$date_cor)
    req(input$city_sel2)
    req(df_map_cor())
    req(db_plt())
    withProgress(
      message = 'Cargando datos...',
      value = 1/4,
      expr = {

        validate(need(!is.na(input$date_cor[1]) & !is.na(input$date_cor[2]),
                      "Error: Ingrese ambas fechas iniciales y finales."))
        validate(need(input$date_cor[1] <= input$date_cor[2],
                      "Error: Fecha inicial debe ser menor a final."))

        incProgress(1/4)

        obs_df <- df_map_cor()
        # PARTE MODIFICADA PARA COLORES
        x_tab <- ctab_period(db_plt(),
                             date_range = input$date_cor,
                             div_name = "NOM_DIV1",
                             var_agg = "Numero Hechos")
        x_lagged <- ctab_lag(x_tab,
                             selection = input$sel_div,
                             n_lags = input$n_lags)
        tb_color <- color_assign(x_lagged, selection = input$sel_div,
                                 color = blue2red(25))
        obs_df@data <- dplyr::inner_join(x = obs_df@data,
                                         y = tb_color,
                                         by = c("NOMBRE_DIV1" = "SECTOR"))


        incProgress(1/4)

        # Proxy para mapa de coropletas
        leafletProxy('map_corr', data = obs_df) %>%
          clearShapes() %>%
          clearControls() %>%
          setView(lat = coords_corr()$lat,lng = coords_corr()$lng, zoom = coords_corr()$zoom) %>%
          addPolygons(
            color = 'black',
            weight = 1,
            fillColor = ~COLOR,
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
                    pal = colorNumeric(blue2red(25),
                                       domain = seq(-1,1,length = 5),
                                       reverse = TRUE),
                    title = "Correlación",
                    labFormat = labelFormat(transform = function(x) sort(seq(-1,1,length = 5), decreasing = TRUE)),
                    values = seq(-1,1,length = 5))

        incProgress(1/4)
      }
    )
  })

  ### Grafico PCA
  pca_proj_obj <- reactive({
    req(input$div_pca)
    req(db_plt())
    temp <- pca_crim(data = db_plt(),
                     years = unique(data.table::year(db_plt()[,FECHA])),
                     id_col = "NOM_DIV1",
                     date_col = "FECHA",
                     tz_col = "RANG_DIA",
                     val_col = "Numero Hechos")
    proj <- proj_pca(x = temp$pca_result,
                     year_base = min(as.numeric(names(temp$pca_result))),
                     n_comp = 4)
    n_axis <- temp$axis_names
    list(proj = proj, n_axis = n_axis)
  })
  output$plot_6 <- plotly::renderPlotly({
    req(pca_proj_obj())
    crime_evol(pca_proj_obj()$proj, input$div_pca, pca_proj_obj()$n_axis)
  })

  ## Funcionalidad terciaria - Input de usuario
  # Localizador GPS - solo funciona en navegador
  observeEvent(input$locate,{
    leafletProxy("map_user") %>%
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
  observeEvent(input$map_user_gps_located,{
    u_coord <- input$map_user_gps_located$coordinates
    rv$report_location <- as.numeric(u_coord)
    leafletProxy("map_user") %>%
      clearMarkers() %>%
      addMarkers(lat = u_coord$lat, lng = u_coord$lng)
  })

  # Boton emergente para guardar datos
  output$save_button <- renderUI({
    req(input$map_user_click)
    actionButton(inputId = "save_button",
                               label = "¡Reportar!",
                               class = "btn-primary")
  })
  # Observador del evento de click
  observeEvent(input$map_user_click,{
    click = input$map_user_click
    rv$report_location <- as.numeric(click[-3])
    leafletProxy('map_user') %>%
      clearMarkers() %>%
      addMarkers(
        lng = click$lng,
        lat = click$lat
      )
    if(!is.null(input$map_user_gps_located)){
      leafletProxy("map_user") %>%
        leaflet.extras::deactivateGPS()
    }
    shinyjs::show('save_button')
  })
  ## Funcionalidad cuaternaria - Forecast
  # Reactivos para graficas
  #fore <- reactiveValues(data = NULL, sf_obj = NULL, plot = NULL)
  # Observador del boton
  #observeEvent(input$send4,{
  #  fore$data <- data.table::as.data.table(get(db_call(input$city_sel4,
  #                                                     input$crime4)))
  #  fore$sp_obj <- test2021:::city_selector(input$city_sel4,'RAW')
  #  result <- fore$data
  #  if(!inherits(result$FECHA,"POSIXct")){
  #    result$FECHA <- as.POSIXct(paste0(result$FECHA,"-01"))
  #  }
  #  sp_filter <- fore$sp_obj@data[fore$sp_obj$NOMBRE_DIV1 == input$units4,
  #                                'CODIGO_DIV1'][1]
  #  db_agg <- result[CODIGO_DIV1 == sp_filter, .N, by = FECHA]
  #  db_agg <- db_agg[order(db_agg$FECHA),]
  #  db_agg_filt <- db_agg[seq(nrow(db_agg)-7,nrow(db_agg)),]
  #  split_ts <- create_split_ts(Y = db_agg$N,
  #                              X = subset(db_agg, select = -N),
  #                              split_prop = c(0.8,0.2,0))
  #  fit <- arima_mod_selector(split_ts, quiet = TRUE)
  #  dates_refit <- as.Date("2013-03-27")+
  #    0:(length(split_ts$Y$Y_train)+length(split_ts$Y$Y_val)-1)
  #  Y_tibble <- dplyr::tibble(
  #    date = dates_refit[(length(dates_refit)-length(split_ts$Y$Y_val)+1):
  #                         length(dates_refit)],
  #    value = split_ts$Y$Y_val
  #  )
  #  Y_tsibble <- tsibble::as_tsibble(Y_tibble)
  #  fit_fore <- fit$model %>%
  #    fabletools::refit(Y_tsibble, reestimate = FALSE) %>%
  #    fabletools::forecast(h = "1 week")
  #  fit_fore <- fit_fore[,c(2,4)]
  #  names(fit_fore) <- c("FECHA","N")
  #  time_delta <- ifelse(input$city_sel4 == "Londres","1 month","1 day")
  #  fit_fore$FECHA <- seq(max(db_agg$FECHA),
  #                       length = nrow(fit_fore)+1, by = time_delta)[-1]
  #  p <- ggplot2::ggplot() +
  #    ggplot2::geom_line(data = db_agg_filt,
  #                       ggplot2::aes(x = FECHA, y = N),
  #                       color = '#00AFBB', size = 1) +
  #    ggplot2::geom_point(data = db_agg_filt,
  #                        ggplot2::aes(x = FECHA, y = N),
  #                        color = 'orange') +
  #    ggplot2::geom_line(data = fit_fore,
  #                       ggplot2::aes(x = FECHA, y = N),
  #                       color = 'darkblue') +
  #    ggplot2::geom_point(data = fit_fore,
  #                       ggplot2::aes(x = FECHA, y = N),
  #                       color = 'orange') +
  #    ggplot2::theme_minimal(base_size = 12)

  #  fore$plot <- plotly::ggplotly(p) %>%
  #    plotly::config(displayModeBar = F, locale = "es")
  #})

  #output$plot_forecast <- plotly::renderPlotly({
  #  if (is.null(fore$plot)) return()
  #  fore$plot
  #})
}
