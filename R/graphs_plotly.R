#' @encoding UTF-8
#' @title Procesamiento de datos de delitos para análisis de componentes
#'     principales
#'
#' @description Función dirigida a modularización de las Shiny App.
#'
#' @param data Un \code{data.frame} de delito previamente filtrado
#' @param id_col Carácter. Nombre de la columna que contiene los nombres
#'     de las divisiones espaciales de ínteres
#' @param date_col Carácter. Nombre de la columna que contiene las fechas
#' @param tz_col Carácter. Nombre de la columna que contiene las zonas
#'     horarias del día. Usualmente es "RANG_DIA"
#' @param val_col Carácter. Nombre de la columna que contiene la variable
#'     numérica que se necesita agregar por fechas y divisiones espaciales
#'
#' @return Un \code{data.frame} procesado ideal para ACP
#' @export
#' @importFrom dplyr mutate
#' @importFrom data.table wday
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr inner_join
#'
#' @examples
#' # Aumentar los datos de siedco para ACP
#' pca_crime_pre(data = test2021::siedco_wc[Delito == "HURTO DE CELULARES",],
#' id_col = "NOM_DIV1",
#' date_col = "FECHA",
#' tz_col = "RANG_DIA",
#' val_col = "Numero Hechos")
pca_crime_pre <- function(data, id_col, date_col, tz_col, val_col){
  base_df <- data %>%
    dplyr::mutate(W_DAY = data.table::wday(!!as.name(date_col)))
  # Primer conjunto de datos
  b_1 <- base_df %>%
    dplyr::select(!!as.name(val_col),!!as.name(id_col),W_DAY) %>%
    dplyr::group_by(!!as.name(id_col), W_DAY) %>%
    dplyr::summarise(N = sum(!!as.name(val_col)), .groups = "drop") %>%
    dplyr::mutate(W_DAY = dplyr::case_when(W_DAY == 1 ~ "DOM",
                                           W_DAY == 2 ~ "LUN",
                                           W_DAY == 3 ~ "MAR",
                                           W_DAY == 4 ~ "MIE",
                                           W_DAY == 5 ~ "JUE",
                                           W_DAY == 6 ~ "VIE",
                                           TRUE ~ "SAB")) %>%
    tidyr::pivot_wider(id_cols = !!as.name(id_col), names_from = W_DAY,
                       names_prefix = "DIA_",values_from = N) %>%
    replace(is.na(.),0)
  # Segundo conjunto de datos
  b_2 <- base_df %>%
    dplyr::mutate(tz_col = gsub("\u00D1","N",!!as.name(tz_col))) %>%
    dplyr::select(!!as.name(val_col), !!as.name(id_col), !!as.name(tz_col)) %>%
    dplyr::group_by(!!as.name(id_col), !!as.name(tz_col)) %>%
    dplyr::summarise(N = sum(!!as.name(val_col)), .groups = "drop") %>%
    tidyr::pivot_wider(id_cols = !!as.name(id_col),
                       names_from = !!as.name(tz_col),
                       values_from = N) %>%
    replace(is.na(.),0)
  # Tercer conjunto de datos
  b_3 <- base_df %>%
    dplyr::select(!!as.name(val_col),!!as.name(id_col)) %>%
    dplyr::group_by(!!as.name(id_col)) %>%
    dplyr::summarise(N = sum(!!as.name(val_col)))
  # Cuarto conjunto de datos
  b_4 <- base_df %>%
    dplyr::mutate(MONTH  = data.table::month(!!as.name(date_col))) %>%
    dplyr::select(!!as.name(val_col),!!as.name(id_col), MONTH) %>%
    dplyr::group_by(!!as.name(id_col), MONTH) %>%
    dplyr::summarise(N = sum(!!as.name(val_col)), .groups = "drop") %>%
    tidyr::pivot_wider(id_cols = !!as.name(id_col), names_from = MONTH,
                       names_prefix = "MES_",values_from = N) %>%
    replace(is.na(.),0)
  # Retorno de lista
  b_merged <-  b_3 %>%
    dplyr::inner_join(b_2, by = id_col) %>%
    dplyr::inner_join(b_1, by = id_col)
  if(nrow(b_merged)>(ncol(b_merged)+ncol(b_4)-1)){
    b_merged <- b_merged %>%
      dplyr::inner_join(b_4, by = id_col)
  }
  #
  b_merged <- b_merged %>%
    as.data.frame()
  return(b_merged)
}


#' @encoding UTF-8
#' @title Análisis de componentes principales por años - Delitos
#'
#' @description Función dirigida a modularización de las Shiny App.
#'
#' @param data Un \code{data.frame} de delito previamente filtrado
#' @param years Los años que deben ser incluidos en el análisis
#' @param id_col Carácter. Nombre de la columna que contiene los nombres
#'     de las divisiones espaciales de ínteres
#' @param date_col Carácter. Nombre de la columna que contiene las fechas
#' @param tz_col Carácter. Nombre de la columna que contiene las zonas
#'     horarias del día. Usualmente es "RANG_DIA"
#' @param val_col Carácter. Nombre de la columna que contiene la variable
#'     numérica que se necesita agregar por fechas y divisiones espaciales
#'
#' @return Dos listas: \code{pca_result} contiene los ACP listados por
#'     año y \code{axis_names} una sugerencia de nombre para los dos
#'     primeros componentes
#' @export
#' @importFrom data.table year
#' @importFrom dplyr slice_max
#' @importFrom stats princomp
#'
#' @examples
#' objeto <- pca_crim(data = test2021::siedco_wc[Delito == "HURTO DE CELULARES",],
#' years = unique(data.table::year(test2021::siedco_wc[Delito == "HURTO DE CELULARES",FECHA])),
#' id_col = "NOM_DIV1",
#' date_col = "FECHA",
#' tz_col = "RANG_DIA",
#' val_col = "Numero Hechos")
pca_crim <- function(data, years, id_col, date_col, tz_col, val_col){
  data$year <- data.table::year(data[[date_col]])
  if(!all(years %in% unique(data$year))) stop("Some years are outside the scope")
  #
  tmp_names <- function(x, y, z){
    x %>%
      as.data.frame() %>%
      dplyr::slice_max(order_by = !!as.name(y), n = z) %>%
      rownames() %>%
      gsub("_"," ",.) %>%
      gsub("[[:digit:]]+", "", .) %>%
      unique() %>%
      paste(collapse = "+")
  }
  #
  pca_results <- list()
  axis_names <- list()
  for(i in years){
    data_f <- data %>% dplyr::filter(year == as.numeric(i))
    pre_obj <- pca_crime_pre(data = data_f,
                             id_col = id_col,
                             date_col = date_col,
                             tz_col = tz_col,
                             val_col = val_col)
    rownames(pre_obj) <- pre_obj[,1]
    if(nrow(data_f) < ncol(data_f)) next
    pca_results[[as.character(i)]] <- stats::princomp(pre_obj[,-c(1,14)], cor = TRUE)
    ### Esto es lo necesario para extraer los nombres
    X <- scale(pre_obj[,-c(1,14)])
    dist2_var <- as.vector(crossprod(rep(1, nrow(X)), as.matrix(X^2)))
    tmp <- svd(X)
    eig <- tmp$d^2
    V <- tmp$v
    U <- tmp$u
    coord_var <- t(t(as.matrix(V)) * sqrt(eig))
    contrib_var <- t(t(coord_var^2)/eig)
    cor_var <- coord_var/sqrt(dist2_var)
    cos2_var <- cor_var^2
    rownames(cos2_var)  <- rownames(contrib_var) <- colnames(X)
    colnames(cos2_var)  <- colnames(contrib_var) <-
      paste("Coord",c(1:ncol(V)), sep = ".")
    f_cos <- rowSums(cos2_var[,1:2])>0.7
    contrib_var <- contrib_var[f_cos,]
    axis_names[[as.character(i)]] <-
      sapply(c("Coord.1","Coord.2"),function(y) tmp_names(contrib_var,y,3))
  }
  return(list(pca_result = pca_results, axis_names = axis_names))
}


#' @encoding UTF-8
#' @title Proyecciones de ACP respecto al primer año - Delitos
#'
#' @description Función dirigida a modularización de las Shiny App.
#'
#' @param x Una lista con nombres (años) que contiene los resultados
#'     de ACP para cada año
#' @param year_base El año tomado como base o inicial. Por defecto
#'     se usa el menor año
#' @param n_comp El total de componentes principales a usar en la
#'     proyección
#'
#' @return Una lista con proyecciones de las matrices sobre el primer año
#' @export
#'
#' @examples
#' objeto <- pca_crim(data = test2021::siedco_wc[Delito == "HURTO DE CELULARES",],
#' years = unique(data.table::year(test2021::siedco_wc[Delito == "HURTO DE CELULARES",FECHA])),
#' id_col = "NOM_DIV1",
#' date_col = "FECHA",
#' tz_col = "RANG_DIA",
#' val_col = "Numero Hechos")
#' objeto_2 <- proj_pca(x = objeto$pca_result, year_base = 2012, n_comp = 4)
proj_pca <- function(x, year_base, n_comp){
  if(!year_base %in% names(x)) stop("Selected year isn't present")
  #
  x <- lapply(x, function(x) x$scores)
  #
  proj_list <- list()
  proj_list[[as.character(year_base)]] <-
    x[[as.character(year_base)]][,1:n_comp]
  for (year in names(x)[names(x)!=year_base]) {
    model <-lm(proj_list[[as.character(year_base)]]~.,
               data = as.data.frame(x[[year]][,1:n_comp]))
    proj_list[[year]] <- fitted(model)
  }
  return(proj_list)
}


#' @encoding UTF-8
#' @title Gráfico de proyecciones ACP delitos
#'
#' @param pca_list Una lista con salidas de princomp
#' @param selection Carácter. Nombre de la unidad seleccionada
#' @param axis_names Dos carácteres que represetan los nombres de los ejes
#'
#' @return Un gráfico de plotly
#' @export
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly add_markers
#'
#' @examples
#' # Obtencion de los pca por localidad
#' objeto <- pca_crim(data = test2021::siedco_wc[Delito == "HURTO DE CELULARES",],
#' years = unique(data.table::year(test2021::siedco_wc[Delito == "HURTO DE CELULARES",FECHA])),
#' id_col = "NOM_DIV1",
#' date_col = "FECHA",
#' tz_col = "RANG_DIA",
#' val_col = "Numero Hechos")
#' # Projecciones sobre 2012
#' objeto_2 <- proj_pca(x = objeto$pca_result, year_base = 2012, n_comp = 4)
#' # Grafico
#' crime_evol(objeto_2,"PUENTE ARANDA",objeto$axis_names)
crime_evol <- function(pca_list, selection, axis_names = NULL){
  #
  indx <- sort(names(pca_list))
  pca_list <- lapply(pca_list, function(x) x[,1:2] %>%
                       as.data.frame() %>%
                       dplyr::mutate(Location = rownames(.)))
  # PAR GRAFICOS
  l_w <- rep(1, length(indx))
  l_col <- rep("blue",length(indx))
  #pal <- grDevices::colorRampPalette(c('cyan','white','orange'))
  xaxis <- list(title = ifelse(is.null(axis_names),"Componente 1",
                               axis_names[[indx[1]]][1]))
  yaxis <- list(title = ifelse(is.null(axis_names),"Componente 2",
                               axis_names[[indx[1]]][2]))
  #
  p <- pca_list[[indx[1]]] %>%
    plotly::plot_ly(x=~Comp.1, y=~Comp.2,
                    text = ~Location, type = "scatter",
                    marker = list(size = 5,
                                  color = "rgba(176, 196, 222, 0.4)",
                                  line = list(color = "rgba(152, 0, 0, .8)",
                                              width = 1)),
                    hovertemplate =
                      paste0("%{text}: ",indx[1],"<extra></extra>")) %>%
    plotly::add_markers(inherit = FALSE,
                        x=~Comp.1, y=~Comp.2,
                        text = ~Location,
                        type = "scatter",
                        marker = list(color = "rgb(30, 144, 255)",
                                      size = 7),
                        data = pca_list[[indx[length(indx)]]],
                        hovertemplate =
                          paste0("%{text}: ",indx[length(indx)],"<extra></extra>"))
  #
  sub_set <- lapply(pca_list, function(x) x[x$Location == selection,1:2])
  sub_set <- dplyr::bind_rows(sub_set)
  sub_set <- sub_set[order(names(pca_list)),]
  anno_set <- data.frame(x = sub_set[-nrow(sub_set),1],
                         y = sub_set[-nrow(sub_set),2],
                         endX = sub_set[-1,1],
                         endY = sub_set[-1,2])
  #
  p <- plotly::add_annotations(p,
                               inherit = FALSE,
                               data = anno_set,
                               text = "",
                               showarrow = TRUE,
                               xref = "x", axref = "x",
                               yref = "y", ayref = "y",
                               x = ~endX,
                               ax = ~x,
                               y = ~endY,
                               ay = ~y,
                               arrowhead = 4,
                               arrowsize = 0.7,
                               arrowcolor = "cyan") %>%
    plotly::add_markers(x = sub_set[-1,1],
                        y = sub_set[-1,2],
                        text = indx[-1],
                        hovertemplate = paste0(selection,": %{text} <extra></extra>"),
                        mode = "scatter",
                        marker = list(size = 0.5, color = "cyan"),
                        inherit = FALSE) %>%
    plotly::layout(showlegend = FALSE,
                   title = paste0("Movimiento desde ",indx[1]),
                   xaxis = xaxis,
                   yaxis = yaxis)
  p
}


