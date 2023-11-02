#' @encoding UTF-8
#' @title Creación de tablas en formato wide para mapas de delito
#'
#' @description La intención de esta función es asistir en la representación
#'     de los conjuntos de datos de delitos que originalmente son presentados
#'     como número de ocurrencias por fecha y unidad territorial
#'     (e.g. Localidad, UPZ), agregándolos en los grupos inducidos por las
#'     variables temporales/espaciales de acuerdo a un patrón como
#'     meses/división territorial uno o similares. Puede ser usado con
#'     otro tipo de datos pero se necesita pasar parámetros adicionales
#'
#' @param x El conjunto a procesar. Se admite que los datos ya hayan sido
#'     agrupados y agregados previamente, pero se recomienda incluir
#'     el parámetro adicional \code{pre_pro = FALSE}
#' @param date_range Un vector con la fecha inicial y final. Se sugiere usar
#'     un formato como "aaaa-mm-dd". El orden de las fechas debe mantenerse
#' @param div_name El nombre de la columna que contiene el nombre de las
#'     divisiones espaciales, o sobre la cual se consolidan los grupos (más
#'     las fechas)
#' @param var_agg El nombre de la columna que contienen los valores a ser
#'     agregados
#' @param ... Argumentos adicionales. La columna de fechas se asume con el
#'     nombre "FECHA" (sin distinción de mayúsculas), en caso contrario
#'     incluir el argumento \code{date_col_name} con el nombre correspondiente.
#'     Se realiza una agregación de las fechas por meses, si se desea usar
#'     otra unidad como semanas o días se debe pasar la función que haga este
#'     procedimiento con el argumento \code{date_agg_fun}
#'
#' @return Un \code{data.frame} con formato wide
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom dplyr summarise
#'
#' @examples
#' # Usamos la base de datos de Bogota, filtrando por robo de celulares
#' x <- siedco_wc[Delito == "HURTO DE CELULARES",,]
#' periodo <- c("2019-03-01","2020-12-30")
#' ctab_period(x, date_range = periodo, div_name = "NOM_DIV1",
#' var_agg = "Numero Hechos")
ctab_period <- function(x, date_range, div_name, var_agg = "N", ...){
  # Parametros globlales. Sobreescritos por ...
  pre_pro <- TRUE
  date_col_name <- "FECHA"
  date_agg_fun <- data.table::month
  date_range <- as.Date(date_range)
  # Dots
  list2env(list(...), envir = environment())
  # Checks
  if(!is.function(date_agg_fun)) stop("date_agg_fun debe ser funcional")
  if(date_range[2] <= date_range[1]) stop("Rango de fechas incorrecto")
  # PROC
  if(!inherits(x, "data.table")) x <- as.data.table(x)
  date_col_name <- grep(date_col_name, names(x), value = TRUE, ignore.case = TRUE)[1]
  if(is.null(date_col_name)) stop("Columna de fecha ausente en los datos")
  x_alter <- x[FECHA >= date_range[1] & FECHA <= date_range[2],,]
  if(!pre_pro & ncol(x_alter)>3){
    warning("Numero de columnas superior a lo esperado (3). Agregando...")
    pre_pro <- TRUE
  }
  if(pre_pro){
    x_alter <- x_alter[,.(N=sum(eval(as.name(var_agg)))),
                       .(eval(as.name(div_name)),eval(as.name(date_col_name)))]
    names(x_alter) <- c(div_name, date_col_name, var_agg)
  }
  names(x_alter)[names(x_alter)==var_agg] <- "N"
  x_alter
  x_alter %>%
    tidyr::pivot_wider(id_cols = !!as.name(date_col_name),
                       names_from = !!as.name(div_name),
                       values_from = N) %>%
    replace(is.na(.),0) %>%
    dplyr::mutate(F_ALTER = paste(data.table::year(!!as.name(date_col_name)),
                                  date_agg_fun(!!as.name(date_col_name))),
                  !!date_col_name := NULL) %>%
    dplyr::group_by(F_ALTER) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),sum)) %>%
    subset(select = - F_ALTER)
}

#' @encoding UTF-8
#' @title Aplicar rezagos a tablas en formato wide
#'
#' @description En principio la función se dirige a \code{data.frames}
#'     provenientes del proceso \code{ctab_period} aunque funciona para
#'     cualquier tipo de conjunto que cumpla con poseer un formato wide
#'     y no tenga otras columnas diferentes a las expandidas
#'
#' @param x El \code{data.frame} a tratar
#' @param selection El nombre de la columna a usar como referencia. Sobre ella
#'     no se aplican los rezagos
#' @param n_lags Entero. La cantidad de rezagos a usar en las variables
#'
#' @return El conjunto de datos con las variables rezagadas. Se eliminan las
#'     filas donde las variables rezagadas no tienen observaciones
#' @export
#'
#' @examples
#' # Usamos la base de datos de Bogota, filtrando por robo de celulares
#' x <- siedco_wc[Delito == "HURTO DE CELULARES",,]
#' periodo <- c("2019-03-01","2020-12-30")
#' ejemplo <- ctab_period(x, date_range = periodo, div_name = "NOM_DIV1",
#' var_agg = "Numero Hechos")
#' # Rezago de dos meses para PUENTE ARANDA
#' ctab_lag(ejemplo, "PUENTE ARANDA", 2)
ctab_lag <- function(x, selection, n_lags = 1){
  if(!selection %in% names(x))
    stop("La seleccion de nombre no se encuentra en la tabla")
  x[,names(x)!= selection] <- lapply(x[,names(x)!= selection], lagpad, k = n_lags)
  x[complete.cases(x),]
}

#' @encoding UTF-8
#' @title Gráfico de correlación sobre datos en formatos wide
#'
#' @description Se usa un \code{data.frame} en formato \code{wide} de datos
#'     numéricos clasificados por columnas (variables) y filas
#'     (individuos/tiempos). El gráfico es procesado usando \code{corrplot}
#'
#' @param x Un \code{data.frame} en formato \code{wide}
#' @param selection Carácter. El nombre usado para filtrar la visualización
#' @param n_show Entero positivo. Cantidad de variables a mostrar más la
#'     seleccionada
#' @param cor_method Carácter. Método usado para el cálculo de la matriz de
#'     correlaciones
#' @param ... Argumentos adicionales
#'
#' @return Una gráfica de correlación
#' @export
ctab_corplot <- function(x, selection = NULL, n_show = NULL,
                         cor_method = c("kendall","pearson","spearman"),
                         ...){
  # Parametros globlales. Sobreescritos por ...
  tl.cex <- 1
  if(is.null(n_show)){
    tl.cex <- 0.8
  }else if(n_show > 8){
    tl.cex <- 0.8
  }
  # Checks
  if(!is.null(selection))
    if(!any(selection %in% names(x)))
      stop("Seleccion no se encuentra entre los nombre del conjunto de datos")
  # PROC
  cor_mat <- cor(x, method = cor_method)
  if(!is.null(selection) & !is.null(n_show)){
    top <- names(head(sort(abs(cor_mat[selection,]),decreasing = TRUE),n_show+1))
    cor_mat <- cor_mat[top,top]
  }
  blue2red <- grDevices::colorRampPalette(c('blue','cyan','white','orange','red'))
  cor_mat %>%
    corrplot::corrplot(order = 'FPC',
                       type = 'lower', diag = FALSE, tl.col = "black",
                       cl.ratio = 0.2, tl.srt = 45, tl.cex = tl.cex,
                       col = blue2red(25), is.corr = TRUE)
}
