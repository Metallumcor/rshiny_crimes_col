#' @title Exported function from classInt
#' @export
#' @param var Variable a mapear a intervalos
#' @param n Cantidad de intervalos
#' @param style Tipo de intervalo
#' @param rtimes Ver documentacion de \code{classInt}
#' @param ... argumentos adicionales
#' @param intervalClosure Donde cierra el intervalo
#' @param dataPrecision Ver documentacion de \code{classInt}
#' @param warnSmallN Ver documentacion de \code{classInt}
#' @param warnLargeN Ver documentacion de \code{classInt}
#' @param largeN Ver documentacion de \code{classInt}
#' @param samp_prop Ver documentacion de \code{classInt}
#' @param gr Ver documentacion de \code{classInt}
#' @importFrom stats hclust
#' @importFrom stats kmeans
#' @importFrom stats quantile
#' @importFrom stats cutree
#' @importFrom e1071 bclust
classIntervals <- function (var, n, style = "quantile", rtimes = 3, ..., intervalClosure = c("left",
                                                                                             "right"), dataPrecision = NULL, warnSmallN = TRUE, warnLargeN = TRUE,
                            largeN = 3000L, samp_prop = 0.1, gr = c("[", "]"))
{
  if (is.factor(var))
    stop("var is categorical")
  TZ <- NULL
  POSIX <- FALSE
  DATE <- FALSE
  if (!is.numeric(var)) {
    if (inherits(var, "POSIXt")) {
      TZ <- attr(var, "tzone")
      POSIX <- TRUE
      var <- unclass(as.POSIXct(var))
    }
    else if (inherits(var, "Date")) {
      var <- unclass(var)
      DATE <- TRUE
    }
    else {
      stop("var is not numeric")
    }
  }
  UNITS <- NULL
  if (inherits(var, "units")) {
    UNITS <- paste0(gr[1], as.character(attr(var, "units")),
                    gr[2])
  }
  intervalClosure <- match.arg(intervalClosure)
  ovar <- var
  if (length(style) > 1L)
    style <- style[1L]
  if (any(is.na(var))) {
    warning("var has missing values, omitted in finding classes")
    var <- c(na.omit(var))
  }
  if (any(!is.finite(var))) {
    warning("var has infinite values, omitted in finding classes")
    is.na(var) <- !is.finite(var)
  }
  nobs <- length(unique(var))
  if (nobs == 1)
    stop("single unique value")
  needn <- !(style %in% c("dpih", "headtails"))
  if (missing(n))
    n <- nclass.Sturges(var)
  if (n < 2 & needn)
    stop("n less than 2")
  n <- as.integer(n)
  pars <- NULL
  if (n > nobs & needn) {
    if (warnSmallN) {
      warning(paste("n greater than number of different finite values",
                    "n reset to number of different finite values",
                    sep = "\\n"))
    }
    n <- nobs
  }
  if (n == nobs & needn) {
    if (warnSmallN) {
      warning(paste("n same as number of different finite values",
                    "each different finite value is a separate class",
                    sep = "\\n"))
    }
    sVar <- sort(unique(var))
    dsVar <- diff(sVar)
    brks <- c(sVar[1] - (mean(dsVar)/2), sVar[1:(length(sVar) -
                                                   1)] + (dsVar/2), sVar[length(sVar)] + (mean(dsVar)/2))
    style = "unique"
  }
  else {
    sampling <- FALSE
    if (warnLargeN && (style %in% c("kmeans", "hclust",
                                    "bclust", "jenks"))) {
      if (nobs > largeN) {
        warning("N is large, and some styles will run very slowly; sampling imposed")
        sampling <- TRUE
        nsamp <- ifelse(samp_prop * nobs > 3000, as.integer(ceiling(samp_prop *
                                                                      nobs)), 3000L)
      }
    }
    if (style == "fixed") {
      dots <- list(...)
      fixedBreaks <- sort(dots$fixedBreaks)
      if (is.null(fixedBreaks))
        stop("fixed method requires fixedBreaks argument")
      if (!is.numeric(fixedBreaks)) {
        if (inherits(fixedBreaks, "POSIXt") && POSIX) {
          fixedBreaks <- unclass(as.POSIXct(fixedBreaks))
        }
        else if (inherits(fixedBreaks, "DATE") && DATE) {
          fixedBreaks <- unclass(fixedBreaks)
        }
        else {
          stop("fixedBreaks must be numeric")
        }
      }
      if (any(diff(fixedBreaks) < 0))
        stop("decreasing fixedBreaks found")
      if (min(var) < fixedBreaks[1] || max(var) > fixedBreaks[length(fixedBreaks)])
        warning("variable range greater than fixedBreaks")
      brks <- fixedBreaks
    }
    else if (style == "sd") {
      svar <- scale(var)
      pars <- c(attr(svar, "scaled:center"), attr(svar,
                                                  "scaled:scale"))
      names(pars) <- c("center", "scale")
      sbrks <- pretty(x = svar, n = n, ...)
      brks <- c((sbrks * pars[2]) + pars[1])
    }
    else if (style == "equal") {
      brks <- seq(min(var), max(var), length.out = (n +
                                                      1))
    }
    else if (style == "pretty") {
      brks <- c(pretty(x = var, n = n, ...))
    }
    else if (style == "quantile") {
      brks <- c(stats::quantile(x = var, probs = seq(0, 1, 1/n),
                                ...))
      names(brks) <- NULL
    }
    else if (style == "kmeans") {
      pars <- try(stats::kmeans(x = var, centers = n, ...))
      if (class(pars) == "try-error") {
        warning("jittering in kmeans")
        jvar <- jitter(rep(x = var, times = rtimes))
        pars <- try(stats::kmeans(x = jvar, centers = n, ...))
        if (class(pars) == "try-error")
          stop("kmeans failed after jittering")
        else {
          cols <- match(pars$cluster, order(c(pars$centers)))
          rbrks <- unlist(tapply(jvar, factor(cols),
                                 range))
        }
      }
      else {
        cols <- match(pars$cluster, order(c(pars$centers)))
        rbrks <- unlist(tapply(var, factor(cols), range))
      }
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    }
    else if (style == "hclust") {
      pars <- stats::hclust(dist(x = var, method = "euclidean"),
                            ...)
      rcluster <- stats::cutree(tree = pars, k = n)
      rcenters <- unlist(tapply(var, factor(rcluster),
                                mean))
      cols <- match(rcluster, order(c(rcenters)))
      rbrks <- unlist(tapply(var, factor(cols), range))
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    }
    else if (style == "bclust") {
      pars <- try(e1071::bclust(x = var, centers = n, ...))
      if (class(pars) == "try-error") {
        warning("jittering in bclust")
        jvar <- jitter(rep(x = var, times = rtimes))
        pars <- try(e1071::bclust(x = jvar, centers = n, ...))
        if (class(pars) == "try-error")
          stop("bclust failed after jittering")
        else {
          cols <- match(pars$cluster, order(c(pars$centers)))
          rbrks <- unlist(tapply(jvar, factor(cols),
                                 range))
        }
      }
      else {
        cols <- match(pars$cluster, order(c(pars$centers)))
        rbrks <- unlist(tapply(var, factor(cols), range))
      }
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    }
    else if (style == "jenks") {
      intervalClosure = "right"
      if (storage.mode(var) != "double")
        storage.mode(var) <- "double"
      if (sampling) {
        message("Use \"fisher\" instead of \"jenks\" for larger data sets")
        d <- sort(c(range(var), sample(x = var, size = nsamp)))
      }
      else {
        d <- sort(var)
      }
      k <- n
      mat1 <- matrix(1, length(d), k)
      mat2 <- matrix(0, length(d), k)
      mat2[2:length(d), 1:k] <- .Machine$double.xmax
      v <- 0
      for (l in 2:length(d)) {
        s1 = s2 = w = 0
        for (m in 1:l) {
          i3 <- l - m + 1
          val <- d[i3]
          s2 <- s2 + val * val
          s1 <- s1 + val
          w <- w + 1
          v <- s2 - (s1 * s1)/w
          i4 <- trunc(i3 - 1)
          if (i4 != 0) {
            for (j in 2:k) {
              if (mat2[l, j] >= (v + mat2[i4, j - 1])) {
                mat1[l, j] <- i3
                mat2[l, j] <- v + mat2[i4, j - 1]
              }
            }
          }
        }
        mat1[l, 1] <- 1
        mat2[l, 1] <- v
      }
      kclass <- 1:k
      kclass[k] <- length(d)
      k <- length(d)
      last <- length(d)
      for (j in length(kclass):1) {
        id <- trunc(mat1[k, j]) - 1
        kclass[j - 1] <- id
        k <- id
        last <- k - 1
      }
      brks <- d[c(1, kclass)]
    }
    else if (style == "dpih") {
      h <- dpih(var, ...)
      dots <- list(...)
      if (!is.null(dots$range.x)) {
        vmin <- dots$range.x[1]
        vmax <- dots$range.x[2]
      }
      else {
        vmin <- min(var)
        vmax <- max(var)
      }
      brks <- seq(vmin, vmax, by = h)
    }
    else if (style == "headtails") {
      dots <- list(...)
      thr <- ifelse(is.null(dots$thr), 0.4, dots$thr)
      thr <- min(1, max(0, thr))
      head <- var
      breaks <- min(var, na.rm = TRUE)
      for (i in 1:100) {
        mu <- mean(head, na.rm = TRUE)
        breaks <- c(breaks, mu)
        ntot <- length(head)
        head <- head[head > mu]
        prop <- length(head)/ntot
        keepiter <- prop <= thr & length(head) > 1
        if (isFALSE(keepiter)) {
          break
        }
      }
      brks <- sort(unique(c(breaks, max(var, na.rm = TRUE))))
    }
    else stop(paste(style, "unknown"))
  }
  if (is.null(brks))
    stop("Null breaks")
  if (POSIX) {
    ovar <- .POSIXct(ovar, TZ)
    brks <- .POSIXct(brks, TZ)
  }
  else if (DATE) {
    ovar <- as.Date(ovar, origin = "1970-01-01")
    brks <- as.Date(brks, origin = "1970-01-01")
  }
  res <- list(var = ovar, brks = brks)
  attr(res, "style") <- style
  attr(res, "parameters") <- pars
  attr(res, "nobs") <- nobs
  attr(res, "call") <- match.call()
  attr(res, "intervalClosure") <- intervalClosure
  attr(res, "dataPrecision") <- dataPrecision
  attr(res, "var_units") <- UNITS
  class(res) <- "classIntervals"
  res
}

#' @title Exported function from classInt
#' @param rbrks Interno
.rbrks <- function (rbrks)
{
  nb <- length(rbrks)
  if (nb < 2)
    stop("single break")
  brks <- c(rbrks[1], rbrks[nb])
  if (nb > 2) {
    if (nb == 3)
      brks <- append(brks, rbrks[2], 1)
    else {
      ins <- NULL
      for (i in as.integer(seq(2, (nb - 2), 2))) {
        ins <- c(ins, ((rbrks[i] + rbrks[i + 1])/2))
      }
      brks <- append(brks, ins, 1)
    }
  }
  brks
}
