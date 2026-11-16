#' Resumen descriptivo de una variable numérica
#'
#' Calcula el tamaño muestral válido, media, desviación estándar, mediana
#' y rango intercuartílico de un vector numérico.
#'
#' @param x Vector numérico con los datos a analizar.
#'
#' @return Un data.frame de una fila con los estadísticos principales.
#' @export
#'
#' @examples
#' resumen_numerico(c(21, 24, 28, 35, 42, NA))
resumen_numerico <- function(x) {
  if (!is.numeric(x)) {
    stop("El argumento 'x' debe ser un vector numérico.")
  }

  data.frame(
    n       = sum(!is.na(x)),
    media   = round(mean(x, na.rm = TRUE), 2),
    sd      = round(stats::sd(x, na.rm = TRUE), 2),
    mediana = round(stats::median(x, na.rm = TRUE), 2),
    iqr     = round(stats::IQR(x, na.rm = TRUE), 2)
  )
}
