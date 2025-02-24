
#' Validar una solicitud
#'
#' Esta función verifica que las solicitudes contengan todos los campos requeridos,
#' asegurando que los datos sean completos antes de su procesamiento.
#'
#' @param solicitud Un `data.frame` que contiene los datos de la solicitud.
#'                  Debe incluir al menos la columna `codigoEstablecimiento`.
#'
#' @return Devuelve `TRUE` si la validación se completa sin errores. Si falta
#'         algún campo requerido, genera advertencias (`warning`).
#'
#' @keywords internal
validar_solicitud <- function(solicitud) {

  df <- solicitud

  for (i in 1:nrow(df)) {
    ubicacion <- sprintf('solicitud (fila %d)', i)
    if (is.na(df$codigoEstablecimiento[i])) {
      warning(ubicacion,
              " -> Falta el campo 'codigoEstablecimiento'.")
    }
  }
  TRUE
}
