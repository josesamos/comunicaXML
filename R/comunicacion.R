#'
#' @keywords internal
validar_comunicacion <- function(comunicacion) {
  pk <- 'comunicacion_pk'
  if (!pk %in% names(comunicacion)) {
    warning("La columna 'comunicacion_pk' no existe en la hoja 'comunicacion'.")
  } else {
    valores <- comunicacion[[pk]]
    df <- comunicacion

    if (!(all(!is.na(valores)) && length(unique(valores)) == nrow(df))) {
      warning("La columna 'comunicacion_pk' en la hoja 'comunicacion' no puede tener valores NA o duplicados.")
    }
  }
  TRUE
}

#'
#' @keywords internal
validar_referencia <- function(comunicacion, otra, nombre) {
  pk <- 'comunicacion_pk'
  fk <- 'comunicacion_fk'
  if (!fk %in% names(otra)) {
    stop("La columna 'comunicacion_fk' no existe en la hoja '",
         nombre,
         "'.")
  }

  valores <- otra[[fk]]
  com <- comunicacion[[pk]]
  if (!all(valores %in% com)) {
    stop(
      "Todos los valores de la columna 'comunicacion_fk' en la hoja '",
      nombre,
      "' han de estar en la columna 'comunicacion_pk' de la hoja 'comunicacion'."
    )
  }
  TRUE
}
