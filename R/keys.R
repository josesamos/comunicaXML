#' Validar columna de clave principal
#'
#' Comprueba si una tabla contiene una columna de clave primaria con el formato correcto.
#' La columna de clave principal debe existir, no debe faltar ningún valor ("NA")
#' y contener valores únicos.
#'
#' @param table Un data frame que representa la tabla a validar.
#' @param name Una cadena de caracteres que indica el nombre de la tabla.
#'
#' @return `TRUE`. Genera advertencias si falta la columna de clave principal, contiene valores "NA",
#' o tiene duplicados.
#'
#' @keywords internal
validate_pk <- function(table, name) {
  pk <- paste0(name, '_pk')
  if (!pk %in% names(table)) {
    warning("La columna '", pk, "' no existe en la hoja '", name, "'.")
  } else {
    valores <- table[[pk]]
    if (!(all(!is.na(valores)) &&
          length(unique(valores)) == nrow(table))) {
      warning("La columna '",
              pk,
              "' en la hoja '",
              name,
              "' no puede tener valores NA o duplicados.")
    }
  }
  TRUE
}

#' Validar columna de clave externa
#'
#' Comprueba si existe una columna de clave externa en la tabla de referencia y garantiza
#' que todos los valores de la columna de clave externa coincidan con los valores existentes
#' en la columna de clave principal de la tabla a la que se hace referencia.
#'
#' @param table_pk Un data frame que representa la tabla que contiene la clave principal.
#' @param name_pk Una cadena de caracteres que indica el nombre de la tabla a la que se hace
#' referencia.
#' @param table_fk Un data frame que representa la tabla que contiene la clave externa.
#' @param name_fk Una cadena de caracteres que indica el nombre de la tabla de referencia.
#'
#' @return `TRUE`. Genera advertencias si falta la columna de clave externa en la
#' tabla de referencia o si contiene valores que no están presentes en la columna de
#' clave principal de la tabla a la que se hace referencia.
#'
#' @keywords internal
validate_fk <- function(table_pk, name_pk, table_fk, name_fk) {
  pk <- paste0(name_pk, '_pk')
  fk <- paste0(name_pk, '_fk')
  if (!(fk %in% names(table_fk))) {
    warning("La columna '", fk, "' no existe en la hoja '", name_fk, "'.")
  } else {
    values <- table_fk[[fk]]
    original <- table_pk[[pk]]
    if (!all(values %in% original)) {
      warning(
        "Todos los valores de la columna '",
        fk,
        "' en la hoja '",
        name_fk,
        "' han de estar en la columna '",
        pk,
        "' de la hoja '",
        name_pk,
        "'."
      )
    }
  }
  TRUE
}
