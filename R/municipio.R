#' Validar y advertir sobre un código de país no válido
#'
#' Comprueba si un código de país determinado es válido según la norma ISO 3166-1 alfa-3.
#' Si el código de país no es válido, se emite una advertencia especificando la ubicación.
#'
#' @param pais Una cadena de caracteres que representa el código de país ISO de tres
#' letras a validar.
#' @param ubicacion Una cadena de caracteres que indica la ubicación o contexto en el
#' que se está realizando la validación. Esta información está incluida en el mensaje de
#' advertencia.
#'
#' @return `TRUE` siempre se devuelve, pero se emite una advertencia si el código de
#' país no es válido.
#'
#' @keywords internal
validar_pais <- function(pais, ubicacion) {
  if (!(pais %in% iso_codes$alfa3)) {
    warning(ubicacion, " -> El código de país '",
         pais,
         "' no es válido. Consulta los códigos mediante 'View(iso_codes)'.")
  }
  TRUE
}

#' Validar y advertir sobre un código de municipio no válido
#'
#' Comprueba si un código de municipio determinado es válido.
#' Si el código de municipio no es válido, se emite una advertencia especificando
#' la ubicación.
#'
#' @param codigo_municipio Una cadena de caracteres que representa el código del
#' municipio a validar.
#' @param ubicacion Una cadena de caracteres que indica la ubicación o contexto
#' en el que se está realizando la validación. Esta información está incluida en
#' el mensaje de advertencia.
#'
#' @return `TRUE` siempre se devuelve, pero se emite una advertencia si el código d
#' e municipio no es válido.
#'
#' @keywords internal
validar_municipio <- function(codigo_municipio, ubicacion) {
  if (!(codigo_municipio %in% municipios$codigo)) {
    warning(ubicacion, " -> El código de municipio '",
         codigo_municipio,
         "' no es válido. Consulta los códigos mediante 'View(municipios)'.")
  }
  TRUE
}
