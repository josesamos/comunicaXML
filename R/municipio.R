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
    warning(ubicacion, " -> El c\u00f3digo de pa\u00eds '",
         pais,
         "' no es v\u00e1lido. Consulta los c\u00f3digos mediante 'View(iso_codes)'.")
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
    warning(ubicacion, " -> El c\u00f3digo de municipio '",
         codigo_municipio,
         "' no es v\u00e1lido. Consulta los c\u00f3digos mediante 'View(municipios)'.")
  }
  TRUE
}


#' Validate a Spanish Postal Code
#'
#' Checks if a given postal code is valid in Spain. It must be a 5-digit numeric string
#' where the first two digits correspond to valid Spanish provinces (01 to 52).
#'
#' @param codigo_postal A character string representing the postal code to validate.
#' @return `TRUE` if the postal code is valid, `FALSE` otherwise.
#'
#' @keywords internal
validar_codigo_postal <- function(codigo_postal) {
  # Ensure it is a character string of exactly 5 digits
  if (!grepl("^[0-9]{5}$", codigo_postal)) {
    return(FALSE)
  }

  # Extract the first two digits (province code)
  provincia <- as.numeric(substr(codigo_postal, 1, 2))

  # Valid province codes in Spain (from 01 to 52)
  if (provincia < 1 || provincia > 52) {
    return(FALSE)
  }

  TRUE
}
