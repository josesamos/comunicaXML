

#' Validar un teléfono
#'
#' Comprueba si un teléfono determinado sigue un formato válido.
#'
#' @param telefono Una cadena de caracteres que representa el teléfono a validar.
#' @param ubicacion Carácter. Una descripción de la ubicación donde se realiza la validación.
#'
#' @return `TRUE` si el correo electrónico es válido, `FALSE` en caso contrario.
#'
#' @keywords internal
validar_telefono <- function(telefono, ubicacion) {

  if (!grepl("^[0-9]{9}$", telefono)) {
    warning(ubicacion, " -> El teléfono '",
            telefono,
            "' no es v\u00e1lido. Ha de ser 9 números sin espacios.")
  }
  TRUE
}
