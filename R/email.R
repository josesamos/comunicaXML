#' Validar una dirección de correo electrónico
#'
#' Comprueba si una dirección de correo electrónico determinada sigue un formato válido.
#'
#' @param email Una cadena de caracteres que representa la dirección de correo electrónico a validar.
#' @param ubicacion Carácter. Una descripción de la ubicación donde se realiza la validación.
#'
#' @return Lógico. Siempre devuelve TRUE.
#'
#' @keywords internal
validar_email <- function(email, ubicacion) {
  patron <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

  if (!grepl(patron, email)) {
    warning(ubicacion, " -> La direcci\u00f3n de correo electr\u00f3nico '",
            email,
            "' no es v\u00e1lida.")
  }
  TRUE
}
