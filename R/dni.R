
#' Validate a Spanish NIF (National Identification Number)
#'
#' It checks if a given Spanish NIF (Documento Nacional de Identidad) is valid.
#' It verifies both the format (8 digits followed by an uppercase letter) and the
#' control letter, which is computed using the remainder of the division of the
#' number by 23.
#'
#' @param dni A character string representing the NIF to validate.
#' @return `TRUE` if the NIF is valid, `FALSE` otherwise.
#'
#' @keywords internal
validate_nif <- function(dni) {
  # Control letters for NIF based on modulus 23
  control_letters <- c("T", "R", "W", "A", "G", "M", "Y", "F", "P", "D",
                       "X", "B", "N", "J", "Z", "S", "Q", "V", "H", "L",
                       "C", "K", "E")

  # Regular expression to validate format: 8 digits followed by an uppercase letter
  if (!grepl("^[0-9]{8}[A-Z]$", dni)) {
    return(FALSE)  # Invalid format
  }

  # Extract number and letter
  number <- as.numeric(substr(dni, 1, 8))  # First 8 characters (number)
  letter <- substr(dni, 9, 9)               # Last character (letter)

  # Compute the correct letter
  correct_letter <- control_letters[(number %% 23) + 1]

  # Compare with the provided letter
  return(letter == correct_letter)
}


#' Validate a Spanish NIE (Foreign Identification Number)
#'
#' It checks if a given Spanish NIE (Número de Identidad de Extranjero) is valid.
#' The NIE follows the format: an initial letter (X, Y, or Z), followed by 7 digits,
#' and a control letter. The validation process involves verifying the format
#' and computing the correct control letter using the same algorithm as a NIF.
#'
#' @param nie A character string representing the NIE to validate.
#' @return `TRUE` if the NIE is valid, `FALSE` otherwise.
#'
#' @keywords internal
validate_nie <- function(nie) {
  # Validate format: must start with X, Y, or Z, followed by 7 digits and a letter
  if (!grepl("^[XYZ][0-9]{7}[A-Z]$", nie)) {
    return(FALSE)
  }

  # Convert the first letter to its numeric equivalent
  nie_numeric <- nie
  substr(nie_numeric, 1, 1) <- switch(substr(nie, 1, 1), X = "0", Y = "1", Z = "2")

  # Validate the NIE as if it were a NIF
  return(validate_nif(nie_numeric))
}


#' Validar y avisar de NIF o NIE no válido
#'
#' Comprueba si un determinado NIF o NIE es válido. Si no es válido se emite un
#' aviso especificando la ubicación.
#'
#' @param dni Una cadena de caracteres que representa el NIF o NIE a validar.
#' @param ubicacion Una cadena de caracteres que indica la ubicación o contexto en el que se
#' se está realizando la validación. Esta información está incluida en el mensaje de advertencia.
#'
#' @return Siempre se devuelve `TRUE`, pero se emite un aviso si el NIF o NIE no es válido.
#'
#' @keywords internal
validar_nif_nie <- function(dni, ubicacion){
  if (!(validate_nif(dni) || validate_nie(dni))) {
    warning(ubicacion, " -> El NIF/NIE '",
         dni,
         "' no es válido.")
  }
  TRUE
}


