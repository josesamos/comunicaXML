
#' Validate a Spanish DNI (National Identification Number)
#'
#' It checks if a given Spanish DNI (Documento Nacional de Identidad) is valid.
#' It verifies both the format (8 digits followed by an uppercase letter) and the
#' control letter, which is computed using the remainder of the division of the
#' number by 23.
#'
#' @param dni A character string representing the DNI to validate.
#' @return `TRUE` if the DNI is valid, `FALSE` otherwise.
#'
#' @keywords internal
validate_dni <- function(dni) {
  # Control letters for DNI based on modulus 23
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


validar_dni <- function(dni, ubicacion){
  if (!validate_dni(dni)) {
    stop(ubicacion, " -> El DNI '",
         dni,
         "' no es válido.")
  }
  TRUE
}
