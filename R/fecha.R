#' Validar formato de fecha
#'
#' Comprueba si una fecha determinada sigue el formato 'AAAA-MM-DD'.
#' Si el formato es incorrecto, se emite una advertencia con la ubicación proporcionada.
#'
#' @param fecha Carácter. La cadena de fecha para validar.
#' @param ubicacion Carácter. Una descripción de la ubicación donde se realiza la validación.
#'
#' @return Lógico. Siempre devuelve TRUE.
#'
#' @keywords internal
validar_fecha <- function(fecha, ubicacion) {
  fecha_convertida <- as.character(as.Date(fecha, format = "%Y-%m-%d"))

  if (is.na(fecha_convertida) || fecha_convertida != fecha) {
    warning(ubicacion,
            " -> La fecha '",
            fecha,
            "' no es válida, ha de ser 'AAAA-MM-DD'.")
  }
  TRUE
}



#' Validar formato de fecha y hora
#'
#' Comprueba si una fecha y hora determinada sigue el formato 'AAAA-MM-DDThh:mm:ss'.
#' Si el formato es incorrecto, se emite una advertencia con la ubicación proporcionada.
#'
#' @param fecha_hora Carácter. La cadena de fecha para validar.
#' @param ubicacion Carácter. Una descripción de la ubicación donde se realiza la validación.
#'
#' @return Lógico. Siempre devuelve TRUE.
#'
#' @keywords internal
validar_fecha_hora <- function(fecha_hora, ubicacion) {
  # "AAAA-MM-DDThh:mm:ss"
  patron <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$"

  if (!grepl(patron, fecha_hora)) {
    warning(ubicacion,
            " -> La fecha y hora '",
            fecha_hora,
            "' no es válida, ha de ser 'AAAA-MM-DDThh:mm:ss'.")
  } else {
    fecha_convertida <- as.POSIXct(fecha_hora, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    if (!is.na(fecha_convertida)) {
      fecha_formateada <- format(fecha_convertida, "%Y-%m-%dT%H:%M:%S")
    } else {
      fecha_formateada <- ""
    }

    if(fecha_hora != fecha_formateada) {
      warning(ubicacion,
              " -> La fecha y hora '",
              fecha_hora,
              "' no es válida, ha de ser 'AAAA-MM-DDThh:mm:ss'.")
    }
  }
  TRUE
}



#' Validar fecha de vencimiento de la tarjeta de crédito
#'
#' Comprueba si la fecha de vencimiento de una tarjeta de crédito determinada sigue
#' el formato 'mm/aaaa'. También verifica si la fecha es válida y no ha caducado.
#'
#' Si se encuentra algún problema, se emite una advertencia con la ubicación proporcionada.
#'
#' @param fecha Carácter. La cadena de fecha de vencimiento para validar.
#' @param ubicacion Carácter. Una descripción de la ubicación donde se realiza la validación.
#'
#' @return Lógico. Siempre devuelve TRUE.
#'
#' @keywords internal
validar_fecha_tarjeta <- function(fecha, ubicacion){
  #  mm/aaaa
  patron <- "^([0-1][0-9])/(\\d{4})$"

  if (!grepl(patron, fecha)) {
    warning(ubicacion, " -> La fecha de caducidad de la tarjeta '",
         fecha,
         "' no es válida, ha de ser 'mm/aaaa'.")
  } else {
    partes <- unlist(strsplit(fecha, "/"))
    mes <- as.numeric(partes[1])
    anio <- as.numeric(partes[2])

    if (mes < 1 || mes > 12) {
      warning(ubicacion, " -> El mes de la fecha de caducidad de la tarjeta '",
           fecha,
           "' no es válido.")
    } else {
      fecha_actual <- Sys.Date()
      mes_actual <- as.numeric(format(fecha_actual, "%m"))
      anio_actual <- as.numeric(format(fecha_actual, "%Y"))

      if (anio < anio_actual || (anio == anio_actual && mes < mes_actual)) {
        warning(ubicacion, " -> La tarjeta de fecha '",
             fecha,
             "' está vencida.")
      }
    }
  }
  TRUE
}


#' Verifica si una persona es mayor de edad
#'
#' Esta función determina si una persona es mayor de 18 años a partir de su fecha de nacimiento.
#'
#' @param fecha_nacimiento Una cadena de texto con la fecha de nacimiento en formato "AAAA-MM-DD".
#' @return `TRUE` si la persona tiene 18 años o más, `FALSE` en caso contrario.
#'
#' @keywords internal
es_mayor_de_edad <- function(fecha_nacimiento) {
  fecha_nacimiento <- as.Date(fecha_nacimiento)

  fecha_actual <- Sys.Date()

  edad <- as.numeric(difftime(fecha_actual, fecha_nacimiento, units = "days")) %/% 365

  return(edad >= 18)
}
