validar_fecha <- function(fecha, ubicacion){
  # Intentar convertir la fecha a tipo Date
  fecha_convertida <- as.character(as.Date(fecha, format = "%Y-%m-%d"))

  # Comprobar si la conversión fue exitosa y la fecha resultante es igual a la original
  if (is.na(fecha_convertida) | fecha_convertida != fecha) {
    stop(ubicacion, " -> La fecha '",
         fecha,
         "' no es válida, ha de ser 'aaaa-mm-dd'.")
  }
  TRUE
}

validar_fecha_tarjeta <- function(fecha, ubicacion){
  # Expresión regular para el formato mm/aaaa
  patron <- "^([0-1][0-9])/(\\d{4})$"

  # Verificar si la fecha cumple con el patrón
  if (!grepl(patron, fecha)) {
    stop(ubicacion, " -> La fecha de caducidad de la tarjeta '",
         fecha,
         "' no es válida, ha de ser 'mm/aaaa'.")
  }

  # Extraer mes y año de la fecha
  partes <- unlist(strsplit(fecha, "/"))
  mes <- as.numeric(partes[1])
  anio <- as.numeric(partes[2])

  # Validar que el mes esté entre 01 y 12
  if (mes < 1 || mes > 12) {
    stop(ubicacion, " -> El mes de la fecha de caducidad de la tarjeta '",
         fecha,
         "' no es válido.")
  }

  # Obtener el mes y año actuales
  fecha_actual <- Sys.Date()
  mes_actual <- as.numeric(format(fecha_actual, "%m"))
  anio_actual <- as.numeric(format(fecha_actual, "%Y"))

  # Validar que la fecha de caducidad sea igual o posterior al mes actual
  if (anio < anio_actual || (anio == anio_actual && mes < mes_actual)) {
    stop(ubicacion, " -> La tarjeta de fecha '",
         fecha,
         "' está vencida.")
  }

  TRUE
}
