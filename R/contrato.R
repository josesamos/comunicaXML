#'
#' @keywords internal
validar_contrato <- function(comunicacion, contrato) {
  validate_fk(comunicacion, "comunicacion", contrato, "contrato")

  df <- contrato

  for (i in 1:nrow(df)) {
    validar_fecha(df$fechaContrato[i], sprintf('contrato (fila %d)', i))
    validar_fecha(df$fechaEntrada[i], sprintf('contrato (fila %d)', i))
    validar_fecha(df$fechaSalida[i], sprintf('contrato (fila %d)', i))
    if (df$fechaEntrada[i] >= df$fechaSalida[i]) {
      warning(
        sprintf('contrato (fila %d)', i),
        " -> La fecha de salida ha de ser posterior a la fecha de entrada."
      )
    }
    if (!grepl("^[1-9][0-9]*$", df$numPersonas[i])) {
      warning(
        sprintf('contrato (fila %d)', i),
        " -> El número de personas ha de ser un número entero mayor que 0."
      )
    }
    if (!grepl("^[1-9][0-9]*$", df$numHabitaciones[i])) {
      warning(
        sprintf('contrato (fila %d)', i),
        " -> El número de habitaciones ha de ser un número entero mayor que 0."
      )
    }
    if (!(df$internet[i] %in% c("true", "false"))) {
      warning(
        sprintf('contrato (fila %d)', i),
        " -> El campo 'internet' solo puede tomar los valores 'true' o 'false'."
      )
    }
    if (!(df$tipoPago[i] %in% tipo_pago$codigo)) {
      warning(
        sprintf('contrato (fila %d)', i),
        " -> El campo 'tipoPago' no es válido. Consulta los códigos mediante 'View(tipo_pago)'.."
      )
    }
  }
  TRUE
}
