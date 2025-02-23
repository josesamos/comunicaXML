#' Validar los datos de un contrato
#'
#' Verifica que los datos de un contrato sean válidos, comprobando la existencia
#' de claves foráneas, el formato correcto de fechas, la coherencia de fechas de entrada y salida,
#' y la validez de otros campos como número de personas, número de habitaciones, tipo de pago e internet.
#'
#' @param comunicacion Un `data.frame` que contiene las claves primarias de comunicación.
#' @param contrato Un `data.frame` con la información de los contratos a validar.
#'
#' @details
#' La validación incluye:
#' - Clave foránea: Se verifica que la clave de contrato en `contrato` existe en `comunicacion`.
#' - `referencia`: Debe estar presente.
#' - `fechaContrato`: Debe cumplir con el formato de fecha `AAAA-MM-DD`.
#' - `fechaEntrada` y `fechaSalida`: Deben cumplir con el formato `AAAA-MM-DDThh:mm:ss` y la fecha de salida debe ser posterior a la de entrada.
#' - `numPersonas`: Debe ser un número entero mayor que 0.
#' - `numHabitaciones`: Si está presente, debe ser un número entero mayor que 0.
#' - `internet`: Si está presente, solo puede contener los valores `"true"` o `"false"`.
#' - `tipoPago`: Debe ser un valor válido según la tabla `tipo_pago`.
#' - `fechaPago`: Si está presente, debe cumplir con el formato `AAAA-MM-DD`.
#' - `caducidadTarjeta`: Si está presente, debe cumplir con el formato `MM/AAAA`.
#'
#' Si alguna validación falla, se generarán advertencias (`warning`) indicando el problema y la fila afectada.
#'
#' @return Devuelve `TRUE` si no se encuentran errores. En caso de valores inválidos, muestra advertencias.
#'
#' @keywords internal
validar_contrato <- function(comunicacion, contrato) {
  validate_fk(comunicacion, "comunicacion", contrato, "contrato")

  df <- contrato

  for (i in 1:nrow(df)) {
    ubicacion <- sprintf('contrato (fila %d)', i)
    if (is.na(df$referencia[i])) {
      warning(ubicacion,
              " -> Falta el n\u00famero de referencia del contrato.")
    }
    if (is.na(df$fechaContrato[i])) {
      warning(ubicacion,
              " -> Falta la fecha del contrato.")
    } else {
      validar_fecha(df$fechaContrato[i], ubicacion)
    }
    if (is.na(df$fechaEntrada[i])) {
      warning(ubicacion,
              " -> Falta la fecha de entrada.")
    } else {
      validar_fecha_hora(df$fechaEntrada[i], ubicacion)
    }
    if (is.na(df$fechaSalida[i])) {
      warning(ubicacion,
              " -> Falta la fecha de salida.")
    } else {
      validar_fecha_hora(df$fechaSalida[i], ubicacion)
    }

    if (df$fechaEntrada[i] >= df$fechaSalida[i]) {
      warning(
        ubicacion,
        " -> La fecha de salida ha de ser posterior a la fecha de entrada."
      )
    }
    if (!grepl("^[1-9][0-9]*$", df$numPersonas[i])) {
      warning(
        ubicacion,
        " -> El n\u00famero de personas ha de ser un n\u00famero entero mayor que 0."
      )
    }
    if (!is.na(df$numHabitaciones[i])) {
      if (!grepl("^[1-9][0-9]*$", df$numHabitaciones[i])) {
        warning(
          ubicacion,
          " -> El n\u00famero de habitaciones ha de ser un n\u00famero entero mayor que 0."
        )
      }
    }
    if (!is.na(df$internet[i])) {
      if (!(df$internet[i] %in% c("true", "false"))) {
        warning(
          ubicacion,
          " -> El campo 'internet' solo puede tomar los valores 'true' o 'false'."
        )
      }
    }
    if (!(df$tipoPago[i] %in% tipo_pago$codigo)) {
      warning(
        ubicacion,
        " -> El campo 'tipoPago' no es v\u00e1lido. Consulta los c\u00f3digos mediante 'View(tipo_pago)'.."
      )
    }
    if (!is.na(df$fechaPago[i])) {
      validar_fecha(df$fechaPago[i], ubicacion)
    }
    if (!is.na(df$caducidadTarjeta[i])) {
      validar_fecha_tarjeta(df$caducidadTarjeta[i], ubicacion)
    }
  }
  TRUE
}
