
test_that("validar_contrato detecta errores y valida correctamente", {
  # Datos de comunicación con claves primarias válidas
  comunicacion <- data.frame(comunicacion_pk = c(1, 2, 3))

  # Datos de contrato con valores correctos
  contrato_valido <- data.frame(
    comunicacion_fk = c(1, 2, 3),
    referencia = c("REF123", "REF456", "REF789"),
    fechaContrato = c("2025-02-21", "2025-03-10", "2025-04-05"),
    fechaEntrada = c("2025-02-22T14:00:00", "2025-03-11T15:30:00", "2025-04-06T12:00:00"),
    fechaSalida = c("2025-02-25T10:00:00", "2025-03-15T11:00:00", "2025-04-10T14:00:00"),
    numPersonas = c("2", "3", "1"),
    numHabitaciones = c("1", "2", NA),
    internet = c("true", "false", NA),
    tipoPago = c("EFECT", "TARJT", "PLATF"),
    fechaPago = c("2025-02-20", NA, "2025-04-04"),
    caducidadTarjeta = c("12/2026", "06/2025", NA)
  )

  # Prueba sin errores
  expect_silent(validar_contrato(comunicacion, contrato_valido))

  # Datos con errores para verificar advertencias
  contrato_invalido <- contrato_valido
  contrato_invalido$fechaEntrada[1] <- "2025-02-26T14:00:00"  # Posterior a la fecha de salida

  # Prueba de advertencias esperadas
  expect_warning(validar_contrato(comunicacion, contrato_invalido),
                 "La fecha de salida ha de ser posterior a la fecha de entrada")

  contrato_invalido <- contrato_valido
  contrato_invalido$numPersonas[2] <- "0"  # No puede ser 0

  expect_warning(validar_contrato(comunicacion, contrato_invalido),
                 "El número de personas ha de ser un número entero mayor que 0")

  contrato_invalido <- contrato_valido
  contrato_invalido$internet[3] <- "yes"  # Valor no permitido

  expect_warning(validar_contrato(comunicacion, contrato_invalido),
                 "El campo 'internet' solo puede tomar los valores 'true' o 'false'")
})

