test_that("validar_fecha() detects invalid dates", {
  expect_warning(validar_fecha("2024-02-30", "Test case"),
                 "Test case -> La fecha '2024-02-30' no es válida")
  expect_warning(validar_fecha("20240228", "Test case"),
                 "Test case -> La fecha '20240228' no es válida")
  expect_warning(validar_fecha("31/12/2024", "Test case"),
                 "Test case -> La fecha '31/12/2024' no es válida")
})

test_that("validar_fecha() accepts valid dates", {
  expect_silent(validar_fecha("2024-02-28", "Test case"))
  expect_silent(validar_fecha("1999-12-31", "Test case"))
})




test_that("validar_fecha_hora reconoce fechas y horas válidas", {
  expect_true(validar_fecha_hora("2025-02-21T15:30:45", "Test 1"))
  expect_true(validar_fecha_hora("2000-01-01T00:00:00", "Test 2"))
  expect_true(validar_fecha_hora("1999-12-31T23:59:59", "Test 3"))
})

test_that("validar_fecha_hora detecta formatos incorrectos", {
  expect_warning(validar_fecha_hora("2025-02-21 15:30:45", "Test 4"),
                 "ha de ser 'AAAA-MM-DDThh:mm:ss'")
  expect_warning(validar_fecha_hora("2025/02/21T15:30:45", "Test 5"),
                 "ha de ser 'AAAA-MM-DDThh:mm:ss'")
  expect_warning(validar_fecha_hora("21-02-2025T15:30:45", "Test 6"),
                 "ha de ser 'AAAA-MM-DDThh:mm:ss'")
  expect_warning(validar_fecha_hora("20250221T153045", "Test 7"),
                 "ha de ser 'AAAA-MM-DDThh:mm:ss'")
})

test_that("validar_fecha_hora detecta fechas inválidas", {
  expect_warning(validar_fecha_hora("2025-02-30T12:00:00", "Test 8"),
                 "no es válida")
  expect_warning(validar_fecha_hora("2025-04-31T10:15:00", "Test 9"),
                 "no es válida")
  expect_warning(validar_fecha_hora("2025-02-21T25:30:45", "Test 10"),
                 "no es válida")
  expect_warning(validar_fecha_hora("2025-02-21T12:60:00", "Test 11"),
                 "no es válida")
  expect_warning(validar_fecha_hora("2025-02-21T12:30:60", "Test 12"),
                 "no es válida")
})



test_that("validar_fecha_tarjeta() detects invalid formats", {
  expect_warning(validar_fecha_tarjeta("13/2025", "Test case"),
                 "Test case -> El mes de la fecha de caducidad de la tarjeta '13/2025' no es válido")
  expect_warning(validar_fecha_tarjeta("00/2025", "Test case"),
                 "Test case -> El mes de la fecha de caducidad de la tarjeta '00/2025' no es válido")
  expect_warning(validar_fecha_tarjeta("2025/12", "Test case"),
                 "Test case -> La fecha de caducidad de la tarjeta '2025/12' no es válida")
  expect_warning(validar_fecha_tarjeta("12-2025", "Test case"),
                 "Test case -> La fecha de caducidad de la tarjeta '12-2025' no es válida")
})

test_that("validar_fecha_tarjeta() detects expired cards", {
  fecha_vencida <- format(Sys.Date() - 365, "%m/%Y")
  expect_warning(validar_fecha_tarjeta(fecha_vencida, "Test case"),
                 "Test case -> La tarjeta de fecha '")
})

test_that("validar_fecha_tarjeta() accepts valid dates", {
  fecha_valida <- format(Sys.Date() + 365, "%m/%Y")
  expect_silent(validar_fecha_tarjeta(fecha_valida, "Test case"))
  expect_silent(validar_fecha_tarjeta("12/2030", "Test case"))
})


test_that("Verifica si una persona es mayor de edad", {
  expect_true(es_mayor_de_edad("2000-01-01"))  # Persona mayor de 18 años
  expect_false(es_mayor_de_edad("2010-05-15")) # Persona menor de 18 años
})

test_that("Maneja correctamente fechas límite", {
  fecha_hoy <- as.character(Sys.Date() - (18 * 365)) # Justo 18 años atrás
  expect_true(es_mayor_de_edad(fecha_hoy))  # Justo 18 años debería ser TRUE

  fecha_un_dia_despues <- as.character(Sys.Date() - (18 * 365) + 1)
  expect_false(es_mayor_de_edad(fecha_un_dia_despues))  # Un día menos de 18 años debería ser FALSE
})

