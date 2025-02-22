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
