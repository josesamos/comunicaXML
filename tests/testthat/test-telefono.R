test_that("validar_telefono accepts valid phone numbers", {
  expect_silent(validar_telefono("123456789", "Registro 1"))
  expect_silent(validar_telefono("987654321", "Registro 2"))
})

test_that("validar_telefono warns for phone numbers with less than 9 digits", {
  expect_warning(validar_telefono("12345678", "Registro 3"),
                 "Registro 3 -> El teléfono '12345678' no es válido. Ha de ser 9 números sin espacios.")
})

test_that("validar_telefono warns for phone numbers with more than 9 digits", {
  expect_warning(validar_telefono("1234567890", "Registro 4"),
                 "Registro 4 -> El teléfono '1234567890' no es válido. Ha de ser 9 números sin espacios.")
})

test_that("validar_telefono warns for phone numbers containing spaces", {
  expect_warning(validar_telefono("123 456 789", "Registro 5"),
                 "Registro 5 -> El teléfono '123 456 789' no es válido. Ha de ser 9 números sin espacios.")
})

test_that("validar_telefono warns for phone numbers containing non-numeric characters", {
  expect_warning(validar_telefono("12345A789", "Registro 6"),
                 "Registro 6 -> El teléfono '12345A789' no es válido. Ha de ser 9 números sin espacios.")
})

test_that("validar_telefono warns for empty phone numbers", {
  expect_warning(validar_telefono("", "Registro 7"),
                 "Registro 7 -> El teléfono '' no es válido. Ha de ser 9 números sin espacios.")
})

test_that("validar_telefono warns for NA values", {
  expect_warning(validar_telefono(NA, "Registro 8"),
                 "Registro 8 -> El teléfono 'NA' no es válido. Ha de ser 9 números sin espacios.")
})
