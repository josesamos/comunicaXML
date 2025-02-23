test_that("validate_nif correctly identifies valid DNIs", {
  expect_true(validate_nif("12345678Z"))  # Example valid DNI
  expect_true(validate_nif("87654321X"))  # Another valid DNI
})

test_that("validate_nif correctly identifies invalid DNIs", {
  expect_false(validate_nif("12345678A"))  # Incorrect letter
  expect_false(validate_nif("8765432X"))   # Incorrect format (7 digits)
  expect_false(validate_nif("ABCDEFGHX"))  # Non-numeric input
  expect_false(validate_nif("123456789X")) # Incorrect format (9 digits)
})


test_that("Valid NIEs pass validation", {
  expect_true(validate_nie("X1234567L"))  # Ejemplo válido
  expect_true(validate_nie("Y7654321G"))  # Ejemplo válido
  expect_true(validate_nie("Z2345678M"))  # Ejemplo válido
})

test_that("Invalid NIEs fail validation", {
  expect_false(validate_nie("X12345678L"))  # Demasiados dígitos
  expect_false(validate_nie("B1234567C"))   # Letra inicial inválida
  expect_false(validate_nie("X12A4567L"))   # Caracteres inválidos en la parte numérica
})

test_that("NIEs with incorrect control letters fail", {
  expect_false(validate_nie("X1234567Z"))  # Letra de control incorrecta
  expect_false(validate_nie("Y7654321X"))  # Letra de control incorrecta
})

test_that("Edge cases", {
  expect_false(validate_nie(""))          # Cadena vacía
  expect_true(validate_nie("X0000000T")) # NIE con valores extremos
  expect_false(validate_nie("Z9999999A")) # NIE con valores extremos incorrectos
})



test_that("Valid NIFs and NIEs pass validation", {
  expect_true(validar_nif_nie("12345678Z", "Ubicación 1"))  # NIF válido
  expect_true(validar_nif_nie("X1234567L", "Ubicación 2"))  # NIE válido
  expect_true(validar_nif_nie("Y7654321G", "Ubicación 3"))  # NIE válido
})

test_that("Invalid NIFs and NIEs trigger warnings", {
  expect_warning(validar_nif_nie("12345678A", "Ubicación 4"),
                 "Ubicación 4 -> El NIF/NIE '12345678A' no es válido.") # Letra de control incorrecta

  expect_warning(validar_nif_nie("B1234567C", "Ubicación 5"),
                 "Ubicación 5 -> El NIF/NIE 'B1234567C' no es válido.") # Formato incorrecto

  expect_warning(validar_nif_nie("X12345678L", "Ubicación 6"),
                 "Ubicación 6 -> El NIF/NIE 'X12345678L' no es válido.") # Demasiados dígitos
})

test_that("Edge cases", {
  expect_warning(validar_nif_nie("", "Ubicación 7"),
                 "Ubicación 7 -> El NIF/NIE '' no es válido.") # Cadena vacía

  expect_warning(validar_nif_nie("Z9999999A", "Ubicación 8"),
                 "Ubicación 8 -> El NIF/NIE 'Z9999999A' no es válido.") # NIE inválido

  expect_warning(validar_nif_nie("00000001T", "Ubicación 9"),
                 "Ubicación 9 -> El NIF/NIE '00000001T' no es válido.") # NIF con valores extremos incorrectos
})
