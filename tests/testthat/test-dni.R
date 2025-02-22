test_that("validate_dni correctly identifies valid DNIs", {
  expect_true(validate_dni("12345678Z"))  # Example valid DNI
  expect_true(validate_dni("87654321X"))  # Another valid DNI
})

test_that("validate_dni correctly identifies invalid DNIs", {
  expect_false(validate_dni("12345678A"))  # Incorrect letter
  expect_false(validate_dni("8765432X"))   # Incorrect format (7 digits)
  expect_false(validate_dni("ABCDEFGHX"))  # Non-numeric input
  expect_false(validate_dni("123456789X")) # Incorrect format (9 digits)
})

test_that("validar_dni warns for invalid DNI numbers", {
  expect_warning(validar_dni("12345678A", "Test Case 1"),
                 "Test Case 1 -> El DNI '12345678A' no es válido.")
  expect_warning(validar_dni("ABCDEFGHZ", "Test Case 2"),
                 "Test Case 2 -> El DNI 'ABCDEFGHZ' no es válido.")
  expect_silent(validar_dni("12345678Z", "Test Case 3")) # Replace with a valid DNI
})
