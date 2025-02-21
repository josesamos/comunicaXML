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
