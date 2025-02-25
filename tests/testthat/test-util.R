test_that("is_cell_empty correctly identifies empty and non-empty cells", {
  expect_true(is_cell_empty(NA))         # NA should return TRUE
  expect_true(is_cell_empty(""))         # Empty string should return TRUE
  expect_false(is_cell_empty("Text"))    # Non-empty text should return FALSE
  expect_false(is_cell_empty(" "))       # Space is not empty, should return FALSE
  expect_false(is_cell_empty(123))       # Numbers are not empty, should return FALSE
  expect_false(is_cell_empty(0))         # Zero is a value, should return FALSE

  # Casos con vectores
  input_vector <- c(NA, "", "text", " ", 123)
  expected_output <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  expect_equal(is_cell_empty(input_vector), expected_output)

  # Casos mixtos con NA y cadenas vacías
  input_mixed <- c("", NA, "data", "NA", " ")
  expected_mixed <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  expect_equal(is_cell_empty(input_mixed), expected_mixed)
})
