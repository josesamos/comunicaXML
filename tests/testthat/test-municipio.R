test_that("validar_pais correctly validates country codes", {
  expect_warning(validar_pais("XYZ", "Test Case"), "XYZ' no es válido") # Código inválido
  expect_silent(validar_pais("ESP", "Test Case")) # Código válido
  expect_silent(validar_pais("USA", "Test Case")) # Código válido
})

test_that("validar_municipio correctly validates municipality codes", {
  expect_warning(validar_municipio("99999", "Test Case"), "99999' no es válido") # Código inválido
  expect_silent(validar_municipio("28079", "Test Case")) # Código válido
  expect_silent(validar_municipio("08019", "Test Case")) # Código válido
})
