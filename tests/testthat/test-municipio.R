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


test_that("validar_codigo_postal verifica correctamente los códigos postales españoles", {

  # Casos válidos
  expect_true(validar_codigo_postal("28001"))  # Código postal válido de Madrid
  expect_true(validar_codigo_postal("41001"))  # Código postal válido de Sevilla
  expect_true(validar_codigo_postal("35010"))  # Código postal válido de Las Palmas
  expect_true(validar_codigo_postal(28001))    # Número en vez de cadena de caracteres

  # Casos inválidos
  expect_false(validar_codigo_postal("1234"))   # Menos de 5 dígitos
  expect_false(validar_codigo_postal("123456")) # Más de 5 dígitos
  expect_false(validar_codigo_postal("ABCDE"))  # Letras en lugar de números
  expect_false(validar_codigo_postal("00A12"))  # Caracteres mixtos
  expect_false(validar_codigo_postal("99000"))  # Código de provincia no válido (99)
  expect_false(validar_codigo_postal("53000"))  # Código de provincia fuera del rango (mayor a 52)
  expect_false(validar_codigo_postal(NA))       # Valor NA

})
