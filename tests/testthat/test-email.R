
test_that("validar_email reconoce correos válidos", {
  expect_silent(validar_email("usuario@example.com", "Test 1"))
  expect_silent(validar_email("nombre.apellido@dominio.org", "Test 2"))
  expect_silent(validar_email("correo123@sub.dominio.net", "Test 3"))
  expect_silent(validar_email("user+alias@empresa.co.uk", "Test 4"))
})

test_that("validar_email detecta correos inválidos", {
  expect_warning(validar_email("usuario@@example.com", "Test 5"),
                 "Test 5 -> La dirección de correo electrónico 'usuario@@example.com' no es válida.")
  expect_warning(validar_email("usuario.example.com", "Test 6"),
                 "Test 6 -> La dirección de correo electrónico 'usuario.example.com' no es válida.")
  expect_warning(validar_email("usuario@com", "Test 7"),
                 "Test 7 -> La dirección de correo electrónico 'usuario@com' no es válida.")
  expect_warning(validar_email("@example.com", "Test 8"),
                 "Test 8 -> La dirección de correo electrónico '@example.com' no es válida.")
  expect_warning(validar_email("usuario@.com", "Test 9"),
                 "Test 9 -> La dirección de correo electrónico 'usuario@.com' no es válida.")
})
