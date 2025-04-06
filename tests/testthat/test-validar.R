test_that("validar_hoja_calculo works", {
  hoja_calculo <- system.file("extdata", "alta_reserva_hospedaje.xlsx", package = "comunicaXML")

  res <- validar_hoja_calculo(hoja_calculo)

  expect_equal(res, TRUE)
})

test_that("validar_hoja_calculo works", {
  hoja_calculo <- system.file("extdata", "alta_reserva_hospedaje.xlsx", package = "comunicaXML")

  res <- validar_hoja_calculo(hoja_calculo)

  expect_equal(res, TRUE)
})
