test_that("validar_hoja_calculo works", {
  hoja_calculo <- system.file("extdata", "partes_viajeros.xlsx", package = "comunicaXML")

  res <- validar_hoja_calculo(hoja_calculo)

  expect_equal(res, TRUE)
})
