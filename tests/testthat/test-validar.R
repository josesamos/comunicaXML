test_that("validar_hoja_calculo works", {
  hoja_calculo <- system.file("extdata", "xml/partes_viajeros.xlsx", package = "comunicaXML")

  res <- validar_hoja_calculo(hoja_calculo, pdf = FALSE)

  expect_equal(res, TRUE)
})

test_that("validar_hoja_calculo works", {
  hoja_calculo <- system.file("extdata", "pdf/partes_viajeros.xlsx", package = "comunicaXML")

  res <- validar_hoja_calculo(hoja_calculo)

  expect_equal(res, TRUE)
})
