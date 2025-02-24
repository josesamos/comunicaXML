
test_that("Validar solicitud con datos correctos", {
  solicitud <- structure(
    list(solicitud_pk = "1", codigoEstablecimiento = "A123456789"),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -1L)
  )
  expect_true(validar_solicitud(solicitud))
})

test_that("Validar solicitud con valores faltantes", {
  solicitud <- structure(
    list(solicitud_pk = "1", codigoEstablecimiento =  c("EST001", NA, "EST003")),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -3L)
  )
  expect_warning(validar_solicitud(solicitud), "solicitud \\(fila 2\\) -> Falta el campo 'codigoEstablecimiento'")
})
