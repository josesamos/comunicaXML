
test_that("validar_establecimiento detects missing foreign key", {
  comunicacion <- data.frame(comunicacion_pk = 1:3)
  establecimiento <- data.frame(codigo = 1:3, pais = c("ESP", "FRA", "ESP"))

  expect_warning(validar_establecimiento(comunicacion, establecimiento),
                 "La columna 'comunicacion_fk' no existe en la hoja 'establecimiento'.")
})

test_that("validar_establecimiento detects missing establishment data (less than 7 fields)", {
  comunicacion <- data.frame(comunicacion_pk = 1:3)
  establecimiento <- data.frame(comunicacion_fk = 1, codigo = c(NA),
                                pais = c(NA), codigoMunicipio = c(NA),
                                nombreMunicipio = c(NA), extra1 = NA, extra2 = NA)

  expect_warning(validar_establecimiento(comunicacion, establecimiento),
                 "Faltan datos del establecimiento en la fila 1.")
})

test_that("validar_establecimiento allows valid establishments with at least 7 fields", {
  comunicacion <- data.frame(comunicacion_pk = 1:3)
  establecimiento <- data.frame(comunicacion_fk = 1:3, codigo = c(NA, NA, NA), tipo = c('HOTEL', 'HOTEL', 'HOTEL'),
                                pais = c("ESP", "ESP", "FRA"), codigoMunicipio = c(28079, '08019', NA),
                                nombreMunicipio = c(NA, NA, "Paris"), extra1 = 1, extra2 = 2, extra3 = 3, extra4 = 4)

  expect_silent(validar_establecimiento(comunicacion, establecimiento))
})

test_that("validar_establecimiento detects missing municipality code for Spain", {
  comunicacion <- data.frame(comunicacion_pk = 1:3)
  establecimiento <- data.frame(comunicacion_fk = 1:3, codigo = c(NA, NA, NA), tipo = c('HOTEL', 'HOTEL', 'HOTEL'),
                                pais = c("ESP", "ESP", "ESP"), codigoMunicipio = c(18003, 18003, 18003),
                                nombreMunicipio = c(NA, "NA", NA), extra1 = 1, extra2 = 2, extra3 = 3, extra4 = 4, extra5 = 5)

  expect_warning(validar_establecimiento(comunicacion, establecimiento),
                 "establecimiento \\(fila 2\\) -> Si el país es España \\(ESP\\), el código de municipio ha de ir informado, no así el nombre del municipio.")
})

test_that("validar_establecimiento detects incorrect foreign municipality data", {
  comunicacion <- data.frame(comunicacion_pk = 1:3)
  establecimiento <- data.frame(comunicacion_fk = 1:3, codigo = c(NA, NA, NA), tipo = c('HOTEL', 'HOTEL', 'HOTEL'),
                                pais = c("FRA", "USA", "DEU"), codigoMunicipio = c(NA, NA, 18003),
                                nombreMunicipio = c("NA", "New York", NA), extra1 = 1, extra2 = 2, extra3 = 3, extra4 = 4)

  expect_warning(validar_establecimiento(comunicacion, establecimiento),
                 "establecimiento \\(fila 3\\) -> Si el país es distinto de España \\(ESP\\), ha de informar el nombre del municipio, no así el código de municipio.")
})

test_that("validar_establecimiento detects both code and other data present", {
  comunicacion <- data.frame(comunicacion_pk = 1:3)
  establecimiento <- data.frame(comunicacion_fk = 1:3, codigo = c(1001, NA, NA), tipo = c('HOTEL', 'HOTEL', 'HOTEL'),
                                pais = c("ESP", "ESP", "FRA"), codigoMunicipio = c(12345, 18003, NA),
                                nombreMunicipio = c(NA, NA, "Paris"), extra1 = 1, extra2 = 2, extra3 = 3, extra4 = 4)

  expect_warning(validar_establecimiento(comunicacion, establecimiento),
                 "establecimiento \\(fila 1\\) -> Ha de indicar los datos del establecimiento o bien el código de establecimiento.")
})

test_that("validar_establecimiento passes when all conditions are met", {
  comunicacion <- data.frame(comunicacion_pk = 1:3)
  establecimiento <- data.frame(comunicacion_fk = 1:3, codigo = c(1001, NA, NA), tipo = c(NA, 'HOTEL', 'HOTEL'),
                                pais = c(NA, "ESP", "FRA"), codigoMunicipio = c(NA, 18003, NA),
                                nombreMunicipio = c(NA, NA, "Paris"), extra1 = c(NA,1,1), extra2 = c(NA,1,1), extra3 = c(NA,1,1), extra4 = c(NA,1,1))

  expect_silent(validar_establecimiento(comunicacion, establecimiento))
})
