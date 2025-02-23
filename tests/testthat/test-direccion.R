test_that("validar_direccion detecta errores en la estructura de una dirección", {

  direccion_valida <- data.frame(
    direccion = "Calle Ejemplo 123",
    pais = "ESP",
    codigoMunicipio = 28079,
    nombreMunicipio = NA,
    codigoPostal = "28001",
    stringsAsFactors = FALSE
  )

  direccion_sin_direccion <- data.frame(
    direccion = NA,
    pais = "ESP",
    codigoMunicipio = 28079,
    nombreMunicipio = NA,
    codigoPostal = "28001",
    stringsAsFactors = FALSE
  )

  direccion_sin_pais <- data.frame(
    direccion = "Calle Ejemplo 123",
    pais = NA,
    codigoMunicipio = 28079,
    nombreMunicipio = NA,
    codigoPostal = "28001",
    stringsAsFactors = FALSE
  )

  direccion_sin_codigoPostal <- data.frame(
    direccion = "Calle Ejemplo 123",
    pais = "ESP",
    codigoMunicipio = 28079,
    nombreMunicipio = NA,
    codigoPostal = NA,
    stringsAsFactors = FALSE
  )

  direccion_esp_con_nombreMunicipio <- data.frame(
    direccion = "Calle Ejemplo 123",
    pais = "ESP",
    codigoMunicipio = NA,
    nombreMunicipio = "Madrid",
    codigoPostal = "28001",
    stringsAsFactors = FALSE
  )

  direccion_extranjera_con_codigoMunicipio <- data.frame(
    direccion = "Calle Ejemplo 123",
    pais = "FRA",
    codigoMunicipio = 75001,
    nombreMunicipio = NA,
    codigoPostal = "75000",
    stringsAsFactors = FALSE
  )

  expect_true(validar_direccion(direccion_valida, "Test dirección válida"))

  expect_warning(validar_direccion(direccion_sin_direccion, "Test dirección sin dirección"),
                 "Falta el campo 'direccion'")

  expect_warning(validar_direccion(direccion_sin_pais, "Test dirección sin país"),
                 "Falta el campo 'pais'")

  expect_warning(validar_direccion(direccion_sin_codigoPostal, "Test dirección sin código postal"),
                 "Falta el campo 'codigoPostal'")

  expect_warning(validar_direccion(direccion_esp_con_nombreMunicipio, "Test dirección ESP con nombre de municipio"),
                 "Si el país es España \\(ESP\\), el código de municipio ha de ir informado, no así el nombre del municipio")

  expect_warning(validar_direccion(direccion_extranjera_con_codigoMunicipio, "Test dirección extranjera con código de municipio"),
                 "Si el país es distinto de España \\(ESP\\), ha de informar el nombre del municipio, no así el código de municipio")
})
