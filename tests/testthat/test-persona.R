

comunicacion <- structure(
  list(comunicacion_pk = 1, comentario = "Ejemplo de comunicación"),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -1L)
)

persona <- structure(
  list(
    rol = "TI",
    nombre = "Nombre1",
    apellido1 = "Apellido11",
    apellido2 = "Apellido21",
    tipoDocumento = "NIF",
    numeroDocumento = "11111111H",
    soporteDocumento = "ABC123456",
    fechaNacimiento = "1980-01-01",
    nacionalidad = "ESP",
    sexo = "H",
    direccion = "Dirección 1",
    direccionComplementaria = "Complemento 1",
    codigoMunicipio = "18001",
    nombreMunicipio = NA,
    codigoPostal = "18001",
    pais = "ESP",
    telefono = "111111111",
    telefono2 = "211111111",
    correo = "correo1@correo.es",
    parentesco = NA,
    comunicacion_fk = 1
  ),
  row.names = c(NA, -1L),
  class = c("tbl_df", "tbl", "data.frame")
)


test_that("Validar persona con datos correctos", {
  expect_true(validar_persona(comunicacion, persona))
})

test_that("validar_persona genera advertencia si falta el nombre", {
  persona_fallo <- persona
  persona_fallo$nombre <- NA
  expect_warning(validar_persona(comunicacion, persona_fallo), "Falta el campo 'nombre'")
})

test_that("validar_persona genera advertencia si falta el apellido1", {
  persona_fallo <- persona
  persona_fallo$apellido1 <- NA
  expect_warning(validar_persona(comunicacion, persona_fallo), "Falta el campo 'apellido1'")
})


test_that("validar_persona genera advertencia si una persona mayor de edad no tiene documento", {
  persona_fallo <- persona
  persona_fallo$tipoDocumento <- NA
  expect_warning(validar_persona(comunicacion, persona_fallo), "Falta el campo 'tipo_documento'")
})

test_that("validar_persona genera advertencia si tipoDocumento es inválido", {
  persona_fallo <- persona
  persona_fallo$tipoDocumento <- "INVALIDO"
  expect_warning(validar_persona(comunicacion, persona_fallo), "El campo 'tipo_documento' no es válido")
})

test_that("validar_persona genera advertencia si NIF no tiene apellido2", {
  persona_fallo <- persona
  persona_fallo$apellido2 <- NA
  expect_warning(validar_persona(comunicacion, persona_fallo), "Falta el campo 'apellido2'. Obligatorio si el tipo de documento es NIF")
})

test_that("validar_persona genera advertencia si falta soporteDocumento para NIF/NIE", {
  persona_fallo <- persona
  persona_fallo$soporteDocumento <- NA
  expect_warning(validar_persona(comunicacion, persona_fallo), "Falta el campo 'soporteDocumento'. Obligatorio si el tipo de documento es NIF o NIE")
})

test_that("validar_persona genera advertencia si el teléfono es inválido", {
  persona_fallo <- persona
  persona_fallo$telefono <- "1234"
  expect_warning(validar_persona(comunicacion, persona_fallo), "El teléfono .* no es válido")
})

test_that("validar_persona genera advertencia si no hay teléfono, teléfono2 ni correo", {
  persona_fallo <- persona
  persona_fallo$telefono <- NA
  persona_fallo$telefono2 <- NA
  persona_fallo$correo <- NA
  expect_warning(validar_persona(comunicacion, persona_fallo), "Obligatorio incluir una de estas tres etiquetas: 'telefono', 'telefono2' o 'correo'")
})

test_that("validar_persona genera advertencia si el parentesco no es válido", {
  persona_fallo <- persona
  persona_fallo$parentesco <- "INVALIDO"
  expect_warning(validar_persona(comunicacion, persona_fallo), "El campo 'parentesco' no es válido")
})

