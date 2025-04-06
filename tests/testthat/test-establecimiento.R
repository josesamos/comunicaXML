
test_that("validar_establecimiento maneja correctamente los diferentes casos", {

  # Datos de comunicación válidos
  comunicacion <- data.frame(comunicacion_pk = c(1, 2, 3))

  # Caso 2: Establecimiento sin código, pero con tipo y nombre válidos
  establecimiento_con_datos <- data.frame(
    comunicacion_fk = 1,
    tipo = "HOTEL",
    nombre = "Hotel Ejemplo",
    pais = "ESP",
    codigoMunicipio = 28079,
    nombreMunicipio = NA,
    codigoPostal = "28001",
    direccion = "Calle Ejemplo, 123"
  )
  expect_silent(validar_establecimiento(comunicacion, establecimiento_con_datos))

  # Caso 3: Falta el campo 'tipo'
  establecimiento_sin_tipo <- establecimiento_con_datos
  establecimiento_sin_tipo$tipo <- NA
  expect_warning(validar_establecimiento(comunicacion, establecimiento_sin_tipo),
                 "Falta el campo 'tipo'")

  # Caso 4: Tipo de establecimiento no válido
  establecimiento_tipo_invalido <- establecimiento_con_datos
  establecimiento_tipo_invalido$tipo <- "TIPO_INVALIDO"
  expect_warning(validar_establecimiento(comunicacion, establecimiento_tipo_invalido),
                 "El campo 'tipo' no es válido")

  # Caso 5: Falta el campo 'nombre'
  establecimiento_sin_nombre <- establecimiento_con_datos
  establecimiento_sin_nombre$nombre <- NA
  expect_warning(validar_establecimiento(comunicacion, establecimiento_sin_nombre),
                 "Falta el campo 'nombre'")

  # Caso 6: Dirección incompleta
  establecimiento_direccion_incompleta <- establecimiento_con_datos
  establecimiento_direccion_incompleta$codigoPostal <- NA
  expect_warning(validar_establecimiento(comunicacion, establecimiento_direccion_incompleta),
                 "Falta el campo 'codigoPostal'")

})
