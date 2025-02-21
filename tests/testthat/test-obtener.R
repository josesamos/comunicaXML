archivo_xlsx <- tempfile(fileext = ".xlsx")
archivo_ods <- tempfile(fileext = ".ods")
archivo_txt <- tempfile(fileext = ".txt")

test_that("La función copia correctamente un archivo .xlsx", {
  expect_silent(obtener_hoja_calculo(archivo_xlsx))  # No debería lanzar errores
  expect_true(file.exists(archivo_xlsx))  # El archivo debe existir después de la copia
})

test_that("La función copia correctamente un archivo .ods", {
  expect_silent(obtener_hoja_calculo(archivo_ods))
  expect_true(file.exists(archivo_ods))
})

test_that("La función falla si el archivo ya existe", {
  writeLines("contenido", archivo_xlsx)  # Crear un archivo ficticio
  expect_error(obtener_hoja_calculo(archivo_xlsx), "El archivo ya existe")
})

test_that("La función falla si el formato del archivo no es compatible", {
  expect_error(obtener_hoja_calculo(archivo_txt), "Formato no compatible")
})

# Limpiar archivos de prueba
unlink(c(archivo_xlsx, archivo_ods, archivo_txt))
