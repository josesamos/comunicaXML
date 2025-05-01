utils::globalVariables(c(
  "apellido1", "apellido2", "apellidos", "fechaNacimiento", "n_palabras",
  "nombre", "numeroDocumento", "rol", "tipoDocumento"
))


#' Adaptar datos de Travel Contact a plantilla de alta de hospedaje
#'
#' Toma un archivo de Excel exportado desde Travel Contact, transforma los datos
#' al formato requerido por una plantilla de alta de reserva de hospedaje y los
#' inserta en el archivo de salida.
#'
#' @param travel_contact Nombre del archivo Excel de entrada. Por defecto `"Travel Contact.xlsx"`.
#' @param archivo Nombre del archivo Excel de salida. Por defecto `"tc_plantilla.xlsx"`.
#'
#' @return Nombre del archivo generado.
#'
#' @export
adaptar_travel_contact <- function(travel_contact = 'Travel Contact.xlsx', archivo = 'tc_plantilla.xlsx') {
  df <- readxl::read_excel(travel_contact, sheet = 'Sheet1', col_types = "text")
  names(df) <- c('ref', 'prod', 'nombre', 'apellidos', 'numeroDocumento', 'fechaNacimiento', 'notes')

  num_personas <- nrow(df)
  num_habitaciones <- dplyr::n_distinct(df$ref)

  df <- df |>
    dplyr::mutate(
      n_palabras = stringr::str_count(apellidos, "\\S+"),
      apellido2 = dplyr::if_else(n_palabras > 1, stringr::word(apellidos, -1), NA_character_),
      apellido1 = dplyr::if_else(n_palabras > 1,
                                         stringr::str_remove(apellidos, paste0("\\s", apellido2, "$")),
                                         apellidos)
    ) |>
    dplyr::select(-n_palabras)

  df2 <- dplyr::select(df, nombre, apellido1, apellido2, numeroDocumento, fechaNacimiento)
  df2 <- df2 |>
    dplyr::mutate(rol = c("TI", rep("VI", dplyr::n() - 1))) |>
    dplyr::relocate(rol, .before = 1)

  df2 <- df2 |>
    dplyr::mutate(tipoDocumento = ifelse(!is.na(numeroDocumento) & numeroDocumento != "", "NIF", NA_character_)) |>
    dplyr::relocate(tipoDocumento, .before = numeroDocumento)

  if (any(!is.na(df2$tipoDocumento) & df2$tipoDocumento != "")) {
    message("En la columna 'tipoDocumento' se ha indicado el valor 'NIF'. REVISAR QUE SEA NIF.")
  }

  df2 <- df2 |>
    dplyr::mutate(fechaNacimiento = as.Date(fechaNacimiento, format = "%d/%m/%Y"))

  df2 <- df2 |>
    dplyr::mutate(
      nacionalidad = NA_character_,
      sexo = NA_character_,
      direccion = NA_character_,
      direccionComplementaria = NA_character_,
      codigoMunicipio = NA_character_,
      nombreMunicipio = NA_character_,
      codigoPostal = NA_character_,
      pais = NA_character_,
      telefono = NA_character_,
      telefono2 = NA_character_,
      correo = NA_character_,
      comunicacion_fk = 1
    )

  hoja_calculo <- system.file("extdata", "alta_reserva_hospedaje_template.xlsx", package = "comunicaXML")
  wb <- openxlsx::loadWorkbook(hoja_calculo)

  openxlsx::writeData(wb, sheet = "contrato", x = num_personas, startCol = 5, startRow = 2, colNames = FALSE, rowNames = FALSE)
  openxlsx::writeData(wb, sheet = "contrato", x = num_habitaciones, startCol = 6, startRow = 2, colNames = FALSE, rowNames = FALSE)

  openxlsx::removeWorksheet(wb, "persona")
  openxlsx::addWorksheet(wb, "persona")
  openxlsx::writeData(wb, sheet = "persona", x = df2)

  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)

  message("Archivo transformado correctamente. Completa el archivo generado: ", archivo)

  return(archivo)
}
