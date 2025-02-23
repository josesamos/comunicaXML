
#' Validar una hoja de cálculo con datos de comunicación
#'
#' Valida una hoja de cálculo que contiene datos estructurados de comunicaciones,
#' establecimientos, contratos y personas. Asegura que el archivo existe, que tiene
#' una extensión válida (`.xlsx` o `.ods`) y que los datos dentro de las hojas cumplen
#' con los requisitos de validación.
#'
#' @param hoja_calculo Ruta al archivo de hoja de cálculo en formato `.xlsx` o `.ods`.
#'
#' @return Devuelve `TRUE` si la validación se completa sin errores. En caso de problemas,
#'         se generan errores (`stop`) o advertencias (`warning`).
#'
#' @examples
#' hoja_calculo <- system.file("extdata", "partes_viajeros.xlsx", package = "comunicaXML")
#'
#' res <- validar_hoja_calculo(hoja_calculo)
#'
#' @export
validar_hoja_calculo<- function(hoja_calculo) {
  if (!file.exists(hoja_calculo)) {
    stop("El archivo no existe: ", hoja_calculo)
  }

  extension <- tools::file_ext(hoja_calculo)
  sheets_data <- list()

  if (extension == "xlsx") {
    sheet_names <- readxl::excel_sheets(hoja_calculo)
    sheets_data <- lapply(sheet_names, function(sheet)
      readxl::read_excel(hoja_calculo, sheet = sheet))

  } else if (extension == "ods") {
    sheet_names <- readODS::ods_sheets(hoja_calculo)
    sheets_data <- lapply(seq_along(sheet_names), function(sheet)
      readODS::read_ods(hoja_calculo, sheet = sheet))

  } else {
    stop("Formato no compatible. Utilice un archivo .xlsx o .ods.")
  }
  names(sheets_data) <- tolower(sheet_names)

  validate_pk(sheets_data$comunicacion, "comunicacion")

  validar_establecimiento(sheets_data$comunicacion, sheets_data$establecimiento)

  validar_contrato(sheets_data$comunicacion, sheets_data$contrato)

  validar_persona(sheets_data$comunicacion, sheets_data$persona)

  TRUE
}

