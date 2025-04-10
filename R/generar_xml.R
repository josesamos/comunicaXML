.onLoad <- function(libname, pkgname) {
  utils::data(
    "generos",
    "iso_codes",
    "municipios",
    "parentesco",
    "tipo_documento",
    "tipo_establecimiento",
    "tipo_pago",
    package = pkgname,
    envir = parent.env(environment())
  )
}

#' Generar partes de viajeros XML desde una hoja de cálculo
#'
#' Esta función lee un archivo de hoja de cálculo (Excel o ODS) y genera un archivo
#' de salida XML que contiene datos estructurados.
#'
#' @param hoja_calculo Carácter. Ruta al archivo de hoja de cálculo que se va a procesar.
#' @param archivo_xml Carácter (opcional). Ruta del archivo XML de salida. Si es NULO,
#' el archivo XML tendrá el mismo nombre y ubicación que el archivo de hoja de cálculo.
#'
#' @return Carácter. La ruta del archivo del documento XML generado.
#'
#' @examples
#' hoja_calculo <- system.file("extdata", "alta_reserva_hospedaje.xlsx", package = "comunicaXML")
#' archivo_xml <- tempfile(fileext = ".xml")
#'
#' archivo <- generar_xml(hoja_calculo, archivo_xml)
#'
#' @export
generar_xml <- function(hoja_calculo, archivo_xml = NULL) {
  file <- tab2xml::sheet2xml(
    hoja_calculo,
    system.file("extdata", "templates/alta_reserva_hospedaje.xml", package = "comunicaXML"),
    archivo_xml,
    optimize = TRUE
  )
  file
}
