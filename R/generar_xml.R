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
#' @param pdf Booleano. Sigue el modelo del archivo pdf o del ejemplo xml.
#' @param optimizar Booleano. Elimina los nodos vacíos del archivo xml.
#'
#' @return Carácter. La ruta del archivo del documento XML generado.
#'
#' @examples
#' hoja_calculo <- system.file("extdata", "pdf/partes_viajeros.xlsx", package = "comunicaXML")
#' archivo_xml <- tempfile(fileext = ".xml")
#'
#' archivo <- generar_xml(hoja_calculo, archivo_xml)
#'
#' @export
generar_xml <- function(hoja_calculo, archivo_xml = NULL, pdf = TRUE, optimizar = FALSE) {
  if (pdf) {
    plantilla <- "pdf/templates/partes_viajeros.xml"
  }else {
    plantilla <- "xml/templates/partes_viajeros.xml"
  }

  file <- tab2xml::sheet2xml(hoja_calculo,
                     system.file("extdata", plantilla, package = "comunicaXML"),
                     archivo_xml)

  if (optimizar) {
    contenido <- xml2::read_xml(file)
    remove_empty_nodes(contenido)
    xml2::write_xml(contenido, file)
  }
  file
}
