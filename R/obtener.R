

#' Obtener una hoja de cálculo predefinida
#'
#' Esta función copia una hoja de cálculo predefinida a una ubicación especificada
#' por el usuario. Se admiten los formatos `.xlsx` y `.ods`.
#'
#' Si el archivo de destino ya existe, la función detiene la ejecución con un error.
#' Si el formato del archivo no es compatible, también se genera un error.
#'
#' @param archivo Ruta del archivo de destino donde se copiará la hoja de cálculo.
#' Debe incluir la extensión `.xlsx` o `.ods`.
#' @param pdf Booleano. Sigue el modelo del archivo pdf o del ejemplo xml.
#'
#' @return Devuelve el nombre del archivo.
#'
#' @examples
#' \dontrun{
#' obtener_hoja_calculo("mi_hoja.xlsx")
#' obtener_hoja_calculo("mi_hoja.ods")
#' }
#'
#' @export
obtener_hoja_calculo <- function(archivo, pdf = TRUE) {
  if (file.exists(archivo)) {
    stop("El archivo ya existe: ", archivo)
  }

  extension <- tools::file_ext(archivo)

  if (pdf) {
    dir <- 'pdf/'
  } else {
    dir <- 'xml/'
  }

  if (extension == "xlsx") {
    hoja_calculo <- system.file("extdata", paste0(dir, "partes_viajeros_template.xlsx"), package = "comunicaXML")
  } else if (extension == "ods") {
    hoja_calculo <- system.file("extdata", paste0(dir, "partes_viajeros_template.ods"), package = "comunicaXML")
  } else {
    stop("Formato no compatible. Utilice un archivo .xlsx o .ods.")
  }

  file.copy(hoja_calculo, archivo, overwrite = FALSE)

  archivo
}
