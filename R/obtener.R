

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
obtener_hoja_calculo <- function(archivo) {
  if (file.exists(archivo)) {
    stop("El archivo ya existe: ", archivo)
  }

  extension <- tools::file_ext(archivo)

  if (extension == "xlsx") {
    hoja_calculo <- system.file("extdata", "alta_reserva_hospedaje_template.xlsx", package = "comunicaXML")
  } else if (extension == "ods") {
    hoja_calculo <- system.file("extdata", "alta_reserva_hospedaje_template.ods", package = "comunicaXML")
  } else {
    stop("Formato no compatible. Utilice un archivo .xlsx o .ods.")
  }

  file.copy(hoja_calculo, archivo, overwrite = FALSE)

  archivo
}
