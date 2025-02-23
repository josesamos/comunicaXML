
#' Validar la estructura de una dirección
#'
#' Verifica la validez de los campos de una dirección, asegurándose de que contenga los datos
#' mínimos requeridos y que sean correctos según el país especificado.
#'
#' @param direccion Un data frame con los siguientes campos:
#'   - `direccion` (character): Dirección completa.
#'   - `pais` (character): Código del país en formato ISO 3166-1 alfa-3.
#'   - `codigoMunicipio` (character/numeric): Código del municipio (si aplica).
#'   - `nombreMunicipio` (character): Nombre del municipio (si aplica).
#'   - `codigoPostal` (character/numeric): Código postal.
#' @param ubicacion Un string que indica el contexto de validación, útil para los mensajes de advertencia.
#'
#' @details
#' - Si falta alguno de los campos requeridos (`direccion`, `pais` o `codigoPostal`), se generará una advertencia.
#' - Valida el código de país usando `validar_pais()`.
#' - Si el país es España (`ESP`), el código de municipio debe estar informado y validado con `validar_municipio()`,
#'   y el nombre del municipio no debe estar presente.
#' - Si el país es distinto de España, el nombre del municipio debe estar informado en lugar del código de municipio.
#' - Si se especifica un código postal para España, se valida con `validar_codigo_postal()`.
#'
#' @return Retorna `TRUE` si la validación se completa (aunque puede generar advertencias en caso de datos incorrectos).
#'
#' @keywords internal
validar_direccion <- function(direccion, ubicacion) {
  df <- direccion

  if (is.na(df$direccion)) {
    warning(ubicacion,
            " -> Falta el campo 'direccion'.")
  }
  if (is.na(df$pais)) {
    warning(ubicacion,
            " -> Falta el campo 'pais'.")
  } else {
    validar_pais(df$pais, ubicacion)
    if (df$pais == 'ESP') {
      if (is.na(df$codigoMunicipio) || (!is.na(df$nombreMunicipio))) {
        warning(
          ubicacion,
          " -> Si el país es España (ESP), el código de municipio ha de ir informado, no así el nombre del municipio."
        )
      } else {
        validar_municipio(df$codigoMunicipio, ubicacion)
      }
    } else {
      if ((!is.na(df$codigoMunicipio)) || is.na(df$nombreMunicipio)) {
        warning(
          ubicacion,
          " -> Si el país es distinto de España (ESP), ha de informar el nombre del municipio, no así el código de municipio."
        )
      }
    }
  }
  if (is.na(df$codigoPostal)) {
    warning(ubicacion,
            " -> Falta el campo 'codigoPostal'.")
  } else if (!is.na(df$pais)) {
    if (df$pais == 'ESP') {
      if (!validar_codigo_postal(df$codigoPostal)) {
        warning(ubicacion, " -> El código postal '",
                df$codigoMunicipio,
                "' no es válido.")
      }
    }
  }
  TRUE
}
