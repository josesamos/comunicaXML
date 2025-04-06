#' Validar datos del establecimiento
#'
#' Comprueba si los datos del establecimiento proporcionados están estructurados correctamente y son coherentes.
#' Comprueba que:
#': los campos obligatorios se completan según el país.
#': la información del municipio está especificada correctamente según el país.
#': se proporciona un código de establecimiento válido o detalles completos del establecimiento, pero no ambos.
#'
#' @param comunicacion Un data frame que representa la tabla de comunicación.
#' Debe contener una columna de clave principal.
#' @param establecimiento Un data frame que representa la tabla de establecimiento.
#' Debe contener los campos necesarios:
#' - `codigo`: Código del establecimiento.
#' - `pais`: Código de país (ISO 3).
#' - `codigoMunicipio`: Código de municipio (para España).
#' - `nombreMunicipio`: Nombre del municipio (para ubicaciones no españolas).
#'
#' @return `TRUE`. Genera advertencias si:
#': los datos del establecimiento están incompletos o son inconsistentes.
#': los detalles del municipio no coinciden con los requisitos del país.
#': se proporcionan simultáneamente un código de establecimiento y detalles adicionales.
#'
#' @keywords internal
validar_establecimiento <- function(comunicacion, establecimiento) {
  df <- establecimiento

  for (i in 1:nrow(df)) {
    ubicacion <- sprintf('establecimiento (fila %d)', i)
    resto_instanciado  <- sum(!is_cell_empty(df[i, ]))
    if (is_cell_empty(df$tipo[i])) {
      warning(
        ubicacion,
        " -> Falta el campo 'tipo'. Consulta los c\u00f3digos mediante 'View(tipo_establecimiento)'."
      )
    } else {
      if (!(df$tipo[i] %in% tipo_establecimiento$codigo)) {
        warning(
          ubicacion,
          " -> El campo 'tipo' no es v\u00e1lido. Consulta los c\u00f3digos mediante 'View(tipo_establecimiento)'."
        )
      }
    }
    if (is_cell_empty(df$nombre[i])) {
      warning(ubicacion, " -> Falta el campo 'nombre'.")
    }
    validar_direccion(df[i, ], ubicacion)
  }
  TRUE
}
