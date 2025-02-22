#' Validar datos del establecimiento
#'
#' Comprueba si los datos del establecimiento proporcionados están estructurados correctamente y son coherentes.
#' Comprueba que:
#': la clave externa de establecimiento existe en la tabla de comunicación.
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
  validate_fk(comunicacion, "comunicacion", establecimiento, "establecimiento")

  df <- establecimiento

  for (i in 1:nrow(df)) {
    resto_instanciado  <- sum(!is.na(df[i, ]))
    if (is.na(df$codigo[i])) {
      if (resto_instanciado < 7) {
        warning(sprintf('establecimiento (fila %d)', i),
                " -> Faltan datos.")
      } else {
        if (length(df$tipo[i]) > 0) {
          if (!(df$tipo[i] %in% tipo_establecimiento$codigo)) {
            warning(
              sprintf('establecimiento (fila %d)', i),
              " -> El campo 'tipo' no es válido. Consulta los códigos mediante 'View(tipo_establecimiento)'.."
            )
          }
        } else {
          warning(
            sprintf('establecimiento (fila %d)', i),
            " -> Falta el campo 'tipo'. Consulta los códigos mediante 'View(tipo_establecimiento)'.."
          )
        }

        validar_pais(df$pais[i], sprintf('establecimiento (fila %d)', i))
        if (df$pais[i] == 'ESP') {
          validar_municipio(df$codigoMunicipio[i],
                            sprintf('establecimiento (fila %d)', i))
          if (!is.na(df$nombreMunicipio[i])) {
            warning(
              sprintf('establecimiento (fila %d)', i),
              " -> Si el país es España (ESP), el código de municipio ha de ir informado, no así el nombre del municipio."
            )
          }
        } else {
          if ((!is.na(df$codigoMunicipio[i])) ||
              is.na(df$nombreMunicipio[i])) {
            warning(
              sprintf('establecimiento (fila %d)', i),
              " -> Si el país es distinto de España (ESP), ha de informar el nombre del municipio, no así el código de municipio."
            )
          }
        }
      }
    } else {
      if (resto_instanciado > 2) {
        warning(
          sprintf('establecimiento (fila %d)', i),
          " -> Ha de indicar los datos del establecimiento o bien el código de establecimiento."
        )
      }
    }
  }
  TRUE
}
