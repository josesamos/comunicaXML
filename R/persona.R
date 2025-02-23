#'
#' @keywords internal
validar_persona <- function(comunicacion, persona) {
  validate_fk(comunicacion, "comunicacion", persona, "persona")

  df <- persona

  for (k in comunicacion$comunicacion_pk) {
    bloque <- persona[persona$comunicacion_fk == k, 'rol'][[1]]
    if (!all(bloque %in% c("TI", "VI"))) {
      warning("El campo 'rol' solo puede tomar los valores 'TI' o 'VI'.")
    }
    titulares <- sum(bloque == "TI")
    if (titulares == 0) {
      warning("Debe haber una persona con el rol Titular (TI) obligatoriamente. Las personas con el rol de viajero (VI) son opcionales.")
    }
  }

  for (i in 1:nrow(df)) {
    if (is.na(df$rol[i])) {
      warning(sprintf('persona (fila %d)', i),
              " -> Falta el campo 'rol'.")
    }
    if (is.na(df$nombre[i])) {
      warning(sprintf('persona (fila %d)', i),
              " -> Falta el campo 'nombre'.")
    }
    if (is.na(df$apellido1[i])) {
      warning(sprintf('persona (fila %d)', i),
              " -> Falta el campo 'apellido1'.")
    }
    if (is.na(df$fechaNacimiento[i])) {
      warning(sprintf('persona (fila %d)', i),
              " -> Falta la fecha de nacimiento.")
    } else {
      validar_fecha(df$fechaNacimiento[i], sprintf('persona (fila %d)', i))
      if (es_mayor_de_edad(df$fechaNacimiento[i])) {
        if (is.na(df$tipoDocumento[i])) {
          warning(sprintf('persona (fila %d)', i),
                  " -> Falta el campo 'tipo_documento'. Consulta los códigos mediante 'View(tipo_documento)'.")
        }
        if (is.na(df$numeroDocumento[i])) {
          warning(sprintf('persona (fila %d)', i),
                  " -> Falta el campo 'numeroDocumento'.")
        }
      }
    }
    if (!is.na(df$tipoDocumento[i])) {
      if (!(df$tipoDocumento[i] %in% tipo_documento$codigo)) {
        warning(sprintf('persona (fila %d)', i),
                " -> El campo 'tipo_documento' no es válido. Consulta los códigos mediante 'View(tipo_documento)'.")
      }
      if (df$tipoDocumento[i] == 'NIF') {
        if (is.na(df$apellido2[i])) {
          warning(sprintf('persona (fila %d)', i),
                  " -> Falta el campo 'apellido2'. Obligatorio si el tipo de documento es NIF.")
        }
      }
      if (df$tipoDocumento[i] %in% c('NIF', 'NIE')) {
        if (is.na(df$soporteDocumento[i])) {
          warning(sprintf('persona (fila %d)', i),
                  " -> Falta el campo 'soporteDocumento'. Obligatorio si el tipo de documento es NIF o NIE.")
        } else if (!grepl("^[A-Za-z0-9]{9}$", df$soporteDocumento[i])) {
          warning(sprintf('persona (fila %d)', i),
                  " -> El campo 'soporteDocumento' debe contener exactamente 9 caracteres alfanuméricos.")
        }
        validar_nif_nie(df$numeroDocumento[i], sprintf('persona (fila %d)', i))
      }
    }
    if (!is.na(df$nacionalidad[i])) {
      validar_pais(df$nacionalidad[i], sprintf('persona (fila %d)', i))
    }
    if (!is.na(df$sexo[i])) {
      if (!(df$sexo[i] %in% generos$codigo)) {
        warning(sprintf('persona (fila %d)', i),
                " -> El campo 'sexo' no es válido. Consulta los códigos mediante 'View(generos)'.")
      }
    }







  }
  TRUE
}
