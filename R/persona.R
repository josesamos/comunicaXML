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

    fnac_parent <- persona[persona$comunicacion_fk == k, c('fechaNacimiento', 'parentesco')]
    menores <- fnac_parent[!es_mayor_de_edad(fnac_parent$fechaNacimiento), 'fechaNacimiento'][[1]]
    def_parentesco <- fnac_parent[es_mayor_de_edad(fnac_parent$fechaNacimiento), 'parentesco'][[1]]
    def_parentesco <- def_parentesco[!is.na(def_parentesco)]
    if (length(menores) > 0 && length(def_parentesco) == 0) {
      warning("Si alguna de las personas es menor de edad, al menos una de las personas mayores de edad ha de tener informada su relación de parentesco con esta persona.")
    }
  }

  for (i in 1:nrow(df)) {
    ubicacion <- sprintf('persona (fila %d)', i)
    if (is.na(df$rol[i])) {
      warning(ubicacion,
              " -> Falta el campo 'rol'.")
    }
    if (is.na(df$nombre[i])) {
      warning(ubicacion,
              " -> Falta el campo 'nombre'.")
    }
    if (is.na(df$apellido1[i])) {
      warning(ubicacion,
              " -> Falta el campo 'apellido1'.")
    }
    if (is.na(df$fechaNacimiento[i])) {
      warning(ubicacion,
              " -> Falta la fecha de nacimiento.")
    } else {
      validar_fecha(df$fechaNacimiento[i], ubicacion)
      if (es_mayor_de_edad(df$fechaNacimiento[i])) {
        if (is.na(df$tipoDocumento[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'tipo_documento'. Consulta los códigos mediante 'View(tipo_documento)'.")
        }
        if (is.na(df$numeroDocumento[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'numeroDocumento'.")
        }
      }
    }
    if (!is.na(df$tipoDocumento[i])) {
      if (!(df$tipoDocumento[i] %in% tipo_documento$codigo)) {
        warning(ubicacion,
                " -> El campo 'tipo_documento' no es válido. Consulta los códigos mediante 'View(tipo_documento)'.")
      }
      if (df$tipoDocumento[i] == 'NIF') {
        if (is.na(df$apellido2[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'apellido2'. Obligatorio si el tipo de documento es NIF.")
        }
      }
      if (df$tipoDocumento[i] %in% c('NIF', 'NIE')) {
        if (is.na(df$soporteDocumento[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'soporteDocumento'. Obligatorio si el tipo de documento es NIF o NIE.")
        } else if (!grepl("^[A-Za-z0-9]{9}$", df$soporteDocumento[i])) {
          warning(ubicacion,
                  " -> El campo 'soporteDocumento' debe contener exactamente 9 caracteres alfanuméricos.")
        }
        validar_nif_nie(df$numeroDocumento[i], ubicacion)
      }
    }
    if (!is.na(df$nacionalidad[i])) {
      validar_pais(df$nacionalidad[i], ubicacion)
    }
    if (!is.na(df$sexo[i])) {
      if (!(df$sexo[i] %in% generos$codigo)) {
        warning(ubicacion,
                " -> El campo 'sexo' no es válido. Consulta los códigos mediante 'View(generos)'.")
      }
    }
    if (is.na(df$telefono[i]) && is.na(df$telefono2[i]) && is.na(df$correo[i])) {
      warning(ubicacion,
              " -> Obligatorio incluir una de estas tres etiquetas: 'telefono', 'telefono2' o 'correo'.")
    } else {
      if (!is.na(df$telefono[i])) {
        validar_telefono(df$telefono[i], ubicacion)
      }
      if (!is.na(df$telefono2[i])) {
        validar_telefono(df$telefono2[i], ubicacion)
      }
      if (!is.na(df$correo[i])) {
        validar_email(df$correo[i], ubicacion)
      }
    }
    if (!is.na(df$parentesco[i])) {
      if (!(df$parentesco[i] %in% parentesco$codigo)) {
        warning(ubicacion,
                " -> El campo 'parentesco' no es válido. Consulta los códigos mediante 'View(parentesco)'.")
      }
    }

    validar_direccion(df[i, ], ubicacion)
  }
  TRUE
}
