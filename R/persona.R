#' Validar datos de personas en la comunicación
#'
#' Valida los datos de las personas asociadas a una comunicación, asegurando que
#' los campos obligatorios estén presentes y que la información proporcionada sea consistente.
#' Genera advertencias en caso de datos faltantes o incorrectos.
#'
#' @param comunicacion Data frame con los datos de la comunicación.
#' @param persona Data frame con los datos de las personas asociadas a la comunicación.
#'
#' @details
#' - Verifica que el campo `rol` solo tome los valores "TI" (Titular) o "VI" (Viajero).
#' - Asegura que al menos una persona tenga el rol de Titular (TI).
#' - Si hay menores de edad, al menos un adulto debe tener informado su parentesco con ellos.
#' - Valida la presencia de campos obligatorios como `nombre`, `apellido1` y `fechaNacimiento`.
#' - Si la persona es mayor de edad, se verifica la existencia de `tipoDocumento` y `numeroDocumento`.
#' - Comprueba que `tipoDocumento` tenga un valor válido de `tipo_documento$codigo`.
#' - Si el `tipoDocumento` es "NIF", el campo `apellido2` es obligatorio.
#' - Si el `tipoDocumento` es "NIF" o "NIE", el campo `soporteDocumento` debe existir y tener 9 caracteres alfanuméricos.
#' - Valida que `sexo`, `nacionalidad` y `parentesco` contengan valores válidos según las tablas de códigos.
#' - Se debe proporcionar al menos un dato de contacto: `telefono`, `telefono2` o `correo`.
#' - Se realizan validaciones sobre dirección, teléfono, email y documento de identidad.
#'
#' @return `TRUE` si todas las validaciones se completan sin problemas, de lo contrario, emite advertencias (`warning`).
#'
#' @keywords internal
validar_persona <- function(comunicacion, persona) {
  df <- persona

  for (k in comunicacion$comunicacion_pk) {
    bloque <- persona[persona$comunicacion_fk == k, 'rol'][[1]]
    if (!all(bloque %in% c("TI", "VI"))) {
      warning("El campo 'rol' solo puede tomar los valores 'TI' o 'VI'.")
    }
    titulares <- sum(bloque == "TI")
    if (is_cell_empty(titulares) || titulares == 0) {
      warning("Debe haber una persona con el rol Titular (TI) obligatoriamente. Las personas con el rol de viajero (VI) son opcionales.")
    }

    fnac_parent <- persona[persona$comunicacion_fk == k, c('fechaNacimiento', 'parentesco')]
    menores <- fnac_parent[!es_mayor_de_edad(fnac_parent$fechaNacimiento), "fechaNacimiento"]
    if (!is.null(menores) && is.data.frame(menores) && nrow(menores) > 0) {
      menores <- menores[[1]]
    } else {
      menores <- character(0)
    }

    def_parentesco <- fnac_parent[es_mayor_de_edad(fnac_parent$fechaNacimiento), 'parentesco']
    if (!is.null(def_parentesco) && is.data.frame(def_parentesco) && nrow(def_parentesco) > 0) {
      def_parentesco <- def_parentesco[[1]]
      def_parentesco <- def_parentesco[!is_cell_empty(def_parentesco)]
    } else {
      def_parentesco <- character(0)
    }
    if (length(menores) > 0 && length(def_parentesco) == 0) {
      warning("Si alguna de las personas es menor de edad, al menos una de las personas mayores de edad ha de tener informada su relaci\u00f3n de parentesco con esta persona.")
    }
  }

  for (i in 1:nrow(df)) {
    ubicacion <- sprintf('persona (fila %d)', i)
    if (is_cell_empty(df$rol[i])) {
      warning(ubicacion,
              " -> Falta el campo 'rol'.")
    }
    if (is_cell_empty(df$nombre[i])) {
      warning(ubicacion,
              " -> Falta el campo 'nombre'.")
    }
    if (is_cell_empty(df$apellido1[i])) {
      warning(ubicacion,
              " -> Falta el campo 'apellido1'.")
    }
    if (is_cell_empty(df$fechaNacimiento[i])) {
      warning(ubicacion,
              " -> Falta la fecha de nacimiento.")
    } else {
      validar_fecha(df$fechaNacimiento[i], ubicacion)
      if (es_mayor_de_edad(df$fechaNacimiento[i])) {
        if (is_cell_empty(df$tipoDocumento[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'tipo_documento'. Consulta los c\u00f3digos mediante 'View(tipo_documento)'.")
        }
        if (is_cell_empty(df$numeroDocumento[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'numeroDocumento'.")
        }
      }
    }
    if (!is_cell_empty(df$tipoDocumento[i])) {
      if (!(df$tipoDocumento[i] %in% tipo_documento$codigo)) {
        warning(ubicacion,
                " -> El campo 'tipo_documento' no es v\u00e1lido. Consulta los c\u00f3digos mediante 'View(tipo_documento)'.")
      }
      if (df$tipoDocumento[i] == 'NIF') {
        if (is_cell_empty(df$apellido2[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'apellido2'. Obligatorio si el tipo de documento es NIF.")
        }
      }
      if (df$tipoDocumento[i] %in% c('NIF', 'NIE')) {
        if (is_cell_empty(df$soporteDocumento[i])) {
          warning(ubicacion,
                  " -> Falta el campo 'soporteDocumento'. Obligatorio si el tipo de documento es NIF o NIE.")
        } else if (!grepl("^[A-Za-z0-9]{9}$", df$soporteDocumento[i])) {
          warning(ubicacion,
                  " -> El campo 'soporteDocumento' debe contener exactamente 9 caracteres alfanum\u00e9ricos.")
        }
        validar_nif_nie(df$numeroDocumento[i], ubicacion)
      }
    }
    if (!is_cell_empty(df$nacionalidad[i])) {
      validar_pais(df$nacionalidad[i], ubicacion)
    }
    if (!is_cell_empty(df$sexo[i])) {
      if (!(df$sexo[i] %in% generos$codigo)) {
        warning(ubicacion,
                " -> El campo 'sexo' no es v\u00e1lido. Consulta los c\u00f3digos mediante 'View(generos)'.")
      }
    }
    if (is_cell_empty(df$telefono[i]) && is_cell_empty(df$telefono2[i]) && is_cell_empty(df$correo[i])) {
      warning(ubicacion,
              " -> Obligatorio incluir una de estas tres etiquetas: 'telefono', 'telefono2' o 'correo'.")
    } else {
      if (!is_cell_empty(df$telefono[i])) {
        validar_telefono(df$telefono[i], ubicacion)
      }
      if (!is_cell_empty(df$telefono2[i])) {
        validar_telefono(df$telefono2[i], ubicacion)
      }
      if (!is_cell_empty(df$correo[i])) {
        validar_email(df$correo[i], ubicacion)
      }
    }
    if (!is_cell_empty(df$parentesco[i])) {
      if (!(df$parentesco[i] %in% parentesco$codigo)) {
        warning(ubicacion,
                " -> El campo 'parentesco' no es v\u00e1lido. Consulta los c\u00f3digos mediante 'View(parentesco)'.")
      }
    }

    validar_direccion(df[i, ], ubicacion)
  }
  TRUE
}
