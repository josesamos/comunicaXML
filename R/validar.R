


validar_hoja_calculo<- function(hoja_calculo) {
  if (!file.exists(hoja_calculo)) {
    stop("El archivo no existe: ", hoja_calculo)
  }

  extension <- tools::file_ext(hoja_calculo)
  sheets_data <- list()

  if (extension == "xlsx") {
    sheet_names <- readxl::excel_sheets(hoja_calculo)
    sheets_data <- lapply(sheet_names, function(sheet)
      readxl::read_excel(hoja_calculo, sheet = sheet))

  } else if (extension == "ods") {
    sheet_names <- readODS::ods_sheets(hoja_calculo)
    sheets_data <- lapply(seq_along(sheet_names), function(sheet)
      readODS::read_ods(hoja_calculo, sheet = sheet))

  } else {
    stop("Formato no compatible. Utilice un archivo .xlsx o .ods.")
  }
  names(sheets_data) <- tolower(sheet_names)

  validar_comunicacion(sheets_data$comunicacion)

  validar_establecimiento(sheets_data$comunicacion, sheets_data$establecimiento)

}


validar_comunicacion <- function(comunicacion) {
  pk <- 'comunicacion_pk'
  if (!pk %in% names(comunicacion)) {
    stop("La columna 'comunicacion_pk' no existe en la hoja 'comunicacion'.")
  }
  valores <- comunicacion[[pk]]
  df <- comunicacion

  if (!(all(!is.na(valores)) && length(unique(valores)) == nrow(df))) {
    stop("La columna 'comunicacion_pk' en la hoja 'comunicacion' no puede tener valores NA o duplicados.")
  }
  TRUE
}

validar_referencia <- function(comunicacion, otra, nombre) {
  pk <- 'comunicacion_pk'
  fk <- 'comunicacion_fk'
  if (!fk %in% names(otra)) {
    stop("La columna 'comunicacion_fk' no existe en la hoja '",
         nombre,
         "'.")
  }

  valores <- otra[[fk]]
  com <- comunicacion[[pk]]
  if (!all(valores %in% com)) {
    stop(
      "Todos los valores de la columna 'comunicacion_fk' en la hoja '",
      nombre,
      "' han de estar en la columna 'comunicacion_pk' de la hoja 'comunicacion'."
    )
  }
  TRUE
}

validar_pais <- function(pais, ubicacion) {
  if (!(pais %in% iso_codes$alfa3)) {
    stop(ubicacion, " -> El código de país '",
         pais,
         "' no es válido. Consulta los códigos mediante 'View(iso_codes)'.")
  }
  TRUE
}

validar_municipio <- function(codigo_municipio, ubicacion) {
  if (!(codigo_municipio %in% municipios$codigo)) {
    stop(ubicacion, " -> El código de municipio '",
         codigo_municipio,
         "' no es válido. Consulta los códigos mediante 'View(municipios)'.")
  }
  TRUE
}

validar_fecha <- function(fecha, ubicacion){
  # Intentar convertir la fecha a tipo Date
  fecha_convertida <- as.character(as.Date(fecha, format = "%Y-%m-%d"))

  # Comprobar si la conversión fue exitosa y la fecha resultante es igual a la original
  if (is.na(fecha_convertida) | fecha_convertida != fecha) {
    stop(ubicacion, " -> La fecha '",
         fecha,
         "' no es válida, ha de ser 'aaaa-mm-dd'.")
  }
}

validar_fecha_tarjeta <- function(fecha, ubicacion){
}

validar_establecimiento <- function(comunicacion, establecimiento) {
  validar_referencia(comunicacion, establecimiento, "establecimiento")
  df <- establecimiento

  for (i in 1:nrow(df)) {
    resto_instanciado  <- sum(!is.na(df[i, ]))
    if (is.na(df$codigo[i])) {
      if (resto_instanciado < 7) {
        stop("Faltan datos del establecimiento en la fila ", i, ".")
      }
      validar_pais(df$pais[i], sprintf('establecimiento (fila %d)', i))
      if (df$pais[i] == 'ESP') {
        validar_municipio(df$codigoMunicipio[i],
                          sprintf('establecimiento (fila %d)', i))
        if (!is.na(df$nombreMunicipio[i])) {
          stop(
            sprintf('establecimiento (fila %d)', i),
            " -> Si el país es España (ESP), el código de municipio ha de ir informado, no así el nombre del municipio."
          )
        }
      } else {
        if ((!is.na(df$codigoMunicipio[i])) | is.na(df$nombreMunicipio[i])) {
          stop(
            sprintf('establecimiento (fila %d)', i),
            " -> Si el país es distinto de España (ESP), ha de informar el nombre del municipio, no así el código de municipio."
          )
        }
      }
    } else {
      if (resto_instanciado > 2) {
        stop("Ha de indicar los datos del establecimiento o bien el código de establecimiento.")
      }
    }
  }
  TRUE
}
