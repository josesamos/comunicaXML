
validar_establecimiento <- function(comunicacion, establecimiento) {

  validate_fk(comunicacion, "comunicacion", establecimiento, "establecimiento")

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
        if ((!is.na(df$codigoMunicipio[i])) || is.na(df$nombreMunicipio[i])) {
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
