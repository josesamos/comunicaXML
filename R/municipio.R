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
