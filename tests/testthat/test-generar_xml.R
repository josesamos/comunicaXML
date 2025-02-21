test_that("generar_xml works", {
  hoja_calculo <- system.file("extdata", "partes_viajeros.xlsx", package = "comunicaXML")
  archivo_xml <- tempfile(fileext = ".xml")

  archivo <- generar_xml(hoja_calculo, archivo_xml)

  original_xml <- system.file("extdata", "partes_viajeros_generados.xml", package = "comunicaXML")

  doc1 <- xml2::read_xml(original_xml)
  doc2 <- xml2::read_xml(archivo)

  doc1 <- xml2::xml_ns_strip(doc1)
  doc2 <- xml2::xml_ns_strip(doc2)

  xml1_text <- as.character(doc1)
  xml2_text <- as.character(doc2)

  expect_equal(xml1_text, xml2_text)
})
