test_that("remove_empty_nodes removes empty nodes", {

  xml_str <- '<root>
                <keep>Data</keep>
                <empty1></empty1>
                <empty2 attr="value"></empty2>
                <empty3>
                  <child></child>
                </empty3>
              </root>'

  doc <- xml2::read_xml(xml_str)

  remove_empty_nodes(doc)

  xml_result <- as.character(doc)

  expect_false(grepl("<empty1>", xml_result), info = "empty1 should be removed")
  expect_true(grepl("<empty2", xml_result), info = "empty2 should not be removed (has attribute)")
  expect_false(grepl("<child>", xml_result), info = "child inside empty3 should be removed")
  expect_false(grepl("<empty3>", xml_result), info = "empty3 should be removed")
  expect_true(grepl("<keep>", xml_result), info = "keep node should remain")
})
