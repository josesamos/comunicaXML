

#' Remove Empty XML Nodes Recursively
#'
#' This function traverses an XML node and removes any empty child nodes.
#' A node is considered empty if it has no children, no text content, and no attributes.
#'
#' @param node An XML node of class `xml_node` from the `xml2` package.
#'
#' @return The function modifies the XML structure in place and does not return a value.
#'
#' @keywords internal
remove_empty_nodes <- function(node) {
  children <- xml2::xml_children(node)

  for (child in children) {
    remove_empty_nodes(child)

    if (xml2::xml_length(child) == 0 &&
        xml2::xml_text(child) == "" &&
        length(xml2::xml_attrs(child)) == 0) {
      xml2::xml_remove(child)
    }
  }
}
