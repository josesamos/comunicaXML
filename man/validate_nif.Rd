% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dni.R
\name{validate_nif}
\alias{validate_nif}
\title{Validate a Spanish NIF (National Identification Number)}
\usage{
validate_nif(dni)
}
\arguments{
\item{dni}{A character string representing the NIF to validate.}
}
\value{
`TRUE` if the NIF is valid, `FALSE` otherwise.
}
\description{
It checks if a given Spanish NIF (Documento Nacional de Identidad) is valid.
It verifies both the format (8 digits followed by an uppercase letter) and the
control letter, which is computed using the remainder of the division of the
number by 23.
}
\keyword{internal}
