% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/municipio.R
\name{validar_codigo_postal}
\alias{validar_codigo_postal}
\title{Validate a Spanish Postal Code}
\usage{
validar_codigo_postal(codigo_postal)
}
\arguments{
\item{codigo_postal}{A character string representing the postal code to validate.}
}
\value{
`TRUE` if the postal code is valid, `FALSE` otherwise.
}
\description{
Checks if a given postal code is valid in Spain. It must be a 5-digit numeric string
where the first two digits correspond to valid Spanish provinces (01 to 52).
}
\keyword{internal}
