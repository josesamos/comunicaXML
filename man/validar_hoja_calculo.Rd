% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validar.R
\name{validar_hoja_calculo}
\alias{validar_hoja_calculo}
\title{Validar una hoja de cálculo con datos de comunicación}
\usage{
validar_hoja_calculo(hoja_calculo, pdf = TRUE)
}
\arguments{
\item{hoja_calculo}{Ruta al archivo de hoja de cálculo en formato `.xlsx` o `.ods`.}

\item{pdf}{Booleano. Sigue el modelo del archivo pdf o del ejemplo xml.}
}
\value{
Devuelve `TRUE` si la validación se completa sin errores. En caso de problemas,
        se generan errores (`stop`) o advertencias (`warning`).
}
\description{
Valida una hoja de cálculo que contiene datos estructurados de comunicaciones,
establecimientos, contratos y personas. Asegura que el archivo existe, que tiene
una extensión válida (`.xlsx` o `.ods`) y que los datos dentro de las hojas cumplen
con los requisitos de validación.
}
\examples{
hoja_calculo <- system.file("extdata", "pdf/partes_viajeros.xlsx", package = "comunicaXML")

res <- validar_hoja_calculo(hoja_calculo)

}
