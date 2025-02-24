
<!-- README.md is generated from README.Rmd. Please edit that file -->

# comunicaXML: Comunicación masiva en formato XML <a href="https://josesamos.github.io/comunicaXML/"><img src="man/figures/logo.png" align="right" height="139" alt="comunicaXML website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/josesamos/comunicaXML/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/josesamos/comunicaXML/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/josesamos/comunicaXML/graph/badge.svg)](https://app.codecov.io/gh/josesamos/comunicaXML)
<!-- badges: end -->

El objetivo de `comunicaXML` es facilitar la generación del archivo en
formato XML para realizar el alta masiva de comunicaciones en el
[Sistema de
Hospedajes](https://sede.mir.gob.es/opencms/export/sites/default/es/procedimientos-y-servicios/hospedajes-y-alquiler-de-vehiculos/).

## Instalación

Puedes instalar la versión de desarrollo de `comunicaXML` desde
[GitHub](https://github.com/) con:

``` r
# install.packages("pak")
pak::pak("josesamos/comunicaXML")
```

## Ejemplo

Este es un ejemplo que muestra cómo usar la funcionalidad que ofrece
`comunicaXML`. El flujo de trabajo propuesto es el siguiente:

1.  Obtener la plantilla (un archivo en formato .ods o .xlsx).
2.  Completar los datos en la hoja de cálculo.
3.  Validar el archivo: detectar posibles errores
4.  Generar el archivo XML.

### Obtener la plantilla

En primer lugar, obtenemos una plantilla de la hoja de cálculo con la
que vamos a trabajar. Usamos la función `obtener_hoja_calculo()` a la
que le pasamos el nombre y la ubicación donde queremos se cree el
archivo (p.e., `c:/datos/mis_datos.xlsx`). Para el ejemplo siguiente,
para que se pueda ejecutar aquí, le pasamos una ubicación temporal.

``` r
library(comunicaXML)

mis_datos <- file.path(tempdir(), "mis_datos.xlsx")

achivo <- obtener_hoja_calculo(mis_datos)
```

A continuación se muestra el contenido de las hojas del archivo
obtenido.

#### Hoja: solicitud

| solicitud_pk | codigoEstablecimiento |
|:-------------|:----------------------|
| 1            | NA                    |

#### Hoja: comunicacion

| comunicacion_pk | comentario        | solicitud_fk |
|----------------:|:------------------|:-------------|
|               1 | Comunicación nº 1 | 1            |

#### Hoja: contrato

| referencia | fechaContrato | fechaEntrada | fechaSalida | numPersonas | numHabitaciones | internet | tipoPago | fechaPago | medioPago | titular | caducidadTarjeta | comunicacion_fk |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|
| NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | 1 |

#### Hoja: persona

| rol | nombre | apellido1 | apellido2 | tipoDocumento | numeroDocumento | soporteDocumento | fechaNacimiento | nacionalidad | sexo | direccion | direccionComplementaria | codigoMunicipio | nombreMunicipio | codigoPostal | pais | telefono | telefono2 | correo | parentesco | comunicacion_fk |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|
| NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | NA | 1 |

Las hojas están relacionadas mediante los campos con los sufijos `_pk` y
`_fk`. Hemos de procurar mantener esas relaciones al añadir nueva filas.

### Completar los datos

Completamos los las hojas de la plantilla con nuestros datos. A
continuación se muestran unos datos de ejemplo.

#### Hoja: solicitud

| solicitud_pk | codigoEstablecimiento |
|:-------------|:----------------------|
| 1            | A123456789            |

#### Hoja: comunicacion

| comunicacion_pk | comentario        | solicitud_fk |
|----------------:|:------------------|:-------------|
|               1 | Comunicación nº 1 | 1            |

#### Hoja: contrato

| referencia | fechaContrato | fechaEntrada | fechaSalida | numPersonas | numHabitaciones | internet | tipoPago | fechaPago | medioPago | titular | caducidadTarjeta | comunicacion_fk |
|:---|:---|:---|:---|---:|---:|:---|:---|:---|:---|:---|:---|---:|
| X123ABC | 2025-02-20 | 2025-02-21T00:00:00 | 2025-02-23T00:00:00 | 2 | 1 | true | TARJT | 2025-02-20 | Tarjeta | Nombre Apellido1 Apellido2 | 01/2029 | 1 |

#### Hoja: persona

| rol | nombre | apellido1 | apellido2 | tipoDocumento | numeroDocumento | soporteDocumento | fechaNacimiento | nacionalidad | sexo | direccion | direccionComplementaria | codigoMunicipio | nombreMunicipio | codigoPostal | pais | telefono | telefono2 | correo | parentesco | comunicacion_fk |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|
| TI | Nombre1 | Apellido11 | Apellido21 | NIF | 11111111H | ABC123456 | 1980-01-01 | ESP | H | Dirección 1 | Complemento 1 | 18001 | NA | 18001 | ESP | 111111111 | 211111111 | <correo1@correo.es> | NA | 1 |
| VI | Nombre2 | Apellido12 | Apellido22 | NIF | 22222222J | ABC123457 | 1980-01-02 | ESP | M | Dirección 2 | NA | 18002 | NA | 18002 | ESP | 222222222 | NA | <correo2@correo.es> | NA | 1 |
| VI | Nombre3 | Apellido13 | Apellido23 | NIF | 33333333P | ABC123458 | 1980-01-03 | ESP | O | Dirección 3 | Complemento 3 | 18003 | NA | 18003 | ESP | 333333333 | 233333333 | <correo3@correo.es> | NA | 1 |
| VI | Nombre4 | Apellido14 | Apellido24 | NIF | 44444444A | ABC123459 | 1980-01-04 | ESP | NA | Dirección 4 | NA | 18004 | NA | 18004 | ESP | 444444444 | NA | <correo4@correo.es> | NA | 1 |

### Validar el archivo

Podemos validar nuestro archivo mediante la función
`validar_hoja_calculo()` que comprueba las relaciones entre las hojas,
los contenidos de los campos en función de las restricciones de formato
y dependencias que se definen en la documentación del [Sistema de
Hospedajes](https://sede.mir.gob.es/opencms/export/sites/default/es/procedimientos-y-servicios/hospedajes-y-alquiler-de-vehiculos/).

``` r
validar_hoja_calculo(achivo)
#> [1] TRUE
```

Para facilitar la corrección de errores, se muestran avisos de los
problemas detectados. En este caso, no hay ningún problema.

### Generar el archivo XML

Cuando consideremos que el archivo está correcto, podemos generar el
archivo XML mediante la función `generar_xml()`. Indicamos el nombre del
archivo de nuestra hoja de cálculo y el nombre del archivo XML que
queremos generar (p.e., `c:/datos/resultado.xml`), en este caso, para
que se pueda ejecutar aquí, le pasamos también una ubicación temporal
para el archivo resultado. Si indicamos el parámetro `optimizar = TRUE`
elimina los nodos vacíos del archivo XML obtenido.

``` r
resultado_xml <- file.path(tempdir(), "resultado.xml")

archivo_2 <- generar_xml(achivo, resultado_xml, optimizar = TRUE)
```

A continuación se muestra el contenido del archivo que hemos obtenido.

``` r
contenido <- readLines(archivo_2, warn = FALSE)

cat("```xml\n", paste(contenido, collapse = "\n"), "\n```", sep = "")
```

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<ns2:peticion xmlns:ns2="http://www.neg.hospedajes.mir.es/altaReservaHospedaje">
  <solicitud>
    <codigoEstablecimiento>A123456789</codigoEstablecimiento>
    <comunicacion>
      <!--Comunicación nº 1-->
      <contrato>
        <referencia>X123ABC</referencia>
        <fechaContrato>2025-02-20</fechaContrato>
        <fechaEntrada>2025-02-21T00:00:00</fechaEntrada>
        <fechaSalida>2025-02-23T00:00:00</fechaSalida>
        <numPersonas>2</numPersonas>
        <numHabitaciones>1</numHabitaciones>
        <internet>true</internet>
        <pago>
          <tipoPago>TARJT</tipoPago>
          <fechaPago>2025-02-20</fechaPago>
          <medioPago>Tarjeta</medioPago>
          <titular>Nombre Apellido1 Apellido2</titular>
          <caducidadTarjeta>01/2029</caducidadTarjeta>
        </pago>
      </contrato>
      <persona>
        <rol>TI</rol>
        <nombre>Nombre1</nombre>
        <apellido1>Apellido11</apellido1>
        <apellido2>Apellido21</apellido2>
        <tipoDocumento>NIF</tipoDocumento>
        <numeroDocumento>11111111H</numeroDocumento>
        <soporteDocumento>ABC123456</soporteDocumento>
        <fechaNacimiento>1980-01-01</fechaNacimiento>
        <nacionalidad>ESP</nacionalidad>
        <sexo>H</sexo>
        <direccion>
          <direccion>Dirección 1</direccion>
          <direccionComplementaria>Complemento 1</direccionComplementaria>
          <codigoMunicipio>18001</codigoMunicipio>
          <codigoPostal>18001</codigoPostal>
          <pais>ESP</pais>
        </direccion>
        <telefono>111111111</telefono>
        <telefono2>211111111</telefono2>
        <correo>correo1@correo.es</correo>
      </persona>
      <persona>
        <rol>VI</rol>
        <nombre>Nombre2</nombre>
        <apellido1>Apellido12</apellido1>
        <apellido2>Apellido22</apellido2>
        <tipoDocumento>NIF</tipoDocumento>
        <numeroDocumento>22222222J</numeroDocumento>
        <soporteDocumento>ABC123457</soporteDocumento>
        <fechaNacimiento>1980-01-02</fechaNacimiento>
        <nacionalidad>ESP</nacionalidad>
        <sexo>M</sexo>
        <direccion>
          <direccion>Dirección 2</direccion>
          <codigoMunicipio>18002</codigoMunicipio>
          <codigoPostal>18002</codigoPostal>
          <pais>ESP</pais>
        </direccion>
        <telefono>222222222</telefono>
        <correo>correo2@correo.es</correo>
      </persona>
      <persona>
        <rol>VI</rol>
        <nombre>Nombre3</nombre>
        <apellido1>Apellido13</apellido1>
        <apellido2>Apellido23</apellido2>
        <tipoDocumento>NIF</tipoDocumento>
        <numeroDocumento>33333333P</numeroDocumento>
        <soporteDocumento>ABC123458</soporteDocumento>
        <fechaNacimiento>1980-01-03</fechaNacimiento>
        <nacionalidad>ESP</nacionalidad>
        <sexo>O</sexo>
        <direccion>
          <direccion>Dirección 3</direccion>
          <direccionComplementaria>Complemento 3</direccionComplementaria>
          <codigoMunicipio>18003</codigoMunicipio>
          <codigoPostal>18003</codigoPostal>
          <pais>ESP</pais>
        </direccion>
        <telefono>333333333</telefono>
        <telefono2>233333333</telefono2>
        <correo>correo3@correo.es</correo>
      </persona>
      <persona>
        <rol>VI</rol>
        <nombre>Nombre4</nombre>
        <apellido1>Apellido14</apellido1>
        <apellido2>Apellido24</apellido2>
        <tipoDocumento>NIF</tipoDocumento>
        <numeroDocumento>44444444A</numeroDocumento>
        <soporteDocumento>ABC123459</soporteDocumento>
        <fechaNacimiento>1980-01-04</fechaNacimiento>
        <nacionalidad>ESP</nacionalidad>
        <direccion>
          <direccion>Dirección 4</direccion>
          <codigoMunicipio>18004</codigoMunicipio>
          <codigoPostal>18004</codigoPostal>
          <pais>ESP</pais>
        </direccion>
        <telefono>444444444</telefono>
        <correo>correo4@correo.es</correo>
      </persona>
    </comunicacion>
  </solicitud>
</ns2:peticion>
```

Este es el archivo que podemos subir al sistema para realizar el alta
masiva de comunicaciones.
