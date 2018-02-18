
<!-- README.md is generated from README.Rmd. Please edit that file -->
**UNDER CONSTRUCTION**

hidrowebdown
============

Downloading data from the major database of hydrological data in Brazil ([Hidroweb](http://hidroweb.ana.gov.br) - National Water Agency ([ANA](www.ana.gov.br)) from R.

Installation
------------

You can install hidrowebdown from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("lhmet/hidrowebdown")
```

Example
-------

Find a hydrological station.

``` r
library(hidrowebdown)
# metadata
head(hidro_metadata)
#>     Codigo Longitude Latitude Altitude AreaDrenagem                 Nome
#> 1 10011600  -78.4481  -1.5300     2460          223      PUELA AJ CHAMBO
#> 2 10012000  -78.1767  -1.4283     1150           NA   PASTAZA AJ ENCANTO
#> 3 10011200  -78.5494  -1.2364     2310          407 PACHANLICA AJ AMBATO
#> 4 10010900  -78.7731  -0.7758     3060           91  NEGRO AJ PUMANCUCHI
#> 5 10011300  -78.4211  -1.3897     1729         7983     PASTAZA EN BANÕS
#> 6 10011100  -78.6628  -1.2589     2680          763     AMBATO EN AMBATO
#>   TipoEstacao Municipio  Estado UF        Bacia
#> 1           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 2           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 3           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 4           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 5           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 6           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#>                       SubBacia         Rio
#> 1 RIO SOLIMÕES, JAVARI,ITACUAI RIO PASTAZA
#> 2 RIO SOLIMÕES, JAVARI,ITACUAI RIO PASTAZA
#> 3 RIO SOLIMÕES, JAVARI,ITACUAI RIO PASTAZA
#> 4 RIO SOLIMÕES, JAVARI,ITACUAI RIO PASTAZA
#> 5 RIO SOLIMÕES, JAVARI,ITACUAI RIO PASTAZA
#> 6 RIO SOLIMÕES, JAVARI,ITACUAI RIO PASTAZA
```

Get data.

``` r
# hidroweb_import()
```
