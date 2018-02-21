
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

You can choose a hydrological station looking at the data `hidro_metadata`.

``` r
library(hidrowebdown)
# metadata
head(hidro_metadata)
#>     Codigo Longitude Latitude Altitude AreaDrenagem                 Nome
#> 1 10011600  -78.4481  -1.5300     2460          223      PUELA AJ CHAMBO
#> 2 10012000  -78.1767  -1.4283     1150           NA   PASTAZA AJ ENCANTO
#> 3 10011200  -78.5494  -1.2364     2310          407 PACHANLICA AJ AMBATO
#> 4 10010900  -78.7731  -0.7758     3060           91  NEGRO AJ PUMANCUCHI
#> 5 10011300  -78.4211  -1.3897     1729         7983     PASTAZA EN BANOS
#> 6 10011100  -78.6628  -1.2589     2680          763     AMBATO EN AMBATO
#>   TipoEstacao Municipio  Estado UF        Bacia
#> 1           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 2           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 3           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 4           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 5           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#> 6           1   EQUADOR EQUADOR EQ RIO AMAZONAS
#>                       SubBacia         Rio
#> 1 RIO SOLIMOES, JAVARI,ITACUAI RIO PASTAZA
#> 2 RIO SOLIMOES, JAVARI,ITACUAI RIO PASTAZA
#> 3 RIO SOLIMOES, JAVARI,ITACUAI RIO PASTAZA
#> 4 RIO SOLIMOES, JAVARI,ITACUAI RIO PASTAZA
#> 5 RIO SOLIMOES, JAVARI,ITACUAI RIO PASTAZA
#> 6 RIO SOLIMOES, JAVARI,ITACUAI RIO PASTAZA
```

To download data, for some stations:

``` r
stns <- c("42650000", "00252001")
opts <- c("Vazao", "Chuva")
ctl <- hidroweb_down(stations = stns, options = opts, 
                     verbose = TRUE, dest.dir = "../", meta = TRUE)
#> -----------------------------------------------
#> > 42650000: Vazao
#> elapsed 
#>   2.081
#> Station: 42650000 also have data of:
#> Cotas
#> Warning in .hydroweb_down_station(.x, .y, dest.dir, verbose, metadata = meta): No data was found for station 42650000, option Vazao.
#> -----------------------------------------------
#> > 42650000: Chuva
#> elapsed 
#>   2.145
#> Station: 42650000 also have data of: 
#> Cotas
#> Warning in .hydroweb_down_station(.x, .y, dest.dir, verbose, metadata = meta): No data was found for station 42650000, option Chuva.
#> -----------------------------------------------
#> > 00252001: Vazao
#> elapsed 
#>   2.051
#> Station: 00252001 also have data of:
#> Chuvas
#> Warning in .hydroweb_down_station(.x, .y, dest.dir, verbose, metadata = meta): No data was found for station 00252001, option Vazao.
#> -----------------------------------------------
#> > 00252001: Chuva
#> elapsed 
#>  43.477
#> File saved in 
#>  /home/pqgfapergs1/Dropbox/github/my_reps/lhmet/00252001_Chuva.zip.
ctl
#> # A tibble: 4 x 14
#>   station    lon    lat   alt  area name  state city  river basin subbasin
#>   <chr>    <dbl>  <dbl> <dbl> <dbl> <chr> <chr> <chr> <chr> <chr> <chr>   
#> 1 42650000 -46.1 -17.1    505  1240 BARR… MINA… JOAO… RIO … RIO … RIOS SA…
#> 2 42650000 -46.1 -17.1    505  1240 BARR… MINA… JOAO… RIO … RIO … RIOS SA…
#> 3 00252001 -52.9 - 2.34    NA    NA ITAP… PARA  PORT… <NA>  RIO … RIO AMA…
#> 4 00252001 -52.9 - 2.34    NA    NA ITAP… PARA  PORT… <NA>  RIO … RIO AMA…
#> # ... with 3 more variables: options <chr>, cboTipoReg <int>, file <chr>
```
