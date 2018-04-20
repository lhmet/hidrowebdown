
<!-- README.md is generated from README.Rmd. Please edit that file -->
**DISCONTINUED PACKAGE** after recent changes in the hidroweb site.

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

You can choose a hydrological station looking at the data `hidroweb_metadata`.

``` r
library(hidrowebdown)
# metadata
head(hidroweb_metadata)
#>   station      lon     lat alt area        name        city state uf
#> 1   47000 -47.5500 -0.6500  14   NA SALINOPOLIS SALINOPOLIS  PARA PA
#> 2   47001 -47.5000 -0.8333  68   NA    MARACANA    MARACANA  PARA PA
#> 3   47002 -47.3536 -0.6231  NA   NA SALINOPOLIS SALINOPOLIS  PARA PA
#> 4   47003 -47.8536 -0.7375  NA   NA      CURUCA      CURUCA  PARA PA
#> 5   47004 -47.0994 -0.9294  NA   NA   PRIMAVERA   PRIMAVERA  PARA PA
#> 6   47005 -47.6583 -0.6336  NA   NA      MARUDA   MARAPANIM  PARA PA
#>                             basin                     subbasin river
#> 1 ATLANTICO,TRECHO NORTE/NORDESTE RIOS GURUPI,TURIACU E OUTROS  <NA>
#> 2 ATLANTICO,TRECHO NORTE/NORDESTE RIOS GURUPI,TURIACU E OUTROS  <NA>
#> 3 ATLANTICO,TRECHO NORTE/NORDESTE RIOS GURUPI,TURIACU E OUTROS  <NA>
#> 4 ATLANTICO,TRECHO NORTE/NORDESTE RIOS GURUPI,TURIACU E OUTROS  <NA>
#> 5 ATLANTICO,TRECHO NORTE/NORDESTE RIOS GURUPI,TURIACU E OUTROS  <NA>
#> 6 ATLANTICO,TRECHO NORTE/NORDESTE RIOS GURUPI,TURIACU E OUTROS  <NA>
#>          class Escala RegistradorNivel DescLiquida Sedimentos QualAgua
#> 1 Pluviometric      0                0           0          0        0
#> 2 Pluviometric      0                0           0          0        0
#> 3 Pluviometric      0                0           0          0        0
#> 4 Pluviometric      0                0           0          0        0
#> 5 Pluviometric      0                0           0          0        0
#> 6 Pluviometric      0                0           0          0        0
#>   Pluviometro RegistradorChuva TanqueEvapo Climatologica Piezometria
#> 1           0                0           0             0           0
#> 2           1                0           0             0           0
#> 3           1                0           0             0           0
#> 4           1                0           0             0           0
#> 5           1                0           0             0           0
#> 6           1                0           0             0           0
#>   Telemetrica
#> 1           0
#> 2           0
#> 3           1
#> 4           1
#> 5           0
#> 6           0
comment(hidroweb_metadata)
#> [1] "Last update on Hidroweb in: 2018-02-09"
```

To download data, for some stations:

``` r
stns <- c("42650000", "00252001")
opts <- c("Vazao", "Chuva")
down_stns <- hidroweb_down(stations = stns, 
                     options = opts, 
                     verbose = TRUE, 
                     dest.dir = "../", 
                     meta = TRUE)
#> -----------------------------------------------
#> > 42650000: Vazao
#> elapsed 
#>   2.498
#> Station: 42650000 also has data of:
#> Cotas
#> Warning in .hydroweb_down_station(.x, .y, dest.dir, verbose, metadata = meta): There is no link to download requested data of station 42650000, option Vazao.
#> -----------------------------------------------
#> > 42650000: Chuva
#> elapsed 
#>   2.284
#> Station: 42650000 also has data of: 
#> Cotas
#> Warning in .hydroweb_down_station(.x, .y, dest.dir, verbose, metadata = meta): There is no link to download requested data of station 42650000, option Chuva.
#> -----------------------------------------------
#> > 00252001: Vazao
#> elapsed 
#>   1.749
#> Station: 00252001 also has data of:
#> Chuvas
#> Warning in .hydroweb_down_station(.x, .y, dest.dir, verbose, metadata = meta): There is no link to download requested data of station 00252001, option Vazao.
#> -----------------------------------------------
#> > 00252001: Chuva
#> elapsed 
#>  40.083
#> File saved in 
#>  /home/hidrometeorologista/Dropbox/github/my_reps/lhmet/00252001_Chuva.zip.
down_stns
#> # A tibble: 4 x 13
#>   station  option file      lon    lat   alt  area name  state city  river
#>   <chr>    <chr>  <chr>   <dbl>  <dbl> <dbl> <dbl> <chr> <chr> <chr> <chr>
#> 1 42650000 Vazao  <NA>    -46.1 -17.1    505  1240 BARR… MINA… JOAO… RIO …
#> 2 42650000 Chuva  <NA>    -46.1 -17.1    505  1240 BARR… MINA… JOAO… RIO …
#> 3 00252001 Vazao  <NA>    -52.9 - 2.34    NA    NA ITAP… PARA  PORT… <NA> 
#> 4 00252001 Chuvas /home/… -52.9 - 2.34    NA    NA ITAP… PARA  PORT… <NA> 
#> # ... with 2 more variables: basin <chr>, subbasin <chr>
```
