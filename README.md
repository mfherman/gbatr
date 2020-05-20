
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gbatr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

gbatr is an interface to [Geosupport Dektop
Edition](https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-gde-home.page),
a batch geocoder for New York City created by the NYC Department of City
Planning. It enables very fast local geocoding of NYC addresses. This is
a fork of the [rGBAT16AB pacakge](https://github.com/gmculp/rGBAT16AB)
developed by GM Culp.

## Why gbatr?

  - fast
  - local
  - free
  - can be scripted

## Installation

Before you install gbatr, you must have Geosupport Desktop Edition
installed. Geosupport is a free geocoding application developed by the
NYC Department of City Planning. It is available for Windows and Linux
and can be downloaded from the [City Planning
website](https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-gde-home.page).
For now, gbatr has only been tested using the Windows 64-bit version of
Geosupport, so let me know if you’re having troubling installing on
Linux or 32-bit Windows.

When installing Geosupport Desktop, it is strongly recommended to use
the default installation path (`C:/Program Files/Geosupport Desktop
Edition`). If you install it in this location, building gbatr from
source should just work. If you can’t install it in this location, you
may have to do some additional work to build gbatr.

Once you have Geosupport Desktop Edition installed, the easiest way to
install gbatr is by using the [remotes
package](https://remotes.r-lib.org/):

``` r
remotes::install_github("mfherman/gbatr")
```

## Usage

Geocoding is the process of converting places or addresses to geographic
points, commonly latitiude and longitude or x and y coordinates. To use
gbatr to geocode addresses, first prepare a data frame that contains a
column of street addresses and a column with either zip codes or
boroughs. The zip or borough column must all zip codes or all boroughs,
not a mix of both.

Then, call the `gbat()` function and specify the input data frame, the
name of the the address column, the name of the zip or borough column,
whether you are using zip codes or boroughs, and which geocoded fields
you want to get back from the GBAT geocoder. This last argument is
optional, and if ommitted, all 203 fields from GBAT functions 1A, 1E,
and AP are returned.

``` r
library(gbatr)

za <- tibble::tribble(
  ~name,                 ~address,              ~borough,
  "Roberta's",           "261 Moore Street",    "Brooklyn",
  "L'Industrie",         "254 S 2nd Stret",     "Brooklyn",
  "Emmy Squared",        "364 Grand Street",    "Brooklyn",
  "Di Fara",             "1424 Avenue J",       "Brooklyn",
  "L&B Spumoni Gardens", "2725 86th Street",    "Brooklyn",
  "Totonno's",           "1524 Neptune Avenue", "Broklyn"
  )

za_geo <- gbat(
  za,
  address = "address",
  zip_boro = "borough",
  zip_boro_type = "boro",
  geo_colnames = "lot_centroid_latlon"
  )

za_geo
#> # A tibble: 6 x 5
#>   name                address             borough  F1A_Latitude F1A_Longitude
#>   <chr>               <chr>               <chr>    <chr>        <chr>        
#> 1 Roberta's           261 Moore Street    Brooklyn "40.705171"  "-73.934116" 
#> 2 L'Industrie         254 S 2nd Stret     Brooklyn "40.711481"  "-73.957848" 
#> 3 Emmy Squared        364 Grand Street    Brooklyn "40.712164"  "-73.955708" 
#> 4 Di Fara             1424 Avenue J       Brooklyn "40.624923"  "-73.961487" 
#> 5 L&B Spumoni Gardens 2725 86th Street    Brooklyn "40.594672"  "-73.981269" 
#> 6 Totonno's           1524 Neptune Avenue Broklyn  ""           ""
```

### Specifying return columns

### Dealing with failure

### GBAT functions

### Converting to `sf` objects and mapping

``` r
za_geo %>%
  sf::st_as_sf(
    coords = c("F1A_Longitude", "F1A_Latitude"),
    crs = 4326
    ) %>%
  mapview::mapview()
```
