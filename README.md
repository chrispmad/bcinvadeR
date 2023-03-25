
# bcinvadeR

<!-- badges: start -->
<!-- badges: end -->

The goal of bcinvadeR is to facilitate your access to terrestrial or aquatic species occurrence data in British Columbia and your ability to make engaging Leaflet maps with them. I personally use these functions to search for invasive species occurrence data to then make maps in my job.

## Installation

You can install the development version of bcinvadeR like so:

``` r
remotes::install_github(https://github.com/chrispmad/bcinvadeR)
```

## Example

Let's say we wanted to grab occurrence data in B.C. for Goldfish, Northern Pike,
and the Common Wall Lizard.

``` r
library(bcinvadeR)
fish_dat = grab_aq_occ_data(common_names = c("goldfish","northern pike"))

liz_dat = grab_terr_occ_data(common_names = 'wall lizard', scientific_name = 'Podarcis muralis')

dat = dplyr::bind_rows(fish_dat, liz_dat)

# Let's make a map!
occdat_leafmap(dat = dat, location_variable = 'Location', bg_regions = 'regions')
```

