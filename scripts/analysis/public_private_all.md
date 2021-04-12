public\_private\_all
================
Millie Chapman
4/3/2021

``` r
library(sf)
```

    ## Linking to GEOS 3.8.1, GDAL 3.1.4, PROJ 6.3.1

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

richness all - fee richness all public = fee richness all private

``` r
fee_richness <- read_sf("../data/output/fee_richness_allpublic.shp") %>%
  mutate(area = st_area(geometry))
```

``` r
fee_richness_all <- as_tibble(fee_richness) %>%
  #filter(GAP_Sts == "1" |GAP_Sts == "2") %>%
  mutate(mmml_rc = replace_na(mmml_rc,0),
         rptl_rc = replace_na(rptl_rc,0),
         amphbn_ = replace_na(amphbn_,0),
         fsh_rch = replace_na(fsh_rch,0),
         brd_rch = replace_na(brd_rch,0)) %>%
  mutate(PA_CE = "public") %>%
  group_by(PA_CE) %>%
  mutate(total_area = sum(area)) %>%
  ungroup() %>%
  mutate(amphibians = amphbn_*area/total_area,
         birds = brd_rch*area/total_area,
         mammals = mmml_rc*area/total_area,
         fish = fsh_rch*area/total_area,
         reptiles = rptl_rc*area/total_area) %>%
  group_by(PA_CE) %>%
  summarise(amphibians = sum(amphibians),
         birds = sum(birds),
         mammals = sum(mammals),
         fish = sum(fish),
         reptiles = sum(reptiles),
         area = sum(area)) %>% ungroup() %>%
  mutate(amphibians = as.numeric(amphibians),
         birds = as.numeric(birds),
         mammals = as.numeric(mammals),
         fish = as.numeric(fish),
         reptiles = as.numeric(reptiles))
```

``` r
background_richness <- st_read("../data/output/background_richness.shp") %>%
  mutate(area = st_area(geometry))
```

    ## Reading layer `background_richness' from data source `/Users/milliechapman/Desktop/Berkeley/birdlife/data/output/background_richness.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 1 feature and 7 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -179.1506 ymin: 18.90986 xmax: 179.7734 ymax: 72.6875
    ## geographic CRS: WGS 84

``` r
public_private <- as_tibble(background_richness) %>%
  select(-GID_0) %>%
  rename(
        amphibians_all = "amphbn_",
         birds_all = "brd_rch",
         mammals_all = "mmml_rc",
         fish_all = "fsh_rch",
         reptiles_all = "rptl_rc",
        area_all = "area") %>%
  select(-geometry) %>%
  bind_cols(fee_richness_all)
```

``` r
private <- public_private %>%
  mutate(area_pr = area_all-area) %>%
  mutate(mammals= (mammals_all*area_all - mammals*area)/area_pr,
         reptiles= (reptiles_all*area_all - reptiles*area)/area_pr,
         amphibians= (amphibians_all*area_all - amphibians*area)/area_pr,
         fish= (fish_all*area_all - fish*area)/area_pr,
         birds= (birds_all*area_all - birds*area)/area_pr) %>%
  select(PA_CE:reptiles) %>%
  mutate(PA_CE = "private") %>%
  mutate(amphibians = as.numeric(amphibians),
         birds = as.numeric(birds),
         mammals = as.numeric(mammals),
         fish = as.numeric(fish),
         reptiles = as.numeric(reptiles))
```

``` r
public_private <- private %>%
  bind_rows(fee_richness_all) %>%
  select(-area) %>%
  pivot_longer(-PA_CE) %>%
  rename(type = "PA_CE") %>% ungroup()
```

``` r
write_csv(public_private, "../data/private_public_richness.csv", append = FALSE)
```

``` r
private_public_richness1 <- read_csv("../data/private_public_richness.csv") 
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   type = col_character(),
    ##   name = col_character(),
    ##   value = col_double()
    ## )
