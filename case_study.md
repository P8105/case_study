Case Study
================

## Get the data

``` r
library(p8105.datasets)

data(nyc_airbnb)

nyc_airbnb = 
  nyc_airbnb %>% 
  rename(borough = neighbourhood_group) %>% 
  mutate(stars = review_scores_location / 2)
```

## Brainstorm questions

## Attempt solutions
