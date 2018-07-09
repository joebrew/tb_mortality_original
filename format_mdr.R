library(dplyr)
library(readr)

# Read data
mdr <- read_csv('data/MDR_RR_TB_burden_estimates_2016-12-20.csv')

# Keep only years of interest
mdr <-
  mdr %>%
  filter(year == 2015)

# Keep only variables of interest
mdr <- 
  mdr %>%
  dplyr::select(iso3, e_rr_pct_new)
