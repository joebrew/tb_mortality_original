library(dplyr)
library(readr)

# Read data
notifications <- read_csv('data/TB_notifications_2016-12-20.csv')

# Keep only years of interest
notifications <-
  notifications %>%
  filter(year == 2015)

# Keep only columns of interest
notifications <- notifications %>%
  dplyr::select(iso3, c_newinc,
                newrel_hivpos,
                newrel_hivtest,
                ret_nrel,
                conf_rrmdr,
                newrel_f014,
                newrel_m014) %>%
  rename(mdr_rr = conf_rrmdr)
