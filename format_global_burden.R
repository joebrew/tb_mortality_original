library(tidyr)
library(readxl)
library(dplyr)

# Read in data
gb <- read_excel('data/Global burden public excel WHO.xlsx')

# Remove irrelevant vars and keep only 2015
gb <- gb %>%
  filter(year == 2015) %>%
  dplyr::select(-country,
                -iso2,
                -iso_numeric,
                -g_whoregion)


# Remove all hi/lo vars
gb <- gb[,!grepl('hi|lo', names(gb))]

# Get rid of year (all 2015)
gb <- gb %>% dplyr::select(-year)

# Prepend all with gb
names(gb) <- 
  if_else(names(gb) == 'iso3',
          names(gb),
          paste0('gb_', names(gb)))

# Make gb_c_cdr numeric
gb$gb_c_cdr <- as.numeric(as.character(gb$gb_c_cdr))
