library(dplyr)
library(readr)

# Read data
outcomes <- read_csv('data/TB_outcomes_2017-01-11.csv')

# Get an average of last three years
outcomes3 <- outcomes %>%
  filter(year %in% 2012:2014) %>%
  group_by(iso3) %>%
  summarise(newrel_died_avg_2012_to_2014 = mean(newrel_died, na.rm = TRUE), 
            ret_nrel_died_avg_2012_to_2014 = mean(ret_nrel_died, na.rm = TRUE),
            ret_nrel_coh_avg_2012_to_2014 = mean(ret_nrel_coh, na.rm = TRUE),
            newrel_coh_avg_2012_to_2014 = mean(newrel_coh, na.rm = TRUE))

# Keep only years of interest
outcomes <-
  outcomes %>%
  filter(year == 2014) # 2015 not yet available

# Join with the 3 year average
outcomes <- 
  left_join(x = outcomes,
            y = outcomes3,
            by = 'iso3')

# Keep only variables of interest
outcomes <- 
  outcomes %>%
  dplyr::select(iso3, newrel_died, ret_nrel_died,
                ret_nrel_coh,
                newrel_coh,
                newrel_died_avg_2012_to_2014,
                ret_nrel_died_avg_2012_to_2014,
                ret_nrel_coh_avg_2012_to_2014,
                newrel_coh_avg_2012_to_2014)

# Calculate new variable for Alberto
outcomes <- outcomes %>%
  mutate(case_fatality_rate_2014 = 
           (newrel_died + 
              ret_nrel_died ) /  
           ( ret_nrel_coh + 
               newrel_coh ),
         case_fatality_rate_2012_to_2014 = 
           (newrel_died_avg_2012_to_2014 +
           ret_nrel_died_avg_2012_to_2014) / 
           ( ret_nrel_coh_avg_2012_to_2014 +
               newrel_coh_avg_2012_to_2014  )
           )

# Those countries which have a greater than 1 absolute percentage points
# difference between their 2014 and estimated 2012-2014 case fatality rates,
# we take the latter. If the difference is less than a percentage point, we take the former in our estimates for 2015
outcomes <- outcomes %>%
  mutate(the_difference = abs(case_fatality_rate_2014 - case_fatality_rate_2012_to_2014)) %>%
  mutate(case_fatality_rate_2015 = 
           ifelse(the_difference >= 0.01,
                  case_fatality_rate_2012_to_2014,
                  case_fatality_rate_2014)) %>%
  dplyr::select(-the_difference)

# Create a case fatality rate only among new cases.
# AS with the case fatality rate, conditionally take years
# We are calling 2015 our "final" estimated number
outcomes <- outcomes %>%
  mutate(case_fatality_rate_2014_new = 
           newrel_died  /  newrel_coh ,
         case_fatality_rate_2012_to_2014_new = 
           newrel_died_avg_2012_to_2014  / 
           newrel_coh_avg_2012_to_2014)
outcomes <- outcomes %>%
  mutate(the_difference = abs(case_fatality_rate_2014_new - case_fatality_rate_2012_to_2014_new)) %>%
  mutate(case_fatality_rate_2015_new = 
           ifelse(the_difference >= 0.01,
                  case_fatality_rate_2012_to_2014_new,
                  case_fatality_rate_2014_new)) %>%
  dplyr::select(-the_difference)


# 10 countries are missing number of deaths among retreatment cases not including relapse.
# For those 10 countries we use the number of (deaths new / number new) [case_fatality_rate_new] multiplied
# by the ratio of case_fatality_rate_2015_new / case_fatality_rate_2015
adjustment_factor <- mean(outcomes$case_fatality_rate_2015 /  outcomes$case_fatality_rate_2015_new, na.rm = TRUE)


fix_these <- is.na(outcomes$ret_nrel_died & !is.na(outcomes$newrel_died))
outcomes$case_fatality_rate_2015_adjusted <- outcomes$case_fatality_rate_2015
outcomes$case_fatality_rate_2015_adjusted[fix_these] <- outcomes$case_fatality_rate_2015_new[fix_these] * adjustment_factor
