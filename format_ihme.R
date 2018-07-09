# Attach packages
library(readxl)
library(tidyr)
library(readr)
library(dplyr)

# Read in data
ihme <- read_csv('data/IHME DATASET 24112016.csv')
old_ihme <- ihme

# Re-organize so that the data are in a 
# one row per country format, with columns inidcating other metrics
# (ie, go from long to wide)

# This would result in the following number of columns

# Get the individual values on which we're spreading the data
location_names <- unique(ihme$location_name)
location_ids <- unique(ihme$location_id)

measure_names <- unique(ihme$measure_name)
sex_names <- unique(ihme$sex_name)
age_names <- unique(ihme$age_name)
cause_names <- unique(ihme$cause_name)
metric_names <- unique(ihme$metric_name)

# Create new dataframe
wide <- data.frame(location_name = location_names,
                   location_id = location_ids)

for(measure_name in measure_names){
  for(sex_name in sex_names){
    for(age_name in age_names){
      for(cause_name in cause_names){
        for(metric_name in metric_names){
          wide[,paste(measure_name, '_',
                      sex_name, '_',
                      age_name, '_',
                      cause_name, '_',
                      metric_name, '_')] <- NA
        }
      }
    }
  }
}

n_wide <- nrow(wide)
for (i in 1:n_wide){
  message(paste0('Row ', i, ' of ', n_wide))
  this_location_name <- wide$location_name[i]
  for(measure_name in measure_names){
    for(sex_name in sex_names){
      for(age_name in age_names){
        for(cause_name in cause_names){
          for(metric_name in metric_names){
            this_row <-
              ihme[which(ihme$measure_name == measure_name &
                           ihme$sex_name == sex_name &
                           ihme$age_name == age_name &
                           ihme$cause_name == cause_name &
                           ihme$metric_name == metric_name &
                           ihme$location_name == this_location_name),]
            this_value <- this_row$val
            if(length(this_value) > 0){
              wide[i,paste(measure_name, '_',
                           sex_name, '_',
                           age_name, '_',
                           cause_name, '_',
                           metric_name, '_')] <- this_value
            }
            
          }
        }
      }
    }
  }
}

# Overwrite
long <- ihme
ihme <- wide; rm(wide)


# Create new, who-compatible age groups
new_age_groups <- c('014', '15plus')
for(measure_name in measure_names){
  for(sex_name in sex_names){
    for(cause_name in cause_names){
      for(metric_name in metric_names){
        for(new_age_group in new_age_groups)
          ihme[,paste(measure_name, '_',
                      sex_name, '_',
                      new_age_group, '_',
                      cause_name, '_',
                      metric_name, '_')] <- NA
      }
    }
  }
}

n_ihme <- nrow(ihme)
for (i in 1:n_ihme){
  message(paste0('Row ', i, ' of ', n_ihme))
  this_location_name <- ihme$location_name[i]
  for(measure_name in measure_names){
    for(sex_name in sex_names){
      for(cause_name in cause_names){
        for(metric_name in metric_names){
          for(new_age_group in new_age_groups){
            
            if(new_age_group == '014'){
              these_age_names <- c('5-14 years', 'Under 5')
            } else if (new_age_group == '15plus'){
              these_age_names <- c('15-49 years', '50-69 years', '70+ years')
            }
            
            these_rows <-
              old_ihme[which(old_ihme$measure_name == measure_name &
                               old_ihme$sex_name == sex_name &
                               old_ihme$age_name %in% these_age_names &
                               old_ihme$cause_name == cause_name &
                               old_ihme$metric_name == metric_name &
                               old_ihme$location_name == this_location_name),] %>%
              mutate(sex_name = tolower(sex_name)) %>%
              rename(age_group_name = age_name)
            
            # Get the population too
            pop <-  old_population[old_population$sex_name == tolower(sex_name) & 
                                     old_population$location_name == this_location_name &
                                     old_population$age_group_name %in% these_age_names,] %>%
              dplyr::select(sex_name, location_name, age_group_name, pop)
            these_rows <- 
              left_join(these_rows, pop,
                        by = c("location_name", "sex_name", "age_group_name"))
            
            
            
            if(measure_name == 'Deaths' &
               metric_name == 'Number'){
              this_value <- sum(these_rows$val, na.rm = TRUE)
            } else if(measure_name == 'Deaths' &
                      metric_name == 'Rate'){
              this_value  <- weighted.mean(these_rows$val, w = these_rows$pop)
            } else if(measure_name == 'Incidence' &
                      metric_name == 'Number'){
              this_value  <- weighted.mean(these_rows$val, w = these_rows$pop)
            } else if(measure_name == 'Incidence' &
                      metric_name == 'Rate'){
              this_value  <- weighted.mean(these_rows$val, w = these_rows$pop)
            }
            
            if(length(this_value) > 0){
              ihme[i,paste(measure_name, '_',
                           sex_name, '_',
                           new_age_group, '_',
                           cause_name, '_',
                           metric_name, '_')] <- this_value
            }
            
          }
        }
      }
    }
  }
}


# Delete all the old age groups
for(measure_name in measure_names){
  for(sex_name in sex_names){
    for(age_name in age_names[age_names != 'All Ages']){ # not deleting the all ages group
      for(cause_name in cause_names){
        for(metric_name in metric_names){
          ihme[,paste(measure_name, '_',
                      sex_name, '_',
                      age_name, '_',
                      cause_name, '_',
                      metric_name, '_')] <- NULL
        }
      }
    }
  }
}

# Remove spaces in column names
names(ihme) <- gsub(' ', '', names(ihme))

# Make all names lowercase
names(ihme) <- tolower(names(ihme))

# Remove slashes
names(ihme) <- gsub('/', '_', names(ihme), fixed = TRUE)

# Remove dashes
names(ihme) <- gsub('-', '_', names(ihme), fixed = TRUE)

# Remove pluses
names(ihme) <- gsub('+', '_plus_', names(ihme), fixed = TRUE)

# Remove trailing underscore
names(ihme) <- sub("_$","",names(ihme))

# Prepend with ihme
names(ihme) <- paste0('ihme_', names(ihme))

# # Get the country linkage name
# ihme$country <- ihme$ihme_location_name

# Remove all incidence related variables
# ihme <- ihme[,!grepl('incidence', names(ihme))]

ihme$country_number <- ihme$ihme_location_id

# Create a total ihme incidence
ihme$ihme_incidence_both_allages_totaltb_number <-
  ihme$ihme_incidence_both_allages_tuberculosis_number +
  ihme$ihme_incidence_both_allages_hiv_aids_tuberculosis_number

# Make column names compatible with who
names(ihme) <- gsub('hiv_aids', 'h', names(ihme))
names(ihme) <- gsub('male', 'm', names(ihme))
names(ihme) <- gsub('female', 'f', names(ihme))
names(ihme) <- gsub('_fem_', '_f_', names(ihme))
names(ihme) <- gsub('_tuberculosis_', '', names(ihme))
names(ihme) <- gsub('number', '_number', names(ihme))
names(ihme) <- gsub('rate', '_rate', names(ihme))
names(ihme) <- gsub('__', '_', names(ihme))

# Remove the global indicator
ihme <-
  ihme %>% filter(!ihme_location_name %in% 'Global')

# # Remove any incidence number column
# remove_these <- which(grepl('incidence', names(ihme)) &
#                         grepl('number', names(ihme)))
# ihme <- ihme[,!(1:ncol(ihme)) %in% remove_these]

# Remove all the excess junk
z <- unlist(lapply(ls(), function(x){class(get(x))[1]}))
files <- ls()
for (i in 1:length(z)){
  if(!z[i] %in% c('data.frame', 'tbl_df')){
    rm(list = files[i], envir = .GlobalEnv)
  }
}
rm(files, i)
rm(#linkage, 
  this_row, z)

