# Attach packages
library(readxl)
library(tidyr)
library(readr)
library(dplyr)

# Read in data
population <- read_excel('data/Population Estimates IHME_2015.xlsx')
old_population <- read_excel('data/Population Estimates IHME_2015.xlsx')

# Make wide
results <- data.frame(country_number = unique(sort(population$location_id)))

# Loop through each row to make wide
sex_names <- sort(unique(population$sex_name))
age_group_names <- sort(unique(population$age_group_name))
new_age_groups <- new_age_groups <- c('014', '15plus')
for(s in sex_names){
  for(a in new_age_groups){
    results[,paste0(s, '_', a)] <- NA
  }
}

for (i in 1:nrow(results)){
  this_location <- results$country_number[i]
  for(s in sex_names){
    for(a in new_age_groups){
      
      if(a == '014'){
        these_age_names <- c('5-14 years', 'Under 5')
      } else if (a == '15plus'){
        these_age_names <- c('15-49 years', '50-69 years', '70+ years')
      }
      
      x <- population %>%
        filter(sex_name == s,
               age_group_name %in% these_age_names,
               location_id == this_location) %>%
        .$pop
      x <- sum(x)
      if(length(x) == 1){
        results[i,paste0(s, '_', a)] <- x
      }
    }
  }}
# Overwrite
population <- results; rm(results)

# Remove spaces in column names
names(population) <- gsub(' ', '', names(population))

# Make all names lowercase
names(population) <- tolower(names(population))

# Remove slashes
names(population) <- gsub('/', '_', names(population), fixed = TRUE)

# Remove trailing underscore
names(population) <- sub("_$","",names(population))

# Prepend with pop
names(population[2:ncol(population)]) <- 
  paste0('pop_', names(population)[2:ncol(population)])

# Remove dashes
names(population) <- gsub('-', '_', names(population), fixed = TRUE)

# Remove pluses
names(population) <- gsub('+', '_plus_', names(population), fixed = TRUE)

# Fix gender
names(population) <- gsub('female', 'f', names(population))
names(population) <- gsub('male', 'm', names(population))

# Append population
for (j in 2:ncol(population)){
  names(population)[j] <- 
    paste0('population_', 
           names(population)[j])
}
# Remove the junk
rm(a, age_group_names, i, s, sex_names, this_location, x)
