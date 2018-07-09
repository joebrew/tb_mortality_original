library(readr)
library(dplyr)
options(scipen = '999')
# Format all data
if('formatted_data.RData' %in% dir('data')){
  load('data/formatted_data.RData')
} else {
  source('format_population_data.R')
  source('format_who.R')
  source('format_ihme.R')
  save.image('data/formatted_data.RData')
}

# Read in the linkage file
linkage <- read_csv('data/ISO_Country_Link.csv') %>%
  rename(country_name = `COUNTRY NAME`)
linkage <- data.frame(linkage)

# Combine all data
df <- who %>%
  left_join(linkage,
            by = 'iso3') %>%
# Get IHME data for those with a country number
  full_join(ihme,
            by = 'country_number') %>%
  left_join(population,
            by = 'country_number') %>%
  # Create a have_both variable
  mutate(have_who = !is.na(who_mort_h_number),
         have_ihme = !is.na(ihme_deaths_f_allages_h_number)) %>%
  mutate(have_both = have_who & have_ihme)
df <- data.frame(df)
df$country_name <- as.character(df$country_name)
df$iso3 <- as.character(df$iso3)

# Get missing country names if relevant
for (i in 1:nrow(df)){
  if(is.na(df$country_name[i])){
    df$country_name[i] <-
      linkage$country_name[which(linkage$country_number == df$country_number[i])][1]
    df$iso3[i] <-
      linkage$iso3[which(linkage$country_number == df$country_number[i])][1]
  }
}

# Manually add Taiwan to Western Pacific region
df$who_g_whoregion[df$iso3 == 'TWN'] <- 'WPR'

# Clean up names
names(df) <- 
  gsub('mort', 'deaths', names(df))

# Remove se and lo/hi
df <- df[,!grepl('_lo|_hi|_se', names(df))]

# Remove commas from country names
df$country_name <- gsub(',', '', df$country_name)
df <- data.frame(df)
df <- df[,unique(c('country_name', 'country_number', 'iso3',
                   'who_g_whoregion', 'who_year', names(df)))]

# Remove all population columns from WHO
df <- df[,!grepl('pop', names(df))]

# Fix one pesky name
names(df)[names(df) == 'who_g_whoregion'] <- 'who_region'

# Remove the year
df$who_year <- NULL


# Standardize per Alberto Garcia Basteiro's naming schema
# who = w
# ihme = i
# nd = number of deaths
# mr = mortality rate
# htb = hiv related to tb
# tb = only tb
albertify <- function(x){
  new_name <- x
  x_split <- unlist(strsplit(x, split = '_'))
  if(x_split[1] %in% c('ihme', 'who')){
    
    # Source
    source <- x_split[1]
    if(source == 'ihme'){
      source <- 'i'
    } else if(source == 'who'){
      source <- 'w'
    } else {
      source <- 'PROBLEMWITHSOURCE'
    }
    
    # Sex
    sex <- x_split[x_split %in% c('m', 'f', 'both')]
    if(length(sex) == 0){
      sex <- 'both'
    }
    
    # Age
    age <- x_split[x_split %in% c('014', '15plus', 'allages')]
    if(length(age) == 0){
      age <- 'all'
    }
    if(age == 'allages'){
      age <- 'all'
    }
    
    # Hiv
    hiv <- x_split[x_split %in% c('h', 'nh')]
    if(length(hiv) == 0){
      hiv <- 'nh'
    }
    if(hiv == 'h'){
      hiv <- 'htb'
    } else {
      hiv <- 'tb'
    }
    
    # rate_number
    rate_number <- x_split[x_split %in% c('number', 'rate', 'incidence')]

    if('incidence' %in% rate_number &
       'number' %in% rate_number){
      rate_number <- 'incidence_number'
    } else {
      if('incidence' %in% rate_number &
         'rate' %in% rate_number){
        rate_number <- 'incidence_rate'
      } else if('incidence' %in% rate_number){
      rate_number <- 'in'
    } else {
      if(length(rate_number) == 0){
        rate_number <- 'number'
      } 
      if(rate_number == 'number'){
        rate_number <- 'nd'
      } else {
        rate_number <- 'mr'
      }
    }
  }
    
    # Combine together
    new_name <- 
      paste0(c(source,
               # indicator,
               sex,
               age,
               hiv,
               rate_number),
             collapse = '_')
    
  }
  return(new_name)
}


okay_to_change <- which(!names(df) %in% 
                          c('country_name',
                            'country_number',
                            'iso3',
                            'who_region') &
                          !grepl('gb_', names(df)))
okay_to_change <- okay_to_change[okay_to_change != which(names(df) == 'ihme_incidence_both_allages_totaltb_number')]
df <- data.frame(df)
for (j in okay_to_change){
  if(!grepl('have', names(df)[j])){
    the_old_name <- names(df)[j]
    the_new_name <- albertify(the_old_name)
    # print(the_old_name)
    # print(the_new_name)
    if(the_old_name == the_new_name){
      message(paste0('Did not change variable ', the_old_name))
    } else {
      # messaC          the_new_name))
      names(df)[j] <-
        the_new_name
    }
  }
}

# Make specific changes
names(df)[which(names(df) == 'country_name')] <- 'country'

# Create a prevalence survey variable
df$prevsurvey <- 0
has_prev_survey <- c(
  161, 6, 178, 179, 207, 206,
  11, 180, 10, 12, 15, 38, 182,
  13, 214, 165, 16, 185, 522, 18,
  189, 190, 20, 191, 198)
df$prevsurvey[df$country_number %in% has_prev_survey] <- 1

# Define which countries have high tb
df$hightb <- 0
has_high_tb <- c(
  168, 161, 135, 169, 6, 171, 170, 179,
  11, 163, 180, 10, 210, 194, 15, 184,
  195, 214, 165, 16, 26, 62, 217, 18, 189, 20,
  196, 191, 198)
df$hightb[df$country_number %in% has_high_tb] <- 1

# Define which countries have high mdr
df$highmdr <- 0
has_high_mdr <- c(168, 34, 161, 57, 6, 171, 179, 11,
                  163, 36, 180, 37, 61, 15, 184, 214,
                  165, 123, 16, 26, 7, 62, 187, 18,
                  39, 63, 41, 20, 196, 198)
df$highmdr[df$country_number %in% has_high_mdr] <- 1

# CREATE NEW VARIABLES
# Use format: source_sex_age_disease_indicator
# Alberto's stata code in comments

# # Create variable ALL TB DEATHS WHO
# egen wndalltb = rowtotal (wndtb_all wndhtb_all)
df$w_both_all_tbtotal_nd <- df$w_both_all_htb_nd + df$w_both_all_tb_nd
df$w_both_014_tbtotal_nd <- df$w_both_014_htb_nd + df$w_both_014_tb_nd
# *Create variable ALL TB DEATHS by IHME (i need to destring first).
# egen indalltb = rowtotal (indtb_all indhtb_all)
df$i_both_all_tbtotal_nd <- df$i_both_all_htb_nd + df$i_both_all_tb_nd
df$i_both_014_tbtotal_nd <- df$i_both_014_htb_nd + df$i_both_014_tb_nd

# Format variables from Global burden public
# and join to df
source('format_global_burden.R')
df <- df[,!duplicated(names(df))]
df <-
  df %>%
  left_join(gb,
            by = 'iso3')

# Bring in variables from notifications
source('format_notifications.R')
df <-
  df %>%
  left_join(notifications,
            by = 'iso3')

# Bring in variables from outcomes
source('format_outcomes.R')
df <- df %>%
  left_join(outcomes,
            by = 'iso3')

# Bring in variable from mdr
source('format_mdr.R')
df <-
  df %>%
  left_join(mdr,
            by = 'iso3')


# Apply this case fatality rate to the total cases of 2015 
# (which is the addition of c_newinc + ret_nrel ).
df$estimated_fatalities_2015 <-
  df$case_fatality_rate_2015_adjusted * 
  (df$c_newinc + df$ret_nrel)

df$estimated_fatalities_2015_using_2012_to_2014_data <-
  df$case_fatality_rate_2012_to_2014 * 
  (df$c_newinc + df$ret_nrel)

# Create a proportion of hiv cases among tb cases
df$p_hiv_of_tb <-
  df$newrel_hivpos / df$newrel_hivtes * 100

# Rename the proportion of mdr among new cases
df$p_mdr_new <- 
  df$e_rr_pct_new

# Get the absolute difference in total
# hiv+tb deaths betwen ihme and who
df$absolute_difference <- 
  abs(df$w_both_all_tbtotal_nd - 
        df$i_both_all_tbtotal_nd)

# And max number of deaths (hiv+tb) between the two
df$max_deaths <-
  if_else(df$w_both_all_tbtotal_nd >= df$i_both_all_tbtotal_nd,
          df$w_both_all_tbtotal_nd,
          ifelse(df$w_both_all_tbtotal_nd < df$i_both_all_tbtotal_nd,
                 df$i_both_all_tbtotal_nd,
                 NA))

# Get the relative difference, using the max_deaths as the baseline
df <- df %>%
  mutate(relative_difference = absolute_difference / max_deaths)

# Get relative difference : ihme over who
df$i_over_w <- 
  df$i_both_all_tbtotal_nd / 
  df$w_both_all_tbtotal_nd
  
# Get relative difference : who over ihme
df$w_over_i <- 
  df$w_both_all_tbtotal_nd / 
  df$i_both_all_tbtotal_nd


# Get absolute differences
df$w_minus_i <-
  df$w_both_all_tbtotal_nd -
  df$i_both_all_tbtotal_nd

df$i_minus_w <-
  df$i_both_all_tbtotal_nd - 
  df$w_both_all_tbtotal_nd


# Calculate the Frank Kobbelens metric
# Number of reported cases divided by deaths
# Use: c_newinc
df$i_cases_over_deaths <-
  df$c_newinc / df$i_both_all_tbtotal_nd
df$w_cases_over_deaths <-
  df$c_newinc / df$w_both_all_tbtotal_nd

# Calculate standardized difference
# per Alberto's method
df <- df %>%
  mutate(stand_w = w_both_all_tbtotal_nd / estimated_fatalities_2015,
         stand_i = i_both_all_tbtotal_nd / estimated_fatalities_2015,
         stand_dif= (w_both_all_tbtotal_nd-i_both_all_tbtotal_nd) / estimated_fatalities_2015) %>%
  # Children
  mutate(stand_child_w = w_both_014_tbtotal_nd / estimated_fatalities_2015,
         stand_child_i = i_both_014_tbtotal_nd / estimated_fatalities_2015,
         stand_dif_child= (w_both_014_tbtotal_nd-i_both_014_tbtotal_nd) / estimated_fatalities_2015) %>%
  # HIV children
  mutate(stand_child_h_w = w_both_014_htb_nd / estimated_fatalities_2015,
         stand_child_h_i = i_both_014_htb_nd / estimated_fatalities_2015,
         stand_dif_child_h= (w_both_014_htb_nd-i_both_014_htb_nd) / estimated_fatalities_2015) %>%
  # HIV all ages
  mutate(stand_h_w = w_both_all_htb_nd / estimated_fatalities_2015,
         stand_h_i = i_both_all_htb_nd / estimated_fatalities_2015,
         stand_dif_h= (w_both_all_htb_nd-i_both_all_htb_nd) / estimated_fatalities_2015)
  

# REPLACE WITH MARTIEN'S
df$original_stand_dif <- df$stand_dif
df$original_stand_dif_child <- df$stand_dif_child
df$original_stand_dif_child_h <- df$stand_dif_child_h
df$original_stand_dif_h <- df$stand_dif_h
df$stand_dif <- NA
df$stand_dif_child <- NA
df$stand_dif_child_h <- NA
df$stand_dif_h <- NA
for (i in 1:nrow(df)){
  
  # ALL
  df$stand_dif[i] <-
    # Difference of estimates
    (df$w_both_all_tbtotal_nd[i] - 
       df$i_both_all_tbtotal_nd[i]) / 
    # Divided by mean estimate
    mean(c(df$w_both_all_tbtotal_nd[i], 
           df$i_both_all_tbtotal_nd[i]))
  
  # CHILDREN
  df$stand_dif_child[i] <-
    # Difference of estimates
    (df$w_both_014_tbtotal_nd[i] -
       df$i_both_014_tbtotal_nd[i]) /
    # Divided by mean estimate
    mean(c(df$w_both_014_tbtotal_nd[i],
           df$i_both_014_tbtotal_nd[i]))
  
  # HIV CHILDREN
  df$stand_dif_child_h[i] <-
    # Difference of estimates
    (df$w_both_014_htb_nd[i] -
       df$i_both_014_htb_nd[i]) /
    # Divided by mean estimate
    mean(c(df$w_both_014_htb_nd[i],
           df$i_both_014_htb_nd[i]))
  
  # All ages hiv
  # HIV CHILDREN
  df$stand_dif_h[i] <-
    # Difference of estimates
    (df$w_both_all_htb_nd[i] -
       df$i_both_all_htb_nd[i]) /
    # Divided by mean estimate
    mean(c(df$w_both_all_htb_nd[i],
           df$i_both_all_htb_nd[i]))
}


# Fix the infs
df$stand_dif[is.infinite(df$stand_dif)] <- NA
df$stand_dif_child[is.infinite(df$stand_dif_child)] <- NA
df$stand_dif_child_h[is.infinite(df$stand_dif_child_h)] <- NA
df$stand_dif_h[is.infinite(df$stand_dif_h)] <- NA


# Get ranking of standdif
df <- df %>%
  mutate(dummy = 1) %>%
  arrange(desc(stand_dif_child)) %>%
  mutate(rank_stand_dif_child = cumsum(dummy)) %>%
  dplyr::select(-dummy)
df <- df %>%
  mutate(dummy = 1) %>%
  arrange(desc(stand_dif)) %>%
  mutate(rank_stand_dif = cumsum(dummy)) %>%
  dplyr::select(-dummy)

# Make percentile of stand dif
df <- df %>%
  mutate(percentile_stand_dif = percent_rank(stand_dif) * 100) %>%
  mutate(percentile_stand_dif_child = percent_rank(stand_dif_child) * 100) 

# Make log of stand dif
df <- df %>%
  mutate(log_stand_dif = log(stand_dif)) %>%
  mutate(log_stand_dif_child = log(stand_dif_child))

# Make stand_dif as percentage of max
df$adjusted_stand_dif <- df$stand_dif / 
  max(abs(c(min(df$stand_dif, na.rm = TRUE), max(df$stand_dif, na.rm = TRUE)))) * 
  100
df$adjusted_stand_dif_child <- df$stand_dif_child / 
  max(abs(c(min(df$stand_dif_child, na.rm = TRUE), max(df$stand_dif_child, na.rm = TRUE)))) * 
  100

# Make an adjusted log stand dif with negatives
df$stand_dif_sqrt_directional <- sqrt(abs(df$stand_dif))
df$stand_dif_sqrt_directional <- ifelse(df$stand_dif > 0,
                                    df$stand_dif_sqrt_directional,
                                    df$stand_dif_sqrt_directional * -1)
df$stand_dif_child_sqrt_directional <- sqrt(abs(df$stand_dif_child))
df$stand_dif_child_sqrt_directional <- ifelse(df$stand_dif_child > 0,
                                        df$stand_dif_child_sqrt_directional,
                                        df$stand_dif_child_sqrt_directional * -1)


# Create age specific who variables
df$w_both_15plus_htb_nd <- 
  df$w_both_all_htb_nd - df$w_both_014_htb_nd

df$w_both_15plus_tb_nd <-
  df$w_both_all_tb_nd - df$w_both_014_tb_nd


# Create total columns
df$i_f_15plus_tbtotal_nd <- df$i_f_15plus_tb_nd +
  df$i_f_15plus_htb_nd
df$w_f_15plus_tbtotal_nd <- df$w_f_15plus_tb_nd +
  df$w_f_15plus_htb_nd


# Create standardized difference with incidence instead of mortality
df <- df %>%
  mutate(stand_dif_inc= (w_both_all_tbtotal_nd-i_both_all_tbtotal_nd) / 
           (c_newinc)) # new cases

# Filter to only those countries with both sources
df <- df %>% filter(have_both)

# Clean up country names
df$country[df$country == "Korea Democratic People's Republic of"] <- 'North Korea'
df$country[df$country == "Congo the Democratic Republic of the"] <- 'Dem. Rep. Congo'
df$country[df$country == "Tanzania United Republic of"] <- 'Tanzania'
df$country[df$country == 'Libyan Arab Jamahiriya'] <- 'Libya'
df$country[df$country == "Lao People's Democratic Republic"] <- 'Laos'

# Adjust standardized difference for incidence
df$stand_dif_inc_adj <- df$stand_dif_inc / max(df$stand_dif_inc, na.rm = TRUE) * 100

# ab
# (a - b) / (a + b) where a is the estimated number of deaths by WHO and b the estimated number of deaths by IHME.
df$ab <- (df$w_both_all_tbtotal_nd - df$i_both_all_tbtotal_nd) /
  (df$w_both_all_tbtotal_nd + df$i_both_all_tbtotal_nd)


# Get ranking of ab
df <- df %>%
  mutate(dummy = 1) %>%
  arrange(desc(ab)) %>%
  mutate(rank_ab = cumsum(dummy)) %>%
  dplyr::select(-dummy)

# Make percentile of ab
df <- df %>%
  mutate(percentile_ab = percent_rank(ab) * 100)

# Make log of ab
df <- df %>%
  mutate(log_ab = log(ab))

# Make ab as percentage of max
df$adjusted_ab <- df$ab / max(df$ab, na.rm = TRUE) * 100

# Make an adjusted log stand dif with negatives
df$ab_sqrt_directional <- sqrt(abs(df$ab))
df$ab_sqrt_directional <- ifelse(df$ab > 0,
                                        df$ab_sqrt_directional,
                                        df$ab_sqrt_directional * -1)


# Get proportion of reported mdr
df$reported_mdr <- df$mdr_rr / df$c_newinc * 100

# Correct prevsurvey
df$prevsurvey[df$country %in%
                c('Bangladesh',
                  'Eritrea',
                  'Kenya',
                  'Malaysia',
                  'Philippines',
                  'Viet Nam')] <- 0

# Create an IHME cdr variable
df$cdr_ihme <- 
  (df$c_newinc #+ 
     # df$ret_nrel
   ) / 
  df$ihme_incidence_both_allages_totaltb_number   * 100

df$cdr_ihme[df$cdr_ihme > 100] <- 100

df$gb_cfr <- as.numeric(as.character(df$gb_cfr))


# Write csv
write_csv(df, 'data/combined_data.csv')


# Make a region specific data frame
df_region <- df %>%
  group_by(who_region) %>%
  summarise(population = sum(gb_e_pop_num, na.rm = TRUE),
            i_both_all_htb_nd = weighted.mean(i_both_all_htb_nd,
                                              w = gb_e_pop_num,
                                              na.rm = TRUE),
            i_both_all_tb_nd = weighted.mean(i_both_all_tb_nd,
                                             w = gb_e_pop_num,
                                             na.rm = TRUE),
            i_both_all_tbtotal_nd = weighted.mean(i_both_all_tbtotal_nd,
                                                  w = gb_e_pop_num,
                                                  na.rm = TRUE),
            w_both_all_htb_nd = weighted.mean(w_both_all_htb_nd,
                                              w = gb_e_pop_num,
                                              na.rm = TRUE),
            w_both_all_tb_nd = weighted.mean(w_both_all_tb_nd,
                                             w = gb_e_pop_num,
                                             na.rm = TRUE),
            w_both_all_tbtotal_nd = weighted.mean(w_both_all_tbtotal_nd,
                                                  w = gb_e_pop_num,
                                                  na.rm = TRUE),
            case_fatality_rate_2015_adjusted = 
              weighted.mean(case_fatality_rate_2015_adjusted,
                            w = gb_e_pop_num,
                            na.rm = TRUE),
            relative_difference = 
              weighted.mean(relative_difference,
                            w = gb_e_pop_num,
                            na.rm = TRUE),
            # i_over_w = weighted.mean(i_over_w,
            #                          w = gb_e_pop_num,
            #                          na.rm = TRUE),
            # w_over_i = weighted.mean(w_over_i,
            #                          w = gb_e_pop_num,
            #                          na.rm = TRUE),
            stand_dif = weighted.mean(stand_dif,
                                      w = gb_e_pop_num,
                                      na.rm = TRUE))

# Create an html table of our data
xdf <- databrew::prettify(df %>% arrange(country),
                          download_options = TRUE,
                          nrows = nrow(df),
                          remove_underscores_columns = FALSE,
                          cap_columns = FALSE,
                          cap_characters = FALSE,
                          comma_numbers = FALSE,
                          date_format = '%Y-%m-%d',
                          round_digits = 2)
htmlwidgets::saveWidget(widget = xdf,
                        file = 'data_widget.html')
file.copy(from = 'data_widget.html',
          to = '../databrew.github.io/data_widget.html')
