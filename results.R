source('make_wide.R')

# A total of 195 countries had TB mortality estimates for 2015 by both WHO and IHME. 
table(df$have_both)

# Among those countries, WHO estimated 1,768,482 total number of deaths attributable to tuberculosis, whereas IHME estimated 1,322,916 deaths, resulting in a difference of 445,567 deaths (24% reduced mortality if taking WHO as a reference, or 33.7% increased mortality if IHME is the reference). 

sum(df$i_both_all_tbtotal_nd)
sum(df$w_both_all_tbtotal_nd)

sum(df$i_both_all_tbtotal_nd) -
sum(df$w_both_all_tbtotal_nd)

(sum(df$i_both_all_tbtotal_nd) -
    sum(df$w_both_all_tbtotal_nd)) / 
  sum(df$i_both_all_tbtotal_nd)

(sum(df$i_both_all_tbtotal_nd) -
    sum(df$w_both_all_tbtotal_nd)) / 
  sum(df$w_both_all_tbtotal_nd)

#This difference in TB mortality was higher in people living with HIV (211,604 by IHME vs 389,042 by WHO), where WHO estimates 84% higher number of deaths attributable to TB than WHO. 
sum(df$i_both_all_htb_nd)
sum(df$w_both_all_htb_nd)

# The relative difference in number of deaths was especially higher for the paediatric population, where WHO estimates almost three times higher number of deaths than IHME (209,837 vs 69,659 number of deaths by WHO and IHME respectively). 

sum(df$i_both_014_tb_nd + df$i_both_014_htb_nd)
sum(df$w_both_014_tb_nd + df$w_both_014_htb_nd)


# Among adult TB deaths, there were not large differences in the sex-specific mortality estimates (table 1).

# There were 84 countries (43.1%), in which WHO estimated higher number of deaths attributable to TB than IHME. 
table(df$w_both_all_tbtotal_nd > df$i_both_all_tbtotal_nd)
prop.table(table(df$w_both_all_tbtotal_nd > df$i_both_all_tbtotal_nd))


# Those countries with larger absolute differences in total number of TB deaths were (by decreasing magnitude of the difference): Nigeria (216621), Bangladesh (49863), Tanzania (38272), South Africa (29108), Mozambique (28909),  Indonesia, (26121),  Democratic Republic of Congo, (26010),  India (20696), North Korea(13218),  and Angola(9910). 
df %>%
  mutate(x = w_both_all_tbtotal_nd - i_both_all_tbtotal_nd) %>%
  arrange(desc(x)) %>%
  mutate(x = round(x)) %>%
  dplyr::select(country, x) %>%
  head(10)


# The countries in which IHME estimated higher number of deaths than WHO were: Ethiopia (22650), China (13538), Zimbabwe(11082), Philippines (9436), Nepal (5477), Uganda (5081), Burkina Faso, (4837),  Niger(3758), Viet Nam (3252),  and Senegal (3147),  (figure 2). 
df %>%
  mutate(x = i_both_all_tbtotal_nd - w_both_all_tbtotal_nd) %>%
  arrange(desc(x)) %>%
  mutate(x = round(x)) %>%
  dplyr::select(country, x) %>%
  head(10)

# Map 1 shows how the largest differences in terms of absolute number of deaths are concentrated in few countries. In fact, the correlation of TB mortality estimates between both institutions is very good for most countries and regions (figure 2) Can we statistically appraise this?. 
cor(df$i_both_all_tbtotal_nd, df$w_both_all_tbtotal_nd)


t.test(df$i_both_all_tbtotal_nd,
       df$w_both_all_tbtotal_nd, 
       paired=TRUE)

#For the African region, for those countries with lower mortality burden, IHME estimates higher mortality and viceversa. Can we statistically appraise this?
x <- df %>% filter(who_region == 'AFR')
plot(x$w_both_all_tbtotal_nd,
     x$i_both_all_tbtotal_nd)

# After standardizing the absolute difference by the total number of reported deaths, thus taking into account the country-specific TB burden, the countries where WHO estimated higher mortality than IHME (by decreasing magnitude of the adjusted standardized difference) are: Libya (100), Nigeria (37.0), Iceland (19.6), Congo (18.9), Afghanistan (13.7), Timor Leste(9.9), Tanzania (9.0), Angola (8.2), Sudan (7.6),  and Mozambique (6.8) ranking changed (figure 2). 

df %>%
  mutate(x = adjusted_stand_dif) %>%
  arrange(desc(x)) %>%
  mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)

#Likewise, the most important differences in which IHME estimated higher mortality are Eritrea (-13.4), Burkina Faso(-7.5), Togo (-6.1), Benin (-5.9), Burundi (-5.6), Nepal, (-5.0),  Rwanda(-4.9), South Sudan(-4.8), Senegal (-4.5),  and Botswana(-4.5) (figure 3). 

df %>%
  mutate(x = adjusted_stand_dif) %>%
  arrange(x) %>%
  mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)

# Twenty-three countries did not have an adjusted standardized difference since reported number of deaths were not available. 
table(is.na(df$adjusted_stand_dif))

# Standardization of the absolute difference in TB number of deaths by incident number of TB cases (as a proxy of burden of tuberculosis) yields similar country rankings. In most countries of South East Asia, WHO estimates higher number of deaths than IHME, although the magnitude of this difference (as depicted by adjusted standardized difference) is not large.
x <- df %>% filter(who_region == 'SEA')
table(x$w_both_all_tbtotal_nd > x$i_both_all_tbtotal_nd)

# As for the absolute differences we can see that the most dramatic differences in TB mortality estimates are concentrated in few countries (Map 2). In the online supplementary material, Map 3 shows all indicators by country of this descriptive analysis.


# When looking at the association between the adjusted standardized difference between IHME and WHO mortality estimates and different potential drivers of this difference we see poor correlation with reported HIV prevalence among TB cases (r= 0.03, 95%CI) , 

cor(df$adjusted_stand_dif, df$p_hiv_of_tb, na.rm = TRUE)

#MDR/RR prevalence (r= -0.05, 95%CI)  

#and case fatality rate (r= -0.19, 95%CI). 

#There is a moderate correlation with case detection rate (as estimated by WHO), (r= -0.32, 95%CI), which disappears when using CDR based on IHME number of incident cases (r= 0.06, 95%CI: ) (figure 2). Those countries which have had a national prevalence survey have a higher adjusted standardized difference than those without a prevalence survey (3.5 vs 0.88 respectively). In other words, in those countries for which prevalence survey is available, WHO tents to estimate higher number of deaths attributable to TB, although this difference is not statistically significant (p=0.2).



# 1. Can you tell me the number of countries with WHO estimates without IHME estimates? (and the total number of deaths in those countries (the total, not country by country)?) I think these are 22 countries, but not 100% sure!


#   
#   2. The paragraph of the associations with different factors, you did not include the 95% CI, which i think would look good. Any reason?
# 
# 3. Lastly, i wanted to mention something about children. Could you tell me top 10 countries with higher differences (and the actual values) in children towards WHO and IHME? I am interested in knowing if there are the same as for all ages. I think a paragraph in the results section would be very interesting (since the initial table points at a high difference in children) Any other idea on what to put in this paragraph is welcome :)

x <- df %>%
  dplyr::select(country,
                w_both_014_htb_nd,
                w_both_014_tb_nd,
                i_both_014_htb_nd,
                i_both_014_tb_nd,
                newrel_f014,
                newrel_m014) %>%
  mutate(difference = (w_both_014_htb_nd +  w_both_014_tb_nd) -  (i_both_014_htb_nd  + i_both_014_tb_nd)) %>%
  mutate(absolute_difference = abs(difference)) %>%
  arrange(desc(absolute_difference)) %>%
  # In order to further appraise the children issue, i had an idea. Could we standardize (divide) the difference (a) by the reported indicent cases (newrel_f014 + newrel_f014) (from the notifications dataset of WHO) and see that are the rankings?
  mutate(y = absolute_difference / (newrel_f014 + newrel_m014))


x %>%
  mutate(x = difference) %>%
  arrange(x) %>%
  mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)

x %>%
  mutate(x = difference) %>%
  arrange(desc(x)) %>%
  mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)


x %>%
  mutate(x = y) %>%
  filter(!is.na(x),
         !is.infinite(y)) %>%
  arrange(x) %>%
  # mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)

x %>%
  mutate(x = y) %>%
  filter(!is.na(x),
         !is.infinite(y)) %>%
  arrange(desc(x)) %>%
  # mutate(x = round(x, 1)) %>%
  dplyr::select(country, x) %>%
  head(10)



write_csv(x, '~/Desktop/children_updated.csv')



# How many deaths in those 23 countries with who and not ihme
sum(df$w_both_all_tbtotal_nd[df$have_who & !df$have_ihme], na.rm = T)


cor(df$adjusted_stand_dif, df$newrel_hivpos/df$newrel_hivtest,
    use = 'complete.obs')

# b) CDR by IHME
# adjusted_stand_diff and cdr_ihme
cor(df$adjusted_stand_dif, df$cdr_ihme,
    use = 'complete.obs')
cor(df$stand_dif, df$cdr_ihme,
    use = 'complete.obs')

# c) CDR by WHO
# adjusted_stand_diff and gb_c_cdr
cor(df$adjusted_stand_dif, as.numeric(df$gb_c_cdr),
    use = 'complete.obs')
cor(df$stand_dif, as.numeric(df$gb_c_cdr),
    use = 'complete.obs')

# d) Reported case fatality rate
# adjusted_stand_diff and case_fatality_rate_2015_adjusted
cor(df$adjusted_stand_dif,df$case_fatality_rate_2015_adjusted,
    use = 'complete.obs')
cor(df$stand_dif,df$case_fatality_rate_2015_adjusted,
    use = 'complete.obs')
# e) Reported MDR
# adjusted_stand_diff and reported_mdr
cor(df$adjusted_stand_dif,df$reported_mdr,
    use = 'complete.obs')
cor(df$stand_dif,df$reported_mdr,
    use = 'complete.obs')

library(psychometric)

# Correlation coefficients
confy <- function(x){
  # fit <- lm(df$adjusted_stand_dif ~ x)
  r <- cor(df$stand_dif, x,
           use = 'complete.obs')
  message(paste0('Correlation coefficient: ', r))
  print(CIr(r=r, n = nrow(df), level = .95))
}

confy(df$p_hiv_of_tb)
confy(df$p_mdr_new)
confy(df$newrel_hivpos/df$newrel_hivtest)
confy(df$cdr_ihme)
confy(df$gb_c_cdr)
confy(df$case_fatality_rate_2015_adjusted)

by(df$prevsurvey, mean(df$adjusted_stand_dif))
x = df %>%
  group_by(prevsurvey) %>%
  summarise(m = mean(adjusted_stand_dif, na.rm = TRUE))

# a) was the WHO estimated CDR significantly lower for countries that had prevalence surveys than for countries that did not? If so, it suggests that the two are interrelated, although this may in part reflect reverse causality (i.e prevalence surveys were done BECAUSE the countries concerned were expected to have low CDRs).
fit <- lm(gb_c_cdr ~ prevsurvey, data = df)
confint(fit)



# b)  Could be to do a box and whisker plot of CDR estimates for WHO and IHME (e.g. different colours) for countries with prevalence surveys vs countries without?
x <- df %>%
  dplyr::select(country, prevsurvey,
                gb_c_cdr,
                cdr_ihme) %>%
  rename(IHME = cdr_ihme,
         WHO = gb_c_cdr,
         `Prevalence survey` = prevsurvey) %>%
  mutate(`Prevalence survey` = ifelse(`Prevalence survey` == 1, 'Survey',
                                      'No survey')) %>%
  gather(Source,
         value,
         WHO:IHME)
library(ggplot2)
ggplot(data = x,
       aes(x = Source,
           y = value,
           fill = Source,
           group = Source)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~`Prevalence survey`) +
  scale_y_log10()

# c) to what extent the association of CDR are stand diff changes if we remove those countries with prevalence survey)? Both in the case of WHO CDR and IHME CDR.
x <- df %>%
  dplyr::select(country, prevsurvey,
                gb_c_cdr,
                cdr_ihme,
                stand_dif) %>%
  rename(IHME = cdr_ihme,
         WHO = gb_c_cdr,
         `Prevalence survey` = prevsurvey,
         `Standardized difference` = stand_dif) %>%
  mutate(`Prevalence survey` = ifelse(`Prevalence survey` == 1, 'Survey',
                                      'No survey'))

ggplot(data = x,
       aes(x = WHO,
           y = `Standardized difference`)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~`Prevalence survey`) +
  geom_smooth() +
  labs(title = 'WHO CDR and standardized difference')

ggplot(data = x,
       aes(x = IHME,
           y = `Standardized difference`)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~`Prevalence survey`) +
  geom_smooth() +
  labs(title = 'IHME CDR and standardized difference')


# d) In the paper of TB by IHME, they report 1,324,342 deaths (195 countries). Our results for IHME are 1,322,916, a difference of 1426 deaths. I thought we included all IHME countries... Is there any WHO country not included in IHME countries?
