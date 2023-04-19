#Emily Keenan
#19/04/2023
#Code to estimate the income distribution in council tax bands 

library(tidyverse)
library(readxl)
library(janitor)
library(readODS)

income_input <- "Q:\\ADD Directorate\\Local Policy Analysis\\LGF\\Council Tax\\Households Income Analysis\\c2-income-state-support.xlsx"
ctb_input <- "Q:\\ADD Directorate\\Local Policy Analysis\\LGF\\Council Tax\\Households Income Analysis\\Council_Taxbase_local_authority_level_data_2022 (4).ods"
country <- c('United Kingdom', 'England', 'Wales', 'Scotland', 'Northern Ireland', 'Great Britain', 'Inner London', 'Outer London')
ct_bands <- c('band_a', 'band_b', 'band_c', 'band_d', 'band_e', 'band_f', 'band_g', 'band_h', 'total')
bandsa_c <- c('band_a', 'band_b', 'band_c')
region_code <- c('NE', 'NW', 'YH', 'EM', 'WM', 'E', 'L', 'SE', 'SW')


#Read in the income data 
raw_income <- read_excel(income_input, sheet = "2_6", skip = 8)

#initial data clean
income<-raw_income %>%
  clean_names() %>%
  na.omit()

income <- income %>%
  rename(region = region_country_note_8, 
         all_percent = all, 
         under_200 = under_200_a_week)

colnames(income) <- gsub("x_", "", colnames(income))
colnames(income) <- gsub("x", "", colnames(income))


#remove larger areas
income <- income %>%
  filter(!(region %in% country))

#add region codes
income$re_code <- region_code

#read in taxbase data

raw_ctb <- read_ods(ctb_input, sheet = "Council_Taxbase_Data", range = "A6:N316")

ctb <- raw_ctb %>%
  clean_names()%>%
  select(!notes)

ctb_region <- ctb %>%
  group_by(region) %>%
  summarise(across(contains("band"), sum))

#add AtoC and DtoH
ctb_region <- ctb_region %>% rowwise() %>% mutate(a_c = sum(band_a, band_b, band_c, na.rm = TRUE),
                                                  d_h =  sum(band_d, band_e, band_f, band_g, band_h, na.rm=TRUE))

#####analysis####


#converting percentages to households
income_hh <- income %>%
  mutate(across(contains("00"), ~ .x / 100 *sample_size, name = "{col}"))

#adjust sample size for bs rounding
income_hh <- income_hh %>% rowwise() %>%
  mutate(sample_size = sum(across(contains("00"))))

#calculating percentage less than 1000 per week and more
income_hh <- income_hh %>% rowwise() %>% mutate(less_1000 = sum(under_200, `200_but_less_than_400`, `400_but_less_than_600`, `600_but_less_than_800`, `800_but_less_than_1_000`, na.rm = TRUE),
                                                more_1000 = sum(`1_000_but_less_than_1_200`, `1_200_but_less_than_1_400`, `1_400_but_less_than_1_600`,`1_600_but_less_than_1_800` ,`1_800_but_less_than_2_000`, `2_000_or_more`, nq.rm = TRUE))
income_hh <- income_hh %>%
  mutate(percent_less_1000 = less_1000/sample_size, 
         percent_more_1000 = more_1000/sample_size)

#Estimation of households in each region with less than 1000 and more than 1000
hh_estimate <- 

