#10/05/2023
#investigating the family resources survey
#update to 2021-22 frs survey



#import packages
library(haven)
library(tidyverse)
library(xlsx)

#define variables to map onto encoded data
income_levels <- c("Less 200", "200 to 400", "400 to 600", "600 to 800", "800 to 1000", "1000 to 1200", "1200 to 1400", "1400 to 1600", "1600 to 1800", "1800 to 2000", "Above 2000")
ctband_levels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "N/A")
names(ctband_levels) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
not_eng <-  c("299999999.0", "399999999.0", "499999999.0")

#set working directory
setwd("D:\\Users\\emily.keenan\\Documents\\GitHub\\income_ctband2")

#data location
frs_househol <- paste0("2122_househol.dta")

#read in frs data househol file
househol_raw <- read_dta(frs_househol)

#select possible relevent variable
#ct band, gross income from employment, total household income and household income bands
househol <- househol_raw |>
    select(SERNUM, CTBAND, HEARNS, HHINC, HHINCBND, GVTREGN)

#initally focus on ctband and household income bands from employment
#remove households from walse, scotland and northern ireland
incband_hh <- househol |>
    select(SERNUM, CTBAND, HHINCBND, GVTREGN) |>
    filter(!GVTREGN %in% as.numeric(not_eng))

###################################
#### messy stuff I tried #####
##################################
#incband_a <- incband_hh |>
#    filter(CTBAND == 1)
#a_row <- table(incband_a$HHINCBND)
#a_row <- as.data.frame(table(incband_a$HHINCBND))
#a_row <- pivot_wider(a_row, name = 'Var1')
#incband_a <- incband_a |>
#    group_by(HHINCBND)
#pivotwider <- incband_hh |>
#    pivot_wider(names_from = CTBAND, values_from = HHINCBND)


#condense the data to the number of household in each income range by ct band 
t <- incband_hh %>% 
group_by(CTBAND,HHINCBND) %>%
count() 

#calculate the percentage of households in each income range by ct band
t <- t |>
group_by(CTBAND) |>
mutate(total = sum(n)) |>
mutate(percent = n/total)


#plot graphs of the distribution of household income within council tax bands
p <- t %>% 
filter(CTBAND != -1) %>%
#filter(CTBAND == 5) %>%
ggplot(aes(x = HHINCBND, y = percent)) +
geom_bar(stat = "identity") +
scale_x_discrete(limit = income_levels) +
theme(axis.text.x = element_text(angle = 90), 
axis.ticks = element_blank())+
facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))

#save plot
ggsave("eng.png", p)
#save plot input data
t_ungroup <- ungroup(t)
write.xlsx(t_ungroup, "D:\\Users\\emily.keenan\\Documents\\GitHub\\income_ctband2\\incomedis_ctband_input.xlsx")

#  incband_hh %>% 
# group_by(CTBAND,HHINCBND) -> t2

# str(t2)
# str(incband_hh)

# t2 %>% count() %>% view()
# incband_hh %>% count() %>% view()


#  incband_hh %>% 
# group_by(CTBAND,HHINCBND) %>%
# count()  %>% view()

