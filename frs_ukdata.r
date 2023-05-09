#09/05/2023
#investigating the family resources survey

library(haven)
library(tidyverse)

income_levels <- c("Less 200", "200 to 400", "400 to 600", "600 to 800", "800 to 1000", "1000 to 1200", "1200 to 1400", "1400 to 1600", "1600 to 1800", "1800 to 2000", "Above 2000")
ctband_levels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "N/A")
names(ctband_levels) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")


setwd("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Documents\\GitHub\\income_ctband")

frs_househol <- paste0("8948stata_BC04C9F797A5FB7AB83541531D5D370C9FDDC03A938BE2631C396F85A0B1C797_V1\\UKDA-8948-stata\\stata\\stata13_se\\househol.dta")

househol_raw <- read_dta(frs_househol)


househol <- househol_raw |>
    select(SERNUM, CTBAND, HEARNS, HHINC, HHINCBND)

incband_hh <- househol |>
    select(SERNUM, CTBAND, HHINCBND)

incband_a <- incband_hh |>
    filter(CTBAND == 1)


a_row <- table(incband_a$HHINCBND)
a_row <- as.data.frame(table(incband_a$HHINCBND))

a_row <- pivot_wider(a_row, name = 'Var1')


 
incband_a <- incband_a |>
    group_by(HHINCBND)



pivotwider <- incband_hh |>
    pivot_wider(names_from = CTBAND, values_from = HHINCBND)



t <- incband_hh %>% 
group_by(CTBAND,HHINCBND) %>%
count() 

t <- t |>
group_by(CTBAND) |>
mutate(total = sum(n)) |>
mutate(percent = n/total)

t %>% 
filter(CTBAND != -1) %>%
#filter(CTBAND == 5) %>%
ggplot(aes(x = HHINCBND, y = percent)) +
geom_bar(stat = "identity") +
scale_x_discrete(limit = income_levels) +
theme(axis.text.x = element_text(angle = 90), 
axis.ticks = element_blank())+
facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))

 incband_hh %>% 
group_by(CTBAND,HHINCBND) -> t2

str(t2)
str(incband_hh)

t2 %>% count() %>% view()
incband_hh %>% count() %>% view()


 incband_hh %>% 
group_by(CTBAND,HHINCBND) %>%
count()  %>% view()
