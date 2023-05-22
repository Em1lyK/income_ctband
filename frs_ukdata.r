#10/05/2023
#investigating the family resources survey
#update to 2021-22 frs survey



#import packages
library(haven)
library(tidyverse)
library(xlsx)
library(readODS)
library(janitor)
library(wesanderson)

#define variables to map onto encoded data
income_levels <- c("Less 200", "200 to 400", "400 to 600", "600 to 800", "800 to 1000", "1000 to 1200", "1200 to 1400", "1400 to 1600", "1600 to 1800", "1800 to 2000", "Above 2000")
ctband_levels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "N/A")
names(ctband_levels) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
not_eng <-  c("299999999.0", "399999999.0", "499999999.0")

#set working directory
setwd("D:\\Users\\emily.keenan\\Documents\\GitHub\\income_ctband2")

#data location: family resources survey and ctb
frs_househol <- paste0("inputs\\2122_househol.dta")
ctb_input <- "Q:\\ADD Directorate\\Local Policy Analysis\\LGF\\Council Tax\\Households Income Analysis\\Council_Taxbase_local_authority_level_data_2022 (4).ods"


#read in frs data househol file and ctb dewellings on the valuation list 
househol_raw <- read_dta(frs_househol)
ctb_raw <- read_ods(ctb_input, sheet = "Council_Taxbase_Data", range = "A6:N316")

#clean ctb
ctb_val <- ctb_raw %>%
  clean_names()%>%
  select(!notes)

ctb_val <- pivot_longer(ctb_val, band_a:total)

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

#calculate the percentage of households in each income range by ct band, removing NAs 
t <- t |>
filter(CTBAND != -1, CTBAND != 10) |>
group_by(CTBAND) |>
mutate(total = sum(n)) |>
mutate(percent = n/total)

view(t)

#change income band to factor type 
t$HHINCBND <- as.factor(t$HHINCBND)

#add the corresponding income bands in exchange for the numbers  
levels(t$HHINCBND) <- list( "Less 200" = 1, 
                            "200 to 400" = 2, 
                            "400 to 600" = 3, 
                            "600 to 800" = 4, 
                            "800 to 1000" = 5, 
                            "1000 to 1200" = 6, 
                            "1200 to 1400" = 7, 
                            "1400 to 1600" = 8, 
                            "1600 to 1800" = 9, 
                            "1800 to 2000" = 10, 
                            "Above 2000" = 11)

#plot graphs of the distribution of household income within council tax bands
p <- t %>% 
ggplot(aes(x = HHINCBND, y = percent, fill=HHINCBND)) +              #input data fro hh percentage and income band
geom_bar(stat = "identity") +                                        #specify a bar chart
ggplot2::scale_fill_brewer(palette="Spectral") +                     #add thecolour palette
theme(axis.text.x=element_blank(),                                   #remove x axis labels
        axis.ticks.x=element_blank())+                               #remove x axis ticks
 ylab("Percentage of households by council tax band and income band at England level") + 
 xlab("Income bands") +                                              #add axis lables
 ggplot2::guides(fill=guide_legend(title="Income bands \n per week")) +          #add lgend title
facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))     #make multiple graphs by CT band
p

#save plot
ggsave("hhdist_plots\\eng.png", p)
#save plot input data
#t_ungroup <- ungroup(t)
#write.xlsx(t_ungroup, "D:\\Users\\emily.keenan\\Documents\\GitHub\\income_ctband2\\incomedis_ctband_input.xlsx")



##########################################
####cheking data distribution ###########
#########################################

#select only the council tax band columns
only_ct <- incband_hh |>
    select(SERNUM, CTBAND) |>
    group_by(CTBAND) |>
    count()
#total number of houses in England in the council tax band part of the survey 
total <- sum(only_ct$n)

#calculate the percentage of households in each ct band of the frs 
only_ct <- only_ct |>
    mutate(dist = n/total*100)|>
    filter(CTBAND != 10)

only_ct <- ungroup(only_ct)

#percentage of dwellings on the valuation list in each council tax band
ctb_total <- as.numeric(ctb_val[9, 6])

ctb_engval <- ctb_val |>
    filter(region == "ENG") |>
    mutate(total = ctb_total)

ctb_engval <- ctb_engval |>
    mutate(dis = (value/total)*100) |>
    filter(name != "total")

comparison_df <- ctb_engval$dis
comparison_df <- cbind(comparison_df, only_ct$dist)
comparison_df <- comparison_df |> as.data.frame() %>%
    mutate(banddis_diff = (comparison_df - V2))

comparison_df <- comparison_df |>
    rename(ctb_banddis = comparison_df, frs_banddis = V2)

write.xlsx(comparison_df, "data_output\\comp_bandd_frsCTB.xlsx")

view(comparison_df)
view(ctb_val)
view(only_ct)
view(total)
view(ctb_total)
