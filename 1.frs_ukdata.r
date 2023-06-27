#05/06/2023
#read in the family resources 

#import packages
library(haven)
library(tidyverse)
library(xlsx)
library(readODS)
library(janitor)
library(labelled)

#define variables to map onto encoded data
income_levels <- c("Less 200", "200 to 400", "400 to 600", "600 to 800", "800 to 1000", "1000 to 1200", "1200 to 1400", "1400 to 1600", "1600 to 1800", "1800 to 2000", "Above 2000")
ctband_levels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "N/A")
names(ctband_levels) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
not_eng <-  c("299999999.0", "399999999.0", "499999999.0")

#define yusuf's beautiful colour scheme 
yusuf_pal_full <- c("#012169", "#3e2272", "#632076","#851d76", "#a41b72", "#bf1f6b", "#d62d60", "#e94154", "#f65846", "#ff7135", "#ff8b21", "#ffa600")

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


#condense the data to the number of household in each income range by ct band 
t <- incband_hh %>% 
group_by(CTBAND,HHINCBND) %>%
count() 

#calculate the percentage of households in each income range by ct band, removing NAs 
t <- t |>
filter(CTBAND != -1, CTBAND != 10) |>
group_by(CTBAND) |>
mutate(total = sum(n)) |>
mutate(percent = n/total*100)

view(t)

#change income band to factor type 
t$HHINCBND <- as.factor(t$HHINCBND)

#add the corresponding income bands in exchange for the numbers  
levels(t$HHINCBND) <- list( "Less than 200" = 1, 
                                "200 and less than  400" = 2, 
                                "400 and less than 600" = 3, 
                                "600 and less than 800" = 4, 
                                "800 and less than 1000" = 5, 
                                "1000 and less than 1200" = 6, 
                                "1200 and less than 1400" = 7, 
                                "1400 and less than 1600" = 8, 
                                "1600 and less than 1800" = 9, 
                                "1800 and less than 2000" = 10, 
                                "Above 2000" = 11)

#plot graphs of the distribution of household income within council tax bands
p <- t %>% 
ggplot(aes(x = HHINCBND, y = percent, fill=HHINCBND)) +              #input data fro hh percentage and income band
geom_bar(stat = "identity") +                                        #specify a bar chart
scale_fill_manual(values=yusuf_pal_full)+                     #add thecolour palette
scale_y_continuous(limits = c(0, 50))+                                 #fix the y axis range
theme(axis.text.x=element_blank(),                                   #remove x axis labels
      axis.ticks.x=element_blank(), 
      text = element_text(size = 18)) +                               #remove x axis ticks
 ylab("Percentage of households by council tax band and income band at England level (%)") + 
 xlab("Income bands") +                                              #add axis lables
 ggplot2::guides(fill=guide_legend(title="Income bands \n per week")) +          #add lgend title
facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))     #make multiple graphs by CT band
p

#save plot
ggsave("hhdist_plots\\eng.png", p)
#to save a plot with a y axis to 100
ggsave("hhdist_plots\\eng_100.png", p)



#pull out input data
eng_plot_input <- t |>
ungroup() |>
remove_labels() 
write.xlsx(eng_plot_input, "data_output\\eng_plot_input.xlsx")


#save plot input data
#t_ungroup <- ungroup(t)
#write.xlsx(t_ungroup, "D:\\Users\\emily.keenan\\Documents\\GitHub\\income_ctband2\\incomedis_ctband_input.xlsx")
#############################################################
########coming up with key lines from the data################
##############################################################

##BAND A
eng_banda <- t|>
filter(CTBAND == 1)
per_banda <- sum(eng_banda[1:4, 3])/eng_banda[1, 4]
view(per_banda)

##BAND D
eng_bandd <- t|>
filter(CTBAND == 4)
per_bandd <- sum(eng_bandd[1:4, 3])/eng_bandd[1, 4]
view(per_bandd)

##BAND F
eng_bandf <- t|>
filter(CTBAND == 6)
per_bandf <- sum(eng_bandf[1:4, 3])/eng_bandf[1, 4]
view(per_bandf)

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
write.xlsx(ctb_val, "data_output\\ctb_val.xlsx")
write_csv(househol_raw, "data_output\\frs_raw.csv")

view(comparison_df)
view(ctb_val)
view(only_ct)
view(total)
view(ctb_total)
