#05/06/2023
#read in the family resources survey and 

#import packages
library(haven)      
library(tidyverse)                              #includes usual functions for analysis 
library(xlsx)                                   #read in and output excel sheets 
library(readODS)                                #read ods files 
library(janitor)                                #clean data frames
library(labelled)                               #changes the labels of data

#define variables to map onto encoded data
ctband_levels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "N/A")                                          #ct bands
names(ctband_levels) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")                                    #numbers ct bands corresond to the in the data
not_eng <-  c("299999999.0", "399999999.0", "499999999.0")                                                      #codes for the devolved authorities 
yusuf_pal_full <- c("#012169", "#3e2272", "#632076","#851d76", "#a41b72", "#bf1f6b", "#d62d60", "#e94154", "#f65846", "#ff7135", "#ff8b21", "#ffa600")          #define yusuf's beautiful colour scheme 
setwd("D:\\Users\\emily.keenan\\Documents\\GitHub\\income_ctband2")                                             #set working directory

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
ctb_val <- pivot_longer(ctb_val, band_a:total)              #change ctb to long datat frame format

#select relevent variable
incband_hh <- househol_raw |>
    select(SERNUM, CTBAND, HHINCBND, GVTREGN) |>       #hosueholds number, ct band, household income bands, region 
    filter(!GVTREGN %in% as.numeric(not_eng))           #remove households in wales, scotland and northern ireland

#condense the data to the number of household in each income range by ct band 
t <- incband_hh %>% 
group_by(CTBAND,HHINCBND) %>%                           #group by ct band and income band 
count()                                                 #count the number of households in each ct band and income band

#calculate the percentage of households in each income range by ct band
t <- t |>
filter(CTBAND != -1, CTBAND != 10) |>                   #remove NAs and hosueholds not valued separately 
group_by(CTBAND) |>
mutate(total = sum(n)) |>                               #counc the number of hoseuholds in the frs in each ct band 
mutate(percent = n/total*100)                           #number of housholds in each ct bands and income band /total number of households in corresponding ct band

t$HHINCBND <- as.factor(t$HHINCBND)                     #change income band to factor type so it can be lablled on the graph
levels(t$HHINCBND) <- list( "Less than 200" = 1,        #add the corresponding income bands in exchange for the numbers  
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

###################################################################################################
###########plot graphs of the distribution of household income within council tax bands############
###################################################################################################
p <- t %>% 
ggplot(aes(x = HHINCBND, y = percent, fill=HHINCBND)) +              #input data fro hh percentage and income band
geom_bar(stat = "identity") +                                        #specify a bar chart
scale_fill_manual(values=yusuf_pal_full)+                            #add the colour palette
scale_y_continuous(limits = c(0, 50))+                               #fix the y axis range
theme(axis.text.x=element_blank(),                                   #remove x axis labels
      axis.ticks.x=element_blank(), 
      text = element_text(size = 18)) +                               #remove x axis ticks
 ylab("Percentage of households by council tax band and income band at England level (%)") + 
 xlab("Income bands") +                                              #add axis lables
 ggplot2::guides(fill=guide_legend(title="Income bands \n per week")) +          #add legend title
facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))    #make multiple graphs by CT band
p                                                                   #print graph
ggsave("hhdist_plots\\eng.png", p)                                  #save plot

#save plot input data
eng_plot_input <- t |>
ungroup() |>
remove_labels() 
write.xlsx(eng_plot_input, "data_output\\eng_plot_input.xlsx")

###########################################################
#### cheking data distribution against the ctb  ###########
###########################################################
#select only the council tax band columns
only_ct <- incband_hh |>
    select(SERNUM, CTBAND) |>
    filter(CTBAND != 10, CTBAND !=-1) |>                 #remoove hosueholds that were not values independently and NAs
    group_by(CTBAND) |>
    count()                                 #count the number of hosueholds in each council tax band
#sum total number of houses in England in family resources survey (frs)
total <- sum(only_ct$n)                     

#calculate the percentage of households in each ct band of the frs 
only_ct <- only_ct |>
    mutate(dist = n/total*100)|>             #numebr of hosueholds in each council tax band over the total number of households in the frs
    ungroup()

####calculate percentage of dwellings on the valuation list in each council tax band####
ctb_total <- as.numeric(ctb_val[9, 6])      #select eng total value 

ctb_engval <- ctb_val |>                    #select eng totals for each council tax band
    filter(region == "ENG") |>
    mutate(total = ctb_total)               #ad a column full of the england total number of hosueholds

ctb_engval <- ctb_engval |>                 #calculate the percentage of households in each ct band in the ctb                        
    mutate(dis = (value/total)*100) |>
    filter(name != "total")

####comapre the frs distribution of hoseuholds to the ctb dist of households####
comparison_df <- ctb_engval$dis                             #add column of the distribution across ct bands from the ctb
comparison_df <- cbind(comparison_df, only_ct$dist)         #add column of the distribution across ct bands from the frs
comparison_df <- comparison_df |> as.data.frame() %>%
    mutate(banddis_diff = (comparison_df - V2))             #calculate the difference between the two in each ct band

comparison_df <- comparison_df |>
    rename(ctb_banddis = comparison_df, frs_banddis = V2)   #rename fore neatness

write.xlsx(comparison_df, "data_output\\comp_bandd_frsCTB.xlsx")        #save comparision workbook 
