#05/06/2023
#Emily Keenan
### produces plot of te distribution of household (hh) characteristics across counil tax bands###

na_s <- c(-1, 10)                                                   #vector of missing value reference or hh not valued separately 
not_england <-  c("299999999", "399999999", "499999999.0")          #codes for devolved nations 
single_person <- c(3, 4, 9, 10, 11)                                 #numbers denoting single person hh from the data dictionary 
over_pension <- c(1, 2, 5, 6)                                       #numbers denoting hh with two adult or less where at least one is over the pension age
three_more <- c(11, 14, 17)                                         #numbers denoting hh with three or more children
two_kids <- c(10, 13, 16)                                           #numbers denoting hh with two children
single_parents <- c(9, 10, 11)                                      #numbers denoting single person hh
owners_ish <- c(1, 2)                                               #numbers denoting that the dewllings is owned outright by the hh or the hh hold a morgae

#### function to calculate the intput data for plots of percentage of hh in each council tax band with a chosen composition #####
hh_plots <- function(type, percent_type, plot_input) {
    a <- house_compo |>
        filter(HHCOMPS %in% type)                           #select the households with the relevent composition

    a <- a |>                                               #group by council tax band and count up households in each CT band 
        select(SERNUM:CTBAND) |>
        group_by(CTBAND)|>
        count() 
    b <- left_join(a, house_total, by = 'CTBAND')           #join CT band hh totals to CT band hh of specific type
    
    b <- b |>
    mutate(!! paste0(percent_type) := n/total_hh*100) |>    #calculate the percentage of houeholds in that ct band that are of the chosen type 
    ungroup()
    return(assign(plot_input, b, envir = parent.frame()))   #save plot to the wider environment

}

#### function plot a graph of the percentage of hh in each council tax band that have the selected hh composition (type) ####
plot_hh_types <- function(plot_input, percent_type, colour, upper_lim, y_lable, file_loc, file_loc_x ) {
    a <- plot_input %>% 
        ggplot(aes(x = as.character(CTBAND) , y = percent_type)) +              #input data from hh percentage and income band
        geom_bar(stat = "identity", fill = paste0(colour))  +                   #specify a bar chart and select bar colour
        scale_y_continuous(limits = c(0,upper_lim)) +                           #set y axis limit
        theme_minimal() +                                                       #use minumal theme
        scale_x_discrete(labels = c("1" = "A", "2" = "B", "3" =  "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"), position = "bottom") +        #re-lable x axis with ct band letters
        ylab(paste0(y_lable)) +                                                 #add y axis label
        xlab("Council tax bands")                                               #add axis lables
    a <- a + theme(text = element_text(size = 20))                              #increase plot text size 
   
    ggplot2::ggsave(paste0(file_loc), a)                                        #save to two file locations for convenience
    ggplot2::ggsave(paste0(file_loc_x), a)
    return(a)                                                                   #print plot
}

house_compo <- househol_raw |>                                                  #select households number, council tax band, region, household composition
    select(SERNUM, CTBAND, GVTREGN, HHCOMPS)
house_compo <- house_compo |>
 filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)                            #remove missing values, households not valued separately and select only England

house_total <- house_compo |>
    group_by(CTBAND) |>
    count() |>                                                                   #calculating the total number of households in each band
    rename(total_hh = n)

###############################################################################
####### distribution of single person hh across council tax bands #############
###############################################################################
hh_plots(single_person, 'percent_one', 'input_one')                                                             #call function to generate data for the graphs: single adult households 
plot_hh_types(input_one, input_one$percent_one,"#012169", 50, "Percentage of single adult households (%)",     #call function to plot percentage of hh in each council tax band that are made up by single person hh
                "hhdist_plots\\single_adult_hh.png", 
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\single_adult_hh.png")

###############################################################################
####### distribution of pensioner across council tax bands ###################
###############################################################################
hh_plots(over_pension, 'percent_pen', 'pen_input')                                                              #call function to generate data for the graphs: over pension age households (at least one adult) 
plot_hh_types(pen_input, pen_input$percent_pen,"#3e2272", 50, "Percentage of pensioner households (%)",         #plot the data: over pension age households (at least one adult) 
                "hhdist_plots\\pension_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\pension_hh.png")

###############################################################################
################## distribution of 3 or more children hh #####################
###############################################################################
hh_plots(three_more, 'percent_three', 'three_input')                                                                                    #call function to generate data for the graphs: three children or more
plot_hh_types(three_input, three_input$percent_three,"#632076", 50, "Percentage of households with three or more children (%)",         #plot the data: three children or more
                "hhdist_plots\\three_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\three_hh.png")

###############################################################################
################# distribution of two children hh #############################
###############################################################################
hh_plots(two_kids, 'percent_two', 'two_input')                                                                                          #call function to generate data for the graphs: two children
plot_hh_types(two_input, two_input$percent_two,"#851d76", 15, "Percentage of households with two children (%)",                         #plot data: two children
                "hhdist_plots\\two_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\two_hh.png")

###############################################################################
################ distribution of sinlge parents #############################
###############################################################################
hh_plots(single_parents, 'per_sing_par', 'sing_par_input')                                                                              #call function to generate data for the graphs: sinlge parent houesholds
plot_hh_types(sing_par_input, sing_par_input$per_sing_par,"#a41b72", 15, "Percentage of sinlge parent households (%)",                  #plot data: sinlge parent houesholds
                "hhdist_plots\\sing_par_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\sing_par_hh.png")

#############################################################################
#################### distribution of 50% council tax discounts ##############
#############################################################################
hh_fifty <- househol_raw |>
       select(SERNUM, CTBAND, GVTREGN, CT25D50D) |>                                                                     #filter out council tax band column, region and 25%/50% discount column 
       filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)                                                             #remove non england hosueholds and CTBAND NAs 
hh_fifty <- hh_fifty |>
    filter(CT25D50D == 2) |>                                                                                            #select households that recieve 50% discount and count by council tax band 
    select(SERNUM:CTBAND) |>
    group_by(CTBAND) |>
    count()                                                                                                             #count the number of 50% hh in each ct band 

hh_fifty <- left_join(hh_fifty, house_total, by = 'CTBAND')                                                             #join the total number of hh in each ct band
hh_fifty <- hh_fifty |>
    mutate(percent_fifty = n/total_hh * 100)                                                                            #calculate the percentage of 50% hh in each ct band

plot_hh_types(hh_fifty, hh_fifty$percent_fifty,"#bf1f6b", 1, "Percentage of 50% discount households (%)",               #plot data: fifty percent discount 
                "hhdist_plots\\fiftydis_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\fiftydis_hh.png")
write.csv(hh_fifty, 'data_output\\fifty_dis_hh.csv')                                                                    #save the data use to draw the graphs 

#############################################################################
#################### distribution of renters across ct bands ##############
#############################################################################
hh_renters <- househol_raw |>
       select(SERNUM, CTBAND, GVTREGN, TENURE) |>                                                                       #filter out council tax band column, region and type of hh ownership
       filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)                                                             #remove non england hosueholds and CTBAND NAs
hh_renters <- hh_renters |>                                                                                             #Select only renting hoseuholds 
    filter(TENURE == 4) |>
    select(SERNUM:CTBAND) |>
    group_by(CTBAND) |>                                                                                                 #count up how many in each ct band 
    count()

hh_renters <- left_join(hh_renters, house_total, by = 'CTBAND')                                                         #join total number of propertise in each ct band
hh_renters <- hh_renters |>
    mutate(percent_renters = n/total_hh * 100)                                                                          #calculate the percentage that are renters

plot_hh_types(hh_renters, hh_renters$percent_renters,"#d62d60", 60, "Percentage of rented households (%)",              #plot data: renters
                "hhdist_plots\\renters_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\renters_hh.png")


#############################################################################
#################### distribution of hh owners across ct bands ##############
#############################################################################
hh_owners <- househol_raw |>
       select(SERNUM, CTBAND, GVTREGN, TENURE) |>                                                                       #filter out council tax band column, region and type of hh ownership
       filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)                                                             #remove non england hosueholds and CTBAND NAs
hh_owners <- hh_owners |>
    filter(TENURE %in% owners_ish) |>                                                                                   #Select only owning hoseuholds
    select(SERNUM:CTBAND) |>
    group_by(CTBAND) |>                                                                                                 #count up how many in each ct band 
    count()

hh_owners <- left_join(hh_owners, house_total, by = 'CTBAND')                                                           #join total number of propertie in each ct band
hh_owners <- hh_owners |>
    mutate(percent_owners = n/total_hh * 100)                                                                           #calculate the percentage that are owners

plot_hh_types(hh_owners, hh_owners$percent_owners,"#d62d60", 100, "Percentage of owned households (%)",                 #plot data: owners
                "hhdist_plots\\owners_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\owners_hh.png")
