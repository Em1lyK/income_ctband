#30/05/2023
#Emily Keenan
### to look into the distribution of single persion households across council tax bands ###

#vector of missing value reference or households not valued separately 
na_s <- c(-1, 10)
not_england <-  c("299999999", "399999999", "499999999.0")
single_person <- c(3, 4, 9, 10, 11)
over_pension <- c(1, 2, 5, 6)
three_more <- c(11, 14, 17)
two_kids <- c(10, 13, 16)
single_parents <- c(9, 10, 11)
owners_ish <- c(1, 2)


#### function to isolate data for plotting #####

hh_plots <- function(type, percent_type, plot_input) {
    a <- house_compo |>
        filter(HHCOMPS %in% type)                           #select the households with the relevent composition

    a <- a |>                                               #group by council tax band and count up households in each CT band 
        select(SERNUM:CTBAND) |>
        group_by(CTBAND)|>
        count() 
    b <- left_join(a, house_total, by = 'CTBAND')           #join CT band hh totals to CT band hh of specific type
    
    b <- b |>
    mutate(!! paste0(percent_type) := n/total_hh*100) |>
    ungroup()

    return(assign(plot_input, b, envir = parent.frame()))

}

plot_hh_types <- function(plot_input, percent_type, colour, upper_lim, y_lable, file_loc, file_loc_x ) {
    a <- plot_input %>% 
        ggplot(aes(x = as.character(CTBAND) , y = percent_type)) +              #input data fro hh percentage and income band
        geom_bar(stat = "identity", fill = paste0(colour))  +                                        #specify a bar chart
        scale_y_continuous(limits = c(0,upper_lim)) +     
        theme_minimal() +                    
        scale_x_discrete(labels = c("1" = "A", "2" = "B", "3" =  "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"), position = "bottom") +
        ylab(paste0(y_lable)) + 
        xlab("Council tax bands")                                           #add axis lables
    a <- a + theme(text = element_text(size = 20))                  #increase text size 
   
    ggplot2::ggsave(paste0(file_loc), a)
    ggplot2::ggsave(paste0(file_loc_x), a)

    return(a)
}

#select households number, council tax band, region, household composition
house_compo <- househol_raw |>
    select(SERNUM, CTBAND, GVTREGN, HHCOMPS)

#remove missing values, households not valued separately and select only England

house_compo <- house_compo |>
 filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)

#calculating the total number of households in each band
house_total <- house_compo |>
    group_by(CTBAND) |>
    count() |>
    rename(total_hh = n)

#call function to generate data for the graphs: single adult households 
hh_plots(single_person, 'percent_one', 'input_one')

####plot the percentage of houses in each band that are single person houses####
plot_hh_types(input_one, input_one$percent_one,"#012169", 50, "Percentage of single person households (%)", 
                "hhdist_plots\\single_adult_hh.png", 
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\single_adult_hh.png")


###################################################################################################################
############# alternative plot of the percentage of single adult hosueholds in each council tax band ###########
####################################################################################################################

total_singlehh <- sum(house_one$total_hh_one)
prop_singlehh <- house_one |>
    mutate(prop_one = total_hh_one/total_singlehh * 100)

plot_prop_sinlgehh <- prop_singlehh %>% 
    ggplot(aes(x = as.character(CTBAND) , y = prop_one)) +                                  #input data fro hh percentage and income band
    geom_bar(stat = "identity", fill = "#012169")  +                                        #specify a bar chart
    scale_y_continuous(limits = c(0,50)) +     
    theme_minimal() +                    
    scale_x_discrete(labels = c("1" = "A", "2" = "B", "3" =  "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"), position = "bottom") +
    ylab("Percentage of single adult households in each council tax band (%)") + 
    xlab("Council tax bands")                                           #add axis lables
plot_prop_sinlgehh <- plot_prop_sinlgehh + theme(text = element_text(size = 17))                  #increase text size

print(plot_prop_sinlgehh)

ggplot2::ggsave("hhdist_plots\\prop_single_adulthh.png", plot_prop_sinlgehh)
ggplot2::ggsave("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\prop_single_adulthh.png", plot_prop_sinlgehh)

###############################################################################
####### distribution of pensioner across council tax bands ###################
###############################################################################

#call function to generate data for the graphs: over pension age households (at least one adult) 
hh_plots(over_pension, 'percent_pen', 'pen_input')

# plot the data: over pension age households (at least one adult) 
plot_hh_types(pen_input, pen_input$percent_pen,"#3e2272", 50, "Percentage of pensioner households (%)", 
                "hhdist_plots\\pension_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\pension_hh.png")

###############################################################################
####### distribution of 3 or more children hh ###################
###############################################################################

#call function to generate data for the graphs: three children or more
hh_plots(three_more, 'percent_three', 'three_input')

#plot data: three children or more
plot_hh_types(three_input, three_input$percent_three,"#632076", 50, "Percentage of households with three or more children (%)", 
                "hhdist_plots\\three_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\three_hh.png")

###############################################################################
####### distribution of two children hh ###################
###############################################################################

#call function to generate data for the graphs: two children
hh_plots(two_kids, 'percent_two', 'two_input')

#plot data: two children
plot_hh_types(two_input, two_input$percent_two,"#851d76", 15, "Percentage of households with two children (%)", 
                "hhdist_plots\\two_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\two_hh.png")

###############################################################################
####### distribution of sinlge parents ###################
###############################################################################
#call function to generate data for the graphs: sinlge parent houesholds
hh_plots(single_parents, 'per_sing_par', 'sing_par_input')

#plot data: sinlge parent houesholds
plot_hh_types(sing_par_input, sing_par_input$per_sing_par,"#a41b72", 15, "Percentage of sinlge parent households (%)", 
                "hhdist_plots\\sing_par_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\sing_par_hh.png")

#############################################################################
#################### distribution of 50% council tax discounts ##############
#############################################################################
#filter out council tax band column, region and 25%/50% discount column 
#remove non england hosueholds and CTBAND NAs 
hh_fifty <- househol_raw |>
       select(SERNUM, CTBAND, GVTREGN, CT25D50D) |>
       filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)

#select households that recieve 50% discount and count by council tax band 
hh_fifty <- hh_fifty |>
    filter(CT25D50D == 2) |>
    select(SERNUM:CTBAND) |>
    group_by(CTBAND) |>
    count()

hh_fifty <- left_join(hh_fifty, house_total, by = 'CTBAND')
hh_fifty <- hh_fifty |>
    mutate(percent_fifty = n/total_hh * 100)

#plot data: fifty percent discount
plot_hh_types(hh_fifty, hh_fifty$percent_fifty,"#bf1f6b", 1, "Percentage of 50% discount households (%)", 
                "hhdist_plots\\fiftydis_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\fiftydis_hh.png")

write.csv(hh_fifty, 'data_output\\fifty_dis_hh.csv')

#############################################################################
#################### distribution of renters across ct bands ##############
#############################################################################

#select the ct band, region and tenure column and remove non England and nas 
hh_renters <- househol_raw |>
       select(SERNUM, CTBAND, GVTREGN, TENURE) |>
       filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)
#Select only renting hoseuholds and count up how many in each ct band 
hh_renters <- hh_renters |>
    filter(TENURE == 4) |>
    select(SERNUM:CTBAND) |>
    group_by(CTBAND) |>
    count()
#join total number of propertie in each ct band and calculate the percentage that are renters
hh_renters <- left_join(hh_renters, house_total, by = 'CTBAND')
hh_renters <- hh_renters |>
    mutate(percent_renters = n/total_hh * 100)

#plot data: renters
plot_hh_types(hh_renters, hh_renters$percent_renters,"#d62d60", 60, "Percentage of rented households (%)", 
                "hhdist_plots\\renters_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\renters_hh.png")


#############################################################################
#################### distribution of hh owners across ct bands ##############
#############################################################################
#select the ct band, region and tenure column and remove non England and nas 
hh_owners <- househol_raw |>
       select(SERNUM, CTBAND, GVTREGN, TENURE) |>
       filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)
#Select only owning hoseuholds and count up how many in each ct band 
hh_owners <- hh_owners |>
    filter(TENURE %in% owners_ish) |>
    select(SERNUM:CTBAND) |>
    group_by(CTBAND) |>
    count()
#join total number of propertie in each ct band and calculate the percentage that are owners
hh_owners <- left_join(hh_owners, house_total, by = 'CTBAND')
hh_owners <- hh_owners |>
    mutate(percent_owners = n/total_hh * 100)

#plot data: owners
plot_hh_types(hh_owners, hh_owners$percent_owners,"#d62d60", 100, "Percentage of owned households (%)", 
                "hhdist_plots\\owners_hh.png",
                "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\owners_hh.png")

str(hh_fifty)
view(hh_fifty)
view(house_total)
view(three_ihouse_totalnput)
view(house_three)
view(pen_input)
view(house_pension)
view(prop_singlehh)
view(total_singlehh)
str(one_plot_input)
str(house_one)
view(one_plot_input)
view(house_one)
view(tot_hh)
view(house_one)  
view(count(house_one))  
view(count(house_compo))
view(house_compo)

attr( house_one, "names")
