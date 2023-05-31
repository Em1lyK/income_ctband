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
plot_hh_types(input_one, input_one$percent_one,"#012169", 50, "Percentage of single person households (%)", "hhdist_plots\\single_adult_hh.png", "D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\single_adult_hh.png")


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


plot_pension <- pen_input %>% 
    ggplot(aes(x = as.character(CTBAND) , y = percent_pen)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity", fill = "#3e2272")  +                                        #specify a bar chart
    scale_y_continuous(limits = c(0,50)) +     
    theme_minimal() +                    
    scale_x_discrete(labels = c("1" = "A", "2" = "B", "3" =  "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"), position = "bottom") +
    ylab("Percentage of pensioner households (%)") + 
    xlab("Council tax bands")                                           #add axis lables
plot_pension <- plot_pension + theme(text = element_text(size = 18))                  #increase text size

print(plot_pension)   
   
ggplot2::ggsave("hhdist_plots\\pension_hh.png", plot_pension)
ggplot2::ggsave("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\pension_hh.png", plot_pension)

###############################################################################
####### distribution of 3 or more children hh ###################
###############################################################################

#call function to generate data for the graphs: three children or more
hh_plots(three_more, 'percent_three', 'three_input')

plot_three <- three_input %>% 
    ggplot(aes(x = as.character(CTBAND) , y = percent_three)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity", fill = "#632076")  +                                        #specify a bar chart
    scale_y_continuous(limits = c(0,50)) +     
    theme_minimal() +                    
    scale_x_discrete(labels = c("1" = "A", "2" = "B", "3" =  "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"), position = "bottom") +
    ylab("Percentage of households with three or more children (%)") + 
    xlab("Council tax bands")                                           #add axis lables
plot_three <- plot_three + theme(text = element_text(size = 18))                  #increase text size

print(plot_three)   
   
ggplot2::ggsave("hhdist_plots\\three_hh.png", plot_three)
ggplot2::ggsave("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\three_hh.png", plot_three)


###############################################################################
####### distribution of two children hh ###################
###############################################################################

#call function to generate data for the graphs: two children
hh_plots(two_kids, 'percent_two', 'two_input')

plot_two <- two_input %>% 
    ggplot(aes(x = as.character(CTBAND) , y = percent_two)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity", fill = "#851d76")  +                                        #specify a bar chart
    scale_y_continuous(limits = c(0,15)) +     
    theme_minimal() +                    
    scale_x_discrete(labels = c("1" = "A", "2" = "B", "3" =  "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"), position = "bottom") +
    ylab("Percentage of households with two children (%)") + 
    xlab("Council tax bands")                                           #add axis lables
plot_two <- plot_two + theme(text = element_text(size = 20))                  #increase text size

print(plot_two)   
   
ggplot2::ggsave("hhdist_plots\\two_hh.png", plot_two)
ggplot2::ggsave("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\two_hh.png", plot_two)

###############################################################################
####### distribution of sinlge parents ###################
###############################################################################
#call function to generate data for the graphs: sinlge parent houesholds
hh_plots(single_parents, 'per_sing_par', 'sing_par_input')

plot_sing_par <- sing_par_input %>% 
    ggplot(aes(x = as.character(CTBAND) , y = per_sing_par)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity", fill = "#a41b72")  +                                        #specify a bar chart
    scale_y_continuous(limits = c(0,15)) +     
    theme_minimal() +                    
    scale_x_discrete(labels = c("1" = "A", "2" = "B", "3" =  "C", "4" = "D", "5" = "E", "6" = "F", "7" = "G", "8" = "H"), position = "bottom") +
    ylab("Percentage of sinlge parent households (%)") + 
    xlab("Council tax bands")                                           #add axis lables
plot_sing_par <-plot_sing_par + theme(text = element_text(size = 20))                  #increase text size

print(plot_sing_par)   
   
ggplot2::ggsave("hhdist_plots\\sing_par_hh.png", plot_sing_par)
ggplot2::ggsave("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\sing_par_hh.png", plot_sing_par)

view(sing_par_input)
view(hh_sinlge_par)
view(three_input)
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
