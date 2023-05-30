#30/05/2023
#Emily Keenan
### to look into the distribution of single persion households across council tax bands ###

#vector of missing value reference or households not valued separately 
na_s <- c(-1, 10)
not_england <-  c("299999999", "399999999", "499999999.0")
single_person <- c(3, 4, 9, 10, 11)

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


 #Select 1 adult households under pension age (HHCOMPS = 3, 4, 9, 10, 11)
 house_one <- house_compo |>
    filter(HHCOMPS %in% single_person)

#remove region and houeholds composition columns as no longer necessary 
house_one <- house_one |>
    select(SERNUM:CTBAND) |>
    group_by(CTBAND)|>
    count() |>
    rename(total_hh_one = n)



###calculate percentage of hosueholds in each band that are single adult households 

one_plot_input <- left_join(house_one, house_total, by = 'CTBAND')           #join the total number of hosueholds on 
one_plot_input <- one_plot_input |>
    mutate(percent_one = total_hh_one/total_hh) |>
    ungroup()
view(one_plot_input)
one_plot_input$CTBAND <- as.factor(one_plot_input$CTBAND)

 levels(one_plot_input$CTBAND) <- list('A' = 1,
                                        'B' =2, 
                                        'C' = 3,
                                        'D' = 4,
                                        'E' = 5,
                                        'F' = 6,
                                        'G' = 7,
                                        'H' = 8)

####plot the percentage of houses in each band that are single person houses####

plot_house_one <- one_plot_input %>% 
    ggplot(aes(x = factor(CTBAND, lables = c("A", "B", "C", "D", "E", "F", "G", "H")) , y = percent_one)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity", fill = "#012169")  +                                        #specify a bar chart
    theme(text = element_text(size = 18))+      
    theme_minimal()+                         #increase text size
    scale_x_discrete(breaks=1:8)+
    ylab("Percentage of households in each council tax band that are signle person households(%)") + 
    xlab("Council tax bands")                                           #add axis lables




print(plot_house_one)   
   
    ggplot2::ggsave(paste0( reg,"_hhdist.png"), d)

str(house_one)
view(one_plot_input)
view(house_one
)
view(tot_hh)
view(house_one)  
view(count(house_one))  
view(count(house_compo))
view(house_compo)

attr( house_one, "names")
