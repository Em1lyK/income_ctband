#01/06/23
#Emily Keenan
#to calculate the average income backet of hoseuholds in band A in each region

#vectors to clean data
na_s <- c(-1, 10)                                                                       #NAs or not independently valued property
not_england <-  c("299999999", "399999999", "499999999.0")                              #codes for Scot, NI, Wales


househol_band <- househol_raw |>
        select(SERNUM, CTBAND, GVTREGN, HHINCBND) |>
        filter(!CTBAND %in% na_s, !GVTREGN %in% not_england)


plot_ave_inc <- function(band, y_lim, file_loc, file_loc_x) {
    #select band A and remove CT band column
    a <- househol_band |>
        filter(CTBAND == band) |>
        select(!CTBAND)|>
        group_by(GVTREGN, HHINCBND) |>
        count() |>
        ungroup()
        
    #####claculate the mean inc band in each region####

    a <- a |>
        mutate(inc_x_n = HHINCBND*n) |>         #frequency times incombe bands
        group_by(GVTREGN) |>                    #group by region
        mutate(reg_total = sum(inc_x_n)) |>       #add together the frequency times inc bracket for all regions
        mutate(reg_hh_tot = sum(n)) |>
        ungroup()

    a <- a |>
        mutate(mean = reg_total/reg_hh_tot)     #calculate themean for each region

    a <- a |>         #remove repeats (due to my messy method - I'm sure there is a better way)
        select(GVTREGN, mean) |>
        unique() |>
        arrange(mean)

    #### plot the means ####


    x_order <- as.character(a$GVTREGN)     #creating an order to control the x axsis points order

    mean_a_plot <- ggplot(a,  aes(x=as.character(GVTREGN), y=mean)) +
    geom_point(size=5, shape=16, color = '#012169') +
    theme_minimal() +
    scale_x_discrete(limits = x_order, labels = c('112000008' = 'SE', '112000005' = 'WM', '112000007' = 'L', 
                                                    '112000002' = 'NW', '112000006' = 'EE', '112000009' = 'SW', 
                                                    '112000001' = 'NE', '112000003' = 'YH','112000004' = 'EM')) +
    scale_y_continuous(limits = c(0, y_lim), breaks = seq(0, y_lim, by = 1))+
    theme(text = element_text(size = 20)) +
    ylab('Average income bracket') +
    xlab('Region')

    ggplot2::ggsave(paste0(file_loc), mean_a_plot)
    ggplot2::ggsave(paste0(file_loc_x), mean_a_plot)

    return(mean_a_plot)

}

plot_ave_inc(1, 4, 'hhdist_plots\\a_ave_inc.png', 'D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\a_ave_inc.png')


plot_ave_inc(2, 5, 'hhdist_plots\\b_ave_inc.png', 'D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\b_ave_inc.png')

plot_ave_inc(3, 6, 'hhdist_plots\\c_ave_inc.png', 'D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\c_ave_inc.png')


##############calculate for all the bands and put on the same graph?
ave_inc <- function(band, band_ave) {
    #select band A and remove CT band column
    b <- househol_band |>
        filter(CTBAND == band) |>
        select(!CTBAND)|>
        group_by(GVTREGN, HHINCBND) |>
        count() |>
        ungroup()
        
    #####claculate the mean inc band in each region####

    b <- b |>
        mutate(inc_x_n = HHINCBND*n) |>         #frequency times incombe bands
        group_by(GVTREGN) |>                    #group by region
        mutate(reg_total = sum(inc_x_n)) |>       #add together the frequency times inc bracket for all regions
        mutate(reg_hh_tot = sum(n)) |>
        ungroup()

    b <- b |>
        mutate(mean = reg_total/reg_hh_tot)     #calculate themean for each region

    b <- b |>         #remove repeats (due to my messy method - I'm sure there is a better way)
        select(GVTREGN, mean) |>
        unique() |>
        arrange(mean)
    
    the_band <- rep(band, each=9)
    b <- cbind(b,the_band)

    return(assign(band_ave, b, envir = parent.frame()))

}

ave_inc(1, 'a_aveinc')
ave_inc(2, 'b_aveinc')
ave_inc(3, 'c_aveinc')
ave_inc(4, 'd_aveinc')
ave_inc(5, 'e_aveinc')
ave_inc(6, 'f_aveinc')
ave_inc(7, 'g_aveinc')
ave_inc(8, 'h_aveinc')
 
aveinc_region <- rbind(a_aveinc, b_aveinc, c_aveinc, d_aveinc, e_aveinc, f_aveinc, g_aveinc, h_aveinc)


aveinc_region$the_band <- as.factor(aveinc_region$the_band)                     #change ctband to factor type so it can be lablled on the graph
levels(aveinc_region$the_band) <- list( "A" = 1,        #add the corresponding income bands in exchange for the numbers  
                                "B" = 2, 
                                "C" = 3, 
                                "D" = 4, 
                                "E" = 5, 
                                "F" = 6, 
                                "G" = 7, 
                                "H" = 8)

plot_region_aveinc <- ggplot(aveinc_region, aes(GVTREGN, mean, group = as.factor(the_band), color = as.factor(the_band))) + 
                        geom_point(size=5, shape=16) +
                        theme(text = element_text(size = 20)) +
                        scale_color_brewer(palette = "Paired")
                        #+
                        #scale_colour_manual(palette = paired)
                        #scale_colour_discrete(name = "CT Band", breaks = seq(1, 8, by = 1))
plot_region_aveinc


plot_region_aveinc_line <- ggplot(aveinc_region, aes(x = as.character(GVTREGN),y = mean, group = as.factor(the_band), color = as.factor(the_band))) + 
                        geom_line(size = 1.2) +
                           scale_x_discrete(labels = c('112000008' = 'SE', '112000005' = 'WM', '112000007' = 'L', 
                                                    '112000002' = 'NW', '112000006' = 'EE', '112000009' = 'SW', 
                                                     '112000001' = 'NE', '112000003' = 'YH','112000004' = 'EM')) +
                        xlab('Region') +
                        ylab('Income bracket') +
                        theme(text = element_text(size = 20)) +
                        scale_color_brewer(palette = "Paired") +
                        labs(fill = 'CT Band')



                        labs(fill='NEW LEGEND TITLE') 
                        scale_fill_discrete(name = "Council Tax Bands")
                        ggplot2::guides(fill=guide_legend(title="Council Tax Bands"))
                              scale_fill_discrete(name = "Council Tax Bands")
                        #+
                        #scale_colour_manual(palette = paired)
                        #scale_colour_discrete(name = "CT Band", breaks = seq(1, 8, by = 1))
plot_region_aveinc_line


 ggplot2::ggsave('D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\all_ave_inc.png', plot_region_aveinc)
plot_region_aveinc 


#MAKE THE GRAPH BETTER 
#START BY CHANGING HTE COLOURS
#LINES ????

  banda_mean_inc <- banda_mean_inc + scale_y_continuous(labels = c(1 = "Less than 200", 
                                                                    2 = "200 and less than  400", 
                                                                    3 =  "400 and less than 600", 
                                                                    4 = "600 and less than 800"))
      

print(banda_mean_inc)

view(househol_banda)
