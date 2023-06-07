letter numebr change

let_num <- data.frame(old_name = c("1", "2", "3", "4", "5", "6", "7", "8"),
                        new_name = c("a", "b", "c", "d", "e", "f", "g", "h"))



#define yusuf's beautiful colour scheme 
yusuf_pal_full <- c("#012169", "#3e2272", "#632076","#851d76", "#a41b72", "#bf1f6b", "#d62d60", "#e94154", "#f65846", "#ff7135", "#ff8b21", "#ffa600")


#distribution of icnome within council tax bands by region eng



region_dist <- function(a, plot_data, c, reg, yaxis) {
    
    #define vectors of regions that should keep band H and regions that should lose band H - probably need a more analytical method to decided when we keep of lose band H
    keep_h <- c("North West", "East England", "West Midlands", "London", "East Midlands")                 
    remove_h <- c("South East", "North East", "South West", "Yorkshire and Humber")                       

    #selecte the region
    b <- a |>
    filter(GVTREGN == c)

    #count the number of houses in each council tax band and income band
    b <- b %>% 
    group_by(CTBAND,HHINCBND) %>%
    count() 

    #calculate the percentage of houses in the region selected in each income band within each council tax band
    b <- b |>
    filter(CTBAND != -1, CTBAND != 10) |>
    group_by(CTBAND) |>
    mutate(total = sum(n)) |>
    mutate(percent = n/total*100)


    # #change income band to factor type 
    b$HHINCBND <- as.factor(b$HHINCBND)

    #add the corresponding income bands in exchange for the numbers  
    levels(b$HHINCBND) <- list( "Less than 200" = 1, 
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

    #if statment to draw differenct graphs depnding on if the region should keep band H in or not
    if (paste0(yaxis) %in% keep_h ) {
    d <- b %>% 
    ggplot(aes(x = HHINCBND, y = percent, fill=HHINCBND)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity") +                                        #specify a bar chart
   scale_fill_manual(values=yusuf_pal_full)+                             #add thecolour palette
   scale_y_continuous(limits = c(0,50))+
    theme(axis.text.x=element_blank(),                                   #remove x axis labels
            axis.ticks.x=element_blank(),
            text = element_text(size = 18))+                               #remove x axis ticks
    ylab(paste0("Percentage of households by council tax band and income band in the ", yaxis, " (%)")) + 
    xlab("Income bands") +                                              #add axis lables
    ggplot2::guides(fill=guide_legend(title="Income bands \n per week")) +          #add lgend title
    facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))     #make multiple graphs by CT band

    ggplot2::ggsave(paste0( reg,"_hhdist.png"), d)

    } else {
    d <- b %>% 
    filter(CTBAND != 8)|>
    ggplot(aes(x = HHINCBND, y = percent, fill=HHINCBND)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity") +                                        #specify a bar chart
   scale_fill_manual(values=yusuf_pal_full)+                             #add thecolour palette
   scale_y_continuous(limits = c(0,50))+
    theme(axis.text.x=element_blank(),                                   #remove x axis labels
            axis.ticks.x=element_blank(),
            text = element_text(size = 18))+                               #remove x axis ticks
    ylab(paste0("Percentage of households by council tax band and income band in the ", yaxis, " (%)")) + 
    xlab("Income bands") +                                              #add axis lables
    ggplot2::guides(fill=guide_legend(title="Income bands \n per week")) +          #add lgend title
    facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))     #make multiple graphs by CT band

    ggplot2::ggsave(paste0( reg,"_hhdist.png"), d)
    }

    return(assign(paste0(plot_data), b, envir = parent.frame()))

}

#code fre uses for each region
ne <- 112000001.0
nw <- 112000002.0
yh <- 112000003.0
em <- 112000004.0
wm <- 112000005.0
ee <- 112000006.0
l <- 112000007.0
se <- 112000008.0
sw <- 112000009.0


#selecting the ct bands, income range and regional variables 
househol_region <- househol_raw |>
    select(SERNUM, CTBAND, HHINCBND, GVTREGN)

region_dist(househol_region, 'househol_ne', ne, "hhdist_plots\\NE", "North East")
region_dist(househol_region, 'househol_nw', nw, "hhdist_plots\\NW", "North West")
region_dist(househol_region, 'househol_yh', yh, "hhdist_plots\\YH", "Yorkshire and Humber ")
region_dist(househol_region, 'househol_em', em, "hhdist_plots\\EM", "East Midlands")
region_dist(househol_region, 'househol_wm', wm, "hhdist_plots\\WM", "West Midlands")
region_dist(househol_region, 'househol_ee', ee, "hhdist_plots\\EE", "East Engalnd")
region_dist(househol_region, 'househol_l', l, "hhdist_plots\\L", "London")
region_dist(househol_region, 'househol_se', se, "hhdist_plots\\SE", "South East")
region_dist(househol_region, 'househol_sw', sw, "hhdist_plots\\SW", "South West")


#pull out input data
nw_plot_input <- b |>
ungroup() |>
remove_labels() 
write.xlsx(nw_plot_input, "data_output\\nw_plot_input.xlsx")
str(eng_plot_input)


wm_plot_input <- b |>
ungroup() |>
remove_labels() 
write.xlsx(nw_plot_input, "data_output\\wm_plot_input.xlsx")
str(eng_plot_input)

#########################################################################
##### check the number of hosueholds in each region #####################
########################################################################

count_reghh <- househol_region |>
    filter(CTBAND != 10) |>
    select(SERNUM, GVTREGN) |>
    group_by(GVTREGN)|>
    count()

regions <- c("NE", "NW", "YH", "EM", "WM", "EE", "L", "SE", "SW", "SCOT", "WAL", "NI")
GVTREGN <- as.numeric(c(112000001.0, 112000002.0, 112000003.0, 112000004.0, 112000005.0, 112000006.0, 112000007.0, 112000008.0, 112000009.0, 299999999, 399999999, 499999999))

region_wcode <- as.data.frame(cbind(regions, as.numeric(GVTREGN)))
region_wcode <- transform(region_wcode, GVTREGN = as.numeric(GVTREGN))

count_reghh <- left_join(count_reghh, region_wcode, by = "GVTREGN")

count_reghh <- count_reghh |>
    select(!V2)

###################################################################
###################### comapre regional dist ######################
###################################################################

### Family Resources Survey

#group the df by council tax band and region 
compreg_dist <- househol_region |>

    group_by(CTBAND, GVTREGN) |>
    count()

#add the total number of household in each region to the df
compreg_dist <- compreg_dist |>
    left_join(count_reghh, by = 'GVTREGN')

#calculate the percentage of househoulds in each council tax band by region
compreg_dist <- compreg_dist |>
    rename(no_hh = n.x, hh_region = n.y) |>
    mutate(frs_banddis = no_hh/hh_region) |>
    filter(!regions %in% c("SCOT", "WAL", "NI")) |>
    filter(!CTBAND == 10)

#view(compreg_dist)

### CTB

# group by region and band, tie together region and band
ctb_regval <- ctb_val |>
    select(region, name, value) |>
    group_by(region, name) |>
    mutate(reg_band = str_c(region, name)) |>
    ungroup()

#add together the households in each council tax band and each region
ctb_regval <- ctb_regval |>
    group_by(reg_band) |>
    summarise(total = sum(value))

#separate out band and region again
ctb_regionhh <- data.frame(do.call('rbind', strsplit(as.character(ctb_regval$reg_band), "band_", fixed=TRUE)))
ctb_regval <- cbind(ctb_regval, ctb_regionhh)
ctb_regval <- ctb_regval |>
    select(!reg_band)

#sum up the total numbers of households in the valuation list for each region
ctb_regtot <- ctb_regval |>
    group_by(X1) |>
    summarise(reg_total = sum(total))

#remove unecessary rows
ctb_regtot <- ctb_regtot |>
    filter(!grepl("total", X1)) |>
    filter (!grepl("ENG", X1))

#join totals to the regional band totals 
ctb_regdist <- left_join(ctb_regval, ctb_regtot, by = "X1")

#tidy ragional ctb datat frame and calculate ct band households distributtions by region
ctb_regdist <- ctb_regdist |>
    na.omit() |>
    mutate(ctb_banddis = total/reg_total*100) |>
    rename(region = X1, ct_band = X2) 

#change the FRS 1 to 8 bands to letter

compreg_dist2 <-compreg_dist

compreg_dist2 <- as.character(compreg_dist2$CTBAND)
compreg_dist2[compreg_dist2 == "1"] <- "a"
compreg_dist2[compreg_dist2 == "2"] <- "b"
compreg_dist2[compreg_dist2 == "3"] <- "c"
compreg_dist2[compreg_dist2 == "4"] <- "d"
compreg_dist2[compreg_dist2 == "5"] <- "e"
compreg_dist2[compreg_dist2 == "6"] <- "f"
compreg_dist2[compreg_dist2 == "7"] <- "g"
compreg_dist2[compreg_dist2 == "8"] <- "h"

#add the letter ct band column back in 
compreg_dist3 <- cbind(compreg_dist, compreg_dist2)

#tidy frs region hh dist data frame
compreg_dist3 <- compreg_dist3 |>
    rename(ct_band = ...7) |>
    relocate(ct_band) 

#select relevent columns
compreg_dist3 <- compreg_dist3 |>
    select(ct_band:frs_banddis)|>
    mutate(frs_banddis = frs_banddis*100)

view(compreg_dist3)

################################
#function to compare the distribution of households in ct bands in different regions -
# STILL WORKING ON THE WRITE TO CSV FILED PART OF THIS TO MAKE THE NAME CHANGE PASTE0
regional_distribution <- function(b, d, g, f) {
    #select region from frs
    a <- ctb_regdist |>
        filter(region == b)

    #select the same region from ctb
    c <- compreg_dist3 |>
        filter(regions == d)
    
    #join the two dist

    e <<- left_join(a, c, by = "ct_band")

    e <<- e |>
        select(region, ct_band, regions, ctb_banddis, frs_banddis) |>
        mutate(dist_diff = ctb_banddis - frs_banddis)

    #write.csv(e, paste0("data_output\\",f , ".csv"))

    #write.csv(e, "data_output\\EE_hhdist.csv")
    write.csv(e, paste0(g))

    return(assign(f, e, envir = parent.frame()))

}
 

#CALL function for each region
regional_distribution("E", "EE", "data_output\\EE_hhdist.csv", "EE_hhdist")
regional_distribution("EM", "EM", "data_output\\EM_hhdist.csv", "EM_hhdist")
regional_distribution("L", "L", "data_output\\L_hhdist.csv", "L_hhdist")
regional_distribution("NE", "NE", "data_output\\NE_hhdist.csv", "NE_hhdist")
regional_distribution("NW", "NW", "data_output\\NW_dist.csv", "NW_hhdist" )
regional_distribution("SE", "SE", "data_output\\SE_hhdist.csv", "SE_hhdist")
regional_distribution("SW", "SW", "data_output\\SW_hhdist.csv", "SW_hhdist")
regional_distribution("WM", "WM", "data_output\\WM_hhdist.csv", "WM_hhdist")
regional_distribution("YH", "YH", "data_output\\YH_hhdist.csv", "YH_hhdist")
view(YH_hhdist)
view(NE_hhdist)
view(L_hhdist)
view(EE_hhdist)
view(EM_hhdist)
view(SW_hhdist)
view(WM_hhdist)
view(NW_hhdist)

#collate all the regional hh dist comparisions
region_hhdist <- rbind(EE_hhdist, EM_hhdist, L_hhdist, NE_hhdist, NW_hhdist, SE_hhdist, SW_hhdist, WM_hhdist, YH_hhdist)

#arrage with the largest dist differences between the frs and ctb at the top
region_hhdist <- region_hhdist |>
    arrange(desc(abs(dist_diff)))

view(region_hhdist)

#write regional dist comparisions to output folder
write.csv(region_hhdist, "data_output\\region_hhdist.csv")

#write the summary of the sample size for each region to the output file
count_reghh <- count_reghh |>
    rename(no_hh = n)

write.csv(count_reghh, "data_output\\frs_reghh.csv")

view(count_reghh)

##### checking all the data is present in each ctb and frs data frame
##checks 
count_frs <- compreg_dist |>
    group_by(CTBAND) |>
    count()

count_regdist <- ctb_regdist |>
    group_by(X2) |>
    count()



write.csv(compreg_dist, "data_output\\frs_regdist.csv")
write.csv(ctb_regdist, "data_output\\ctb_regdist.csv")

view(count_regdist)
view(count_frs)

view(compreg_dist2)
view(ctb_regdist)
view(ctb_regtot)
view(ctb_regionhh)
view(ctb_regval)
view(ctb_val)
