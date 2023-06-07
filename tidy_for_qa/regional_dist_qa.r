#05/06/23
#Emily Keenan
#code to check the regional distribution of hoseuholds (hh) across ct bands in the family resouces survey (frs) comapred to the ctb regional hosuehold distribution

#define yusuf's beautiful colour scheme 
yusuf_pal_full <- c("#012169", "#3e2272", "#632076","#851d76", "#a41b72", "#bf1f6b", "#d62d60", "#e94154", "#f65846", "#ff7135", "#ff8b21", "#ffa600")
#code frs uses for each region - found in the data dictionary in the 'mrdoc' file
ne <- 112000001.0
nw <- 112000002.0
yh <- 112000003.0
em <- 112000004.0
wm <- 112000005.0
ee <- 112000006.0
l <- 112000007.0
se <- 112000008.0
sw <- 112000009.0

####function to draw plots of the regional distribution of hh across ct bands 
region_dist <- function(a, b, c, reg, yaxis) {
    #define vectors of regions that should keep band H and regions that should lose band H - probably need a more analytical method to decided when we keep of lose band H (upsets my y axis scale)
    keep_h <- c("North West", "East England", "West Midlands", "London", "East Midlands")                 
    remove_h <- c("South East", "North East", "South West", "Yorkshire and Humber")   

    b <- a |>
    filter(GVTREGN == c)                     #selecte the region

    b <- b %>% 
    group_by(CTBAND,HHINCBND) %>%            #count the number of houses in each council tax band and income band
    count() 

    b <- b |>
    filter(CTBAND != -1, CTBAND != 10) |>   #remove nas and hh not valued separately
    group_by(CTBAND) |>
    mutate(total = sum(n)) |>                #calculate the total number of hh in each ct band
    mutate(percent = n/total*100)            #calculate the percentage of houses in the region selected in each income band within each council tax band

    b$HHINCBND <- as.factor(b$HHINCBND)     #change income band to factor type so it can be re-lablelled on the graphs

    levels(b$HHINCBND) <- list(  "Less than 10,400" = 1,        #add the corresponding income bands in exchange for the numbers  
                                "10,400 and less than  20,700" = 2, 
                                "20,700 and less than 31,300" = 3, 
                                "31,300 and less than 41,700" = 4, 
                                "41,700 and less than 52,200" = 5, 
                                "52,200 and less than 62,200" = 6, 
                                "62,200 and less than 73,000" = 7, 
                                "73,000 and less than 83,500" = 8, 
                                "83,500 and less than 93,900" = 9, 
                                "93,900 and less than 104,000" = 10, 
                                "Above 104,000" = 11)

    #if statment to draw differenct graphs depnding on if the region should keep band H in or not
    if (paste0(yaxis) %in% keep_h ) {
    d <<- b %>% 
    ggplot(aes(x = HHINCBND, y = percent, fill=HHINCBND)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity") +                                        #specify a bar chart
   scale_fill_manual(values=yusuf_pal_full)+                             #add the colour palette
   scale_y_continuous(limits = c(0,50))+                                 #set y axis limits
    theme(axis.text.x=element_blank(),                                   #remove x axis labels
            axis.ticks.x=element_blank(),
            text = element_text(size = 18))+                             #remove x axis ticks
    ylab(paste0("Percentage of households by council tax band and income band in the ", yaxis, " (%)")) + 
    xlab("Income bands") +                                               #add axis lables
    ggplot2::guides(fill=guide_legend(title="Income bands \n per week")) +          #add lgend title
    facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))     #make multiple graphs by CT band

    ggplot2::ggsave(paste0( reg,"_hhdist.png"), d)                      #save  plot                   

    } else {
    d <<- b %>% 
    filter(CTBAND != 8)|>
    ggplot(aes(x = HHINCBND, y = percent, fill=HHINCBND)) +              #input data fro hh percentage and income band
    geom_bar(stat = "identity") +                                        #specify a bar chart
   scale_fill_manual(values=yusuf_pal_full)+                             #add thecolour palette
   scale_y_continuous(limits = c(0,50))+                                 #set y axis limits
    theme(axis.text.x=element_blank(),                                   #remove x axis labels
            axis.ticks.x=element_blank(),
            text = element_text(size = 18))+                             #remove x axis ticks
    ylab(paste0("Percentage of households by council tax band and income band in the ", yaxis, " (%)")) + 
    xlab("Income bands") +                                              #add axis lables
    ggplot2::guides(fill=guide_legend(title="Income bands \n per week")) +          #add lgend title
    facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))     #make multiple graphs by CT band

    ggplot2::ggsave(paste0( reg,"_hhdist.png"), d)                      #save  plot
    }
}

househol_region <- househol_raw |>
    select(SERNUM, CTBAND, HHINCBND, GVTREGN)                           #selecting the ct bands, income range and regional variables 

#call the regional plotting function fore each region
region_dist(househol_region, househol_ne, ne, "hhdist_plots\\NE", "North East")
region_dist(househol_region, househol_nw, nw, "hhdist_plots\\NW", "North West")
region_dist(househol_region, househol_yh, yh, "hhdist_plots\\YH", "Yorkshire and Humber ")
region_dist(househol_region, househol_em, em, "hhdist_plots\\EM", "East Midlands")
region_dist(househol_region, househol_wm, wm, "hhdist_plots\\WM", "West Midlands")
region_dist(househol_region, househol_ee, ee, "hhdist_plots\\EE", "East Engalnd")
region_dist(househol_region, househol_l, l, "hhdist_plots\\L", "London")
region_dist(househol_region, househol_se, se, "hhdist_plots\\SE", "South East")
region_dist(househol_region, househol_sw, sw, "hhdist_plots\\SW", "South West")


#save input data for NW or WM
nw_plot_input <- b |>
ungroup() |>
remove_labels() 
write.xlsx(nw_plot_input, "data_output\\nw_plot_input.xlsx")

wm_plot_input <- b |>
ungroup() |>
remove_labels() 
write.xlsx(nw_plot_input, "data_output\\wm_plot_input.xlsx")

#########################################################################
##### check the number of hosueholds in each region #####################
########################################################################

count_reghh <- househol_region |>
    filter(CTBAND != 10) |>
    select(SERNUM, GVTREGN) |>
    group_by(GVTREGN)|>             
    count()                             #count the number of hoseuohlds in each resion

regions <- c("NE", "NW", "YH", "EM", "WM", "EE", "L", "SE", "SW", "SCOT", "WAL", "NI")      #name of regions
GVTREGN <- c(112000001.0, 112000002.0, 112000003.0, 112000004.0, 112000005.0, 
                        112000006.0, 112000007.0, 112000008.0, 112000009.0, 299999999, 
                        399999999, 499999999)                                               #corresponding codes of regions 

region_wcode <- as.data.frame(cbind(regions, GVTREGN))                          #combine region codes and names in the same df
region_wcode <- transform(region_wcode, GVTREGN = as.numeric(GVTREGN))          #make the region code a number       
count_reghh <- left_join(count_reghh, region_wcode, by = "GVTREGN")             #join the count of the number of hosueholds in each region 

###################################################################
###################### comapre regional dist ######################
###################################################################

### Family Resources Survey ###
#group the df by council tax band and region 
compreg_dist <- househol_region |>
    filter(CTBAND != -1, CTBAND != 10) |>
    group_by(CTBAND, GVTREGN) |>
    count()                                                                     #count the number of hh in each ct band and income band

compreg_dist <- compreg_dist |>
    left_join(count_reghh, by = 'GVTREGN')                                      #add the total number of hh in each region to the df

#calculate the percentage of househoulds in each council tax band by region
compreg_dist <- compreg_dist |>
    rename(no_hh = n.x, hh_region = n.y) |>
    mutate(frs_banddis = no_hh/hh_region) |>
    filter(!regions %in% c("SCOT", "WAL", "NI"))

#view(compreg_dist)

#### CTB ####
# group by region and band, tie together region and band
ctb_regval <- ctb_val |>
    select(region, name, value) |>                         #select region, ct band column (name) and number of hh (value)
    group_by(region, name) |>
    mutate(reg_band = str_c(region, name)) |>              #create an new colum of string that contain both the region and ct band
    ungroup()

#data is still at LA level, add together the households which match for region and ct band 
ctb_regval <- ctb_regval |>
    group_by(reg_band) |>
    summarise(total = sum(value))                           #gives the n.o. of hh in the ct band of every region

ctb_regionhh <- data.frame(do.call('rbind', strsplit(as.character(ctb_regval$reg_band), "band_", fixed=TRUE)))          #separate out band and region again
ctb_regval <- cbind(ctb_regval, ctb_regionhh)               #add separate band and region columns back to the data frame
ctb_regval <- ctb_regval |>
    select(!reg_band)                                       #remove the band and region merged column

#sum up the total numbers of households in the valuation list for each region
ctb_regtot <- ctb_regval |>
    group_by(X1) |>                                         #group by region (X1)
    summarise(reg_total = sum(total))

ctb_regtot <- ctb_regtot |>                                 #remove unecessary rows
    filter(!grepl("total", X1)) |>
    filter (!grepl("ENG", X1))

ctb_regdist <- left_join(ctb_regval, ctb_regtot, by = "X1") #join totals to the regional band totals 

#tidy ragional ctb datat frame and calculate ct band households distributtions by region
ctb_regdist <- ctb_regdist |>
    na.omit() |>
    mutate(ctb_banddis = total/reg_total*100) |>            #number of hosueholds in ct band by region over the total in each region 
    rename(region = X1, ct_band = X2) 

#change the FRS 1 to 8 bands to letter

compreg_dist2 <-compreg_dist                                #add the distribution of the frs to a new data frame 

compreg_dist2 <- as.character(compreg_dist2$CTBAND)         #rename the numbers in the frs the equivalent band letter
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

compreg_dist3 <- compreg_dist3 |>                           #tidy frs region hh dist data frame
    rename(ct_band = ...7) |>
    relocate(ct_band) 

compreg_dist3 <- compreg_dist3 |>
    select(ct_band:frs_banddis)|>                           #select relevent columns
    mutate(frs_banddis = frs_banddis*100)

############################################################################################
#function to compare the distribution of households in ct bands in different regions
############################################################################################

regional_distribution <- function(b, d, g, f) {
    a <- ctb_regdist |>
        filter(region == b)                                                     #select region from frs

    c <- compreg_dist3 |>
        filter(regions == d)                                                    #select the same region from ctb

    e <<- left_join(a, c, by = "ct_band")                                       #join the two dist

    e <<- e |>
        select(region, ct_band, regions, ctb_banddis, frs_banddis) |>           #select identifying columns and distributional columns
        mutate(dist_diff = ctb_banddis - frs_banddis)                              #take the ctb dist from the frs dist

    write.csv(e, paste0(g))                                                     #write to csv file 
    return(assign(f, e, envir = parent.frame()))                                #save data frame in the work space 
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
region_hhdist <- rbind(EE_hhdist, EM_hhdist, L_hhdist, NE_hhdist, NW_hhdist, SE_hhdist, SW_hhdist, WM_hhdist, YH_hhdist)            #collate all the regional hh dist comparisions

region_hhdist <- region_hhdist |>
    arrange(desc(abs(dist_diff)))                                       #arrage with the largest dist differences between the frs and ctb at the top
write.csv(region_hhdist, "data_output\\region_hhdist.csv")              #write regional dist comparisions to output folder

count_reghh <- count_reghh |>                                           #write the summary of the sample size for each region to the output file
    rename(no_hh = n)
write.csv(count_reghh, "data_output\\frs_reghh.csv")                     #write  sample size to output folder

##### checking all the data is present in each ctb and frs data frame
##checks 
count_frs <- compreg_dist |>
    group_by(CTBAND) |>
    count()

count_regdist <- ctb_regdist |>
    group_by(X2) |>
    count()
write.csv(compreg_dist, "data_output\\frs_regdist.csv")                  #write frs distribution to output folder
write.csv(ctb_regdist, "data_output\\ctb_regdist.csv")                   #write ctb distribution to output folder