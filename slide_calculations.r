#07/06/23
#Emily Keenan
#file to calculat numerbs to go in the slide backs with the graph

setwd("D:\\Users\\emily.keenan\\Documents\\GitHub\\income_ctband2")         #define working directory

source('frs_ukdata.r', chdir = TRUE)                                        #source file that reads in frs and calculated England distribution
source('regional_dist.r')                                                   #source files that calculates regional distribution
 source('househol_composition.r')                                            #source file that plots graphs of household composition


###################################################################################################
####Fuction to calculate the numebr of hh in a area than ####
###################################################################################################
less_x_pw <- function(input, band, x_pw ) {
a <- input |>
    filter(CTBAND == band)                                                     #filter for band D
less_x <- sum(a$n[1:x_pw])                                                #select band D hh that have less than 800 pw
per_lessx <- less_x/a$total[1]  
return(per_lessx)                                         #divide n.o. of hh w/ less 800 pw by the total n.o. of band D hh 
}


####Calcualte the percentage of band D hh at ENG level that have less than 800 per week####
viw(t)                                                  #eng level data frame on hh inc distribution
less_x_pw(t, 4, 4)                                      #call function to calcualtes percentage of band D hosueholds at ENG level that have less than 800 per week


####Calcualte the percentage of band D hh in the NW level that have less than 800 per week####
view(househol_nw)
less_x_pw(househol_nw, 4, 4)  
less_x_pw(househol_nw, 4, 3)                            #band D less 600
less_x_pw(househol_nw, 5, 3)                            #band E less 600


####Calcualte the percentage of band D hh in the WM level that have less than 800 per week####
view(househol_wm)
less_x_pw(househol_wm, 4, 4)  


####Calcualte the percentage of band D hh in the sw level that have less than 800 per week####
view(househol_sw)
less_x_pw(househol_sw, 4, 4)  


####Calcualte the percentage of band D hh in the l level that have less than 800 per week####
view(househol_l)
less_x_pw(househol_l, 4, 4)  
less_x_pw(househol_l, 4, 3)                            #band D less 600
less_x_pw(househol_l, 5, 3)                            #band E less 600


###################################################################################################
####Estimate the number of hosueholds in band D on and income of 400 and less that 600 pw####
###################################################################################################
waldInterval <- function(x, n, conf.level = 0.95, interval){
 p <- x/n
 sd <- sqrt(p*((1-p)/n))
 z <- qnorm(c( (1 - conf.level)/2, 1 - (1-conf.level)/2)) #returns the value of thresholds at which conf.level has to be cut at. for 95% CI, this is -1.96 and +1.96
 ci <- p + z*sd
 return(assign(paste0(interval), ci, envir = parent.frame()))
 }
waldInterval(x = t$n[37], n =t$total[37], conf.level = 0.95, 'b_d_400_600')               #call function to calculate confidence interval, x= n.o. of band D earning 400 to 600 with confidence of 95%

eng_b_d <- ctb_engval$value[4]
est_hh_d <- eng_b_d * b_d_400_600

view(ctb_engval)

    a <- ctb_engval %>% 
        ggplot(aes(x = as.character(name) , y = dis)) +              #input data fro hh percentage and income band
        geom_bar(stat = "identity", fill = paste0('#012169'))  +                                        #specify a bar chart
        scale_y_continuous(limits = c(0,30)) +     
        theme_minimal() +                    
        ylab('Percentage of hosueholds (%)') + 
        xlab("Council tax bands")                                           #add axis lables
    a <- a + theme(text = element_text(size = 20))                  #increase text size 
   
    ggplot2::ggsave(paste0("hhdist_plots\\hh_ctband.png"), a)
    ggplot2::ggsave(paste0("D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\hh_ctband.png"), a)
