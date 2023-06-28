#Emily Keenan
#28/06/23
#to calculate the percentage of hosueholds income speant on council tax by each income decile

not_eng <-  c("299999999.0", "399999999.0", "499999999.0")                                          #codes for scotland,wales and norther Ireland
dec <- c(1:10)                                                                                      #dummy vector for income deciles 
dec_index <- c(1:10)                                                                                #define the number of deciles

#### input data ####
ct_pay <- househol_raw |>
    select(SERNUM, GVTREGN, HHINC, CTANNUAL) |>                                                     #select region, household income and annual ct payments from the frs data
    filter(!GVTREGN %in% as.numeric(not_eng)) |>                                                    #filter out non-england regions
    filter(!CTANNUAL == -1, !CTANNUAL == -9, !HHINC <= 0)|>                                           #rm non inputs or errors
    arrange(HHINC)
nrow(househol_raw |>
    select(SERNUM, GVTREGN, HHINC, CTANNUAL) |>                                                     #select region, household income and annual ct payments from the frs data
    filter(!GVTREGN %in% as.numeric(not_eng)))

#### for loop to calculate decil bins ####
i <- 1
for (i in dec_index){
    dec[i] <- (i*(nrow(ct_pay) + 1))/10
    i <- i+1
}

dec <- round(dec, 0)                                                                                #round decile bin range to neares integer
dec[10] <- nrow(ct_pay)                                                                             #fix the 10th decile bin limit to the number of hosueholds

#### create vector of numbers to place the hosueholds in deciles
decile <- c(rep(1, dec[1]), rep(2, dec[2] - dec[1]), rep(3, dec[3] - dec[2]), rep(4, dec[4] - dec[3]), rep(5, dec[5] - dec[4]), 
            rep(6, dec[6] - dec[5]), rep(7, dec[7] - dec[6]), rep(8, dec[8] - dec[7]), rep(9, dec[9] - dec[8]), rep(10, dec[10] - dec[9]))
ct_pay <- cbind(ct_pay, decile)                                                                     #add the decile numbering to the hosueholds data frame

ct_pay <- ct_pay |>
    mutate(HHINC = HHINC*(365.2/7)) |>                                                                 #calculate the annual household income
    group_by(decile)                                                                                   #group by decil

ct_pay <- ct_pay |>
    mutate(percentage = CTANNUAL/HHINC)|>                                                               #calculate percentage of yearly household income annual CT is 
    
ct_pay_sum <- ct_pay |>
    summarise(HHINC = sum(HHINC), CTANNUAL = sum(CTANNUAL)) |>                                          #sum the households income and annual ct of each income decile
    mutate(percentage = CTANNUAL/HHINC*100)                                                             #multiplke by 100

    a <- ct_pay_sum %>% 
        ggplot(aes(x = decile , y = percentage)) +                                                      #input decile and percentage household data
        geom_bar(stat = "identity", fill = '#012169')  +                                                #specify a bar chart and colour
        scale_y_continuous(limits = c(0,15)) +                                                          #y scale limist
        scale_x_continuous(breaks = seq(1, 10, 1)) +                                                    #show the number of every decile on the x axis
        theme_minimal() +                    
        ylab('Percentage of households income spent on council tax (%)') + 
        xlab("Income deciles")                                                                          #add axis lables
    a <- a + theme(text = element_text(size = 20))                                                      #increase text size 
   
    ggplot2::ggsave('hhdist_plots//ct_inc_decile.png', a)                                               #save to folder
    ggplot2::ggsave('D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\ct_inc_decile.png', a)       #save to DAP transer folder


ct_pay_ave <- ct_pay |>                                 
    dplyr::summarise(average = median(percentage), na.rm=TRUE) |>                                       #coulculate the median percentage of hoseuholds income spent on ct
    mutate(average = average*100)                                                                       #multiple by 100 for the percentage



    b <- ct_pay_ave %>% 
        ggplot(aes(x = decile , y = average)) +                                                         #input decile and percentage household data
        geom_bar(stat = "identity", fill = '#012169')  +                                                #specify a bar chart and colour
        scale_y_continuous(limits = c(0,15)) +                                                          #y sacle limit
        scale_x_continuous(breaks = seq(1, 10, 1)) +
        theme_minimal() +                    
        ylab('Percentage of households income spent on council tax (%)') + 
        xlab("Income deciles")                                         
    b <- b + theme(text = element_text(size = 20))               
    ggplot2::ggsave('hhdist_plots//median_ct_incdec.png', b)
    ggplot2::ggsave('D:\\Users\\emily.keenan\\OneDrive - MHCLG\\Desktop\\DAP Transfer\\median_ct_incdec.png', b)

############### QA #################
view(tail(ct_pay))
count_decil <- count(decile)
decile
dec[2]-dec[1]

decile_factor <- as.factor(decile) 
str(decile_factor)
count(decile_factor)
view(ct_pay)
view(ct_pay_sum)
rm(dec)
rm(dec_index)
view(in_perw)

ct_pay_check <- ct_pay |>
    filter(CTANNUAL <=0)
