#distribution of icnome within council tax bands by region 

region_dist <- function(a, b, c, d, reg) {
    b <- a |>
    filter(GVTREGN == c)

    b <- b %>% 
    group_by(CTBAND,HHINCBND) %>%
    count() 

    b <- b |>
    group_by(CTBAND) |>
    mutate(total = sum(n)) |>
    mutate(percent = n/total)

    d <- b %>% 
    filter(CTBAND != -1) %>%
    ggplot(aes(x = HHINCBND, y = percent)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(limit = income_levels) +
    theme(axis.text.x = element_text(angle = 90), 
    axis.ticks = element_blank())+
    facet_wrap(~CTBAND, labeller = labeller(CTBAND = ctband_levels))
    ggsave(paste0("hhdist_plots\\", reg,"_hhdist.png"), d)
   

}

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

region_dist(househol_region, househol_ne, ne, plot_ne, "NE")
region_dist(househol_region, househol_nw, nw, plot_nw, "NW")
region_dist(househol_region, househol_yh, yh, plot_yh, "YH")
region_dist(househol_region, househol_em, em, plot_em, "EM")
region_dist(househol_region, househol_wm, wm, plot_wm, "WM")
region_dist(househol_region, househol_ee, ee, plot_ee, "EE")
region_dist(househol_region, househol_l, l, plot_l, "L")
region_dist(househol_region, househol_se, se, plot_se, "SE")
region_dist(househol_region, househol_sw, sw, plot_sw, "SW")


#########################################################################
##### check the number of hosueholds in each region #####################
########################################################################

count_reghh <- househol_region |>
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
    mutate(frs_banddis = no_hh/hh_region)

#view(compreg_dist)

### CTB

ctb_regval <- ctb_val |>
    select(region, name, value) |>
    group_by(region, name) |>
    mutate(reg_band = str_c(region, name)) |>
    ungroup()

ctb_regval <- ctb_regval |>
    group_by(reg_band) |>
    summarise(total = sum(value))

ctb_regionhh <- data.frame(do.call('rbind', strsplit(as.character(ctb_regval$reg_band), "band_", fixed=TRUE)))
ctb_regval <- cbind(ctb_regval, ctb_regionhh)

ctb_regtot <- ctb_regval |>
    group_by(X1) |>
    summarise(reg_total = sum(total))

view(ctb_regtot)
view(ctb_regionhh)
view(ctb_regval)
view(ctb_val)
