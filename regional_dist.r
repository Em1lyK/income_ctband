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
    ggsave(paste0(reg,"_hhdist.png"), d)
    return(d)

}

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


region_dist(househol_region, househol_nw, nw, plot_nw, "NW")
region_dist(househol_region, househol_yh, yh, plot_yh, "YH")
region_dist(househol_region, househol_em, em, plot_em, "EM")
region_dist(househol_region, househol_wm, wm, plot_wm, "WM")
region_dist(househol_region, househol_ee, ee, plot_ee, "EE")
region_dist(househol_region, househol_l, l, plot_l, "L")
region_dist(househol_region, househol_se, se, plot_se, "SE")
region_dist(househol_region, househol_sw, sw, plot_sw, "SW")




