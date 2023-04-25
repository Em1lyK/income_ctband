#calculate income distributions for similar regions

ne_nw <- c('NE', 'NW')
em_yh <- c('EM', 'YH')
se_e <- c('SE', 'E')
sw_wm <- c('SW', 'WM')

##north east north west

#make simultaneous equation matricies 
parhh_n_df <- estimate_hh %>% 
  filter(region %in% ne_nw) %>% 
  select(a_c, d_h)

parhh_n_mat <- data.matrix(parhh_n_df)

parin_n_df<- estimate_hh %>% 
  filter(region %in% ne_nw) %>% 
  select(hh_less, hh_more)

parin_n_mat <- data.matrix(parin_n_df)

hh_dist_nenw <- solve(parhh_n_mat, parin_n_mat)

##east midlands yorkshire and the humber

hh_emyh_df <- estimate_hh %>% 
  filter(region %in% em_yh) %>% 
  select(a_c, d_h)

hh_emyh_mat <- data.matrix(hh_emyh_df)

emyh_in_df <- estimate_hh %>% 
  filter(region %in% em_yh) %>% 
  select(hh_less, hh_more)

emyh_in_mat <- data.matrix(emyh_in_df)

emyh_dist <- solve(hh_emyh_mat, emyh_in_mat)



##south east east

see_hh_df <- estimate_hh %>% 
  filter(region %in% se_e) %>% 
  select(a_c, d_h)

see_hh_mat <- data.matrix(see_hh_df)

see_in_df <- estimate_hh %>% 
  filter(region %in% se_e) %>% 
  select(hh_less, hh_more)

see_in_mat <- data.matrix(see_in_df)

see_dist <- solve(see_hh_mat, see_in_mat)


##south west west midlands


swwm_hh_df <- estimate_hh %>% 
  filter(region %in% sw_wm) %>% 
  select(a_c, d_h)

swwm_hh_mat <- data.matrix(swwm_hh_df)

swwm_in_df <- estimate_hh %>%
  filter(region %in% sw_wm) %>% 
  select(hh_less, hh_more)

swwm_in_mat <- data.matrix(swwm_in_df)

swwm_dist <- solve(swwm_hh_mat, swwm_in_mat)  




