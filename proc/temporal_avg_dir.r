# Average flux across different temporal scales:
#   - monthly,
#   - diurnal
#   - monthly x diurnal

# TODO: run density on cos,sin values instead of WD

# /----------------------------------------------------------------------------#
#/   Function that makes pretty log10 axis breaks                   ------------
#    Source: https://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual
base_breaks <- function(n = 10){
    function(x) { axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n) } }



# /----------------------------------------------------------------------------#
#/  Calculate monthly means 
# The resulting radius will be 1 if all angles are equal. 
# If the angles are uniformly distributed on the circle, then the resulting radius will be 0, and there is no circular mean.


flux_m <- flux %>% 
    group_by(site, month, month_abb) %>% 
    summarise(WD_rad_monthmean = circ.summary(WD_rad)[[2]],  # Get mean direction
              WD_rad_monthrho = circ.summary(WD_rad)[[3]],   # Get mean rho
              FCH4_monthmean = mean(FCH4)) %>%
    ungroup() %>% 
    mutate(WD_deg_monthmean = rad2deg(WD_rad_monthmean)) %>% 
    mutate(WD_deg_monthmean = ifelse(WD_deg_monthmean < 0, 360+WD_deg_monthmean, WD_deg_monthmean)) %>% 
    filter(FCH4_monthmean > 0)



# /----------------------------------------------------------------------------#
#/  Windrose plot  - monthly arrows w/ length                    ---------
# this plot cannot represent negative y-values (methane sinks)


if(nrow(flux_m) > 0){ source('./plot/monthly_arrow_plot.r') }



# /----------------------------------------------------------------------------#
#/   Calculate Diurnal averages                                          -------

flux_d <- flux %>% 
    
    mutate(hour = hour(TIMESTAMP_END),
           diurnal_phase = ifelse(hour > 6 & hour <= 18, "Day", "Night")) %>% 
    group_by(site, diurnal_phase) %>% 
    summarise(WD_rad_monthmean = circ.summary(WD_rad)[[2]],
              WD_rad_monthrho = circ.summary(WD_rad)[[3]],
              FCH4_monthmean = mean(FCH4)) %>%
    ungroup() %>% 
    mutate(WD_deg_monthmean = rad2deg(WD_rad_monthmean)) %>% 
    mutate(WD_deg_monthmean = ifelse(WD_deg_monthmean < 0, 360+WD_deg_monthmean, WD_deg_monthmean)) %>% 
    filter(FCH4_monthmean > 0)


# /----------------------------------------------------------------------------#
#/  Diurnal Arrow plot  w/ length                                      ---------
# this plot cannot represent negative y-values (methane sinks)



if(nrow(flux_d) > 0){ source('./plot/diurnal_arrow_plot.r') }



# /----------------------------------------------------------------------------#
#/   Arrange plots in one panel                                         --------
c <- arrangeGrob(month_arrow_plot,
                  diurnal_arrow_plot,
                  nrow=1)

# /----------------------------------------------------------------------------#
#/  Save panel of plots                                                 --------
ggsave(paste0("../output/figures/polar_arrows/polar_arrows_", site.name," .png"),
       c,
       width=187, height=110, units="mm")
