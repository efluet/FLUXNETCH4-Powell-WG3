# Average flux across different temporal scales:
#   - monthly,
#   - diurnal
#   - monthly x diurnal


library(scales)
library(REdaS)

# Calculate mean of angles 
# The resulting radius will be 1 if all angles are equal. If the angles are uniformly distributed on the circle, then the resulting radius will be 0, and there is no circular mean.


comb_flux_m <- comb_flux %>% 
          group_by(site, month) %>% 
          #do(summarise(circ.summary(.$WD_rad))) %>%
          summarise(WD_rad_monthmean = circ.summary(WD_rad)[[2]],
                    WD_rad_monthrho = circ.summary(WD_rad)[[3]],
                    FCH4_monthmean = mean(FCH4)) %>%
          ungroup() %>% 
          mutate(WD_deg_monthmean = rad2deg(WD_rad_monthmean)) %>% 
          mutate(WD_deg_monthmean = ifelse(WD_deg_monthmean < 0, 360+WD_deg_monthmean, WD_deg_monthmean)) %>% 
          mutate(month_abb = month.abb[month]) %>% 
          filter(FCH4_monthmean > 0)



# /----------------------------------------------------------------------------#
#/  Windrose plot  - monthly arrows w/ length                    ---------
# this plot cannot represent negative y-values (methane sinks)

ggplot(comb_flux_m) + 
    
    geom_segment(aes(y = 0, 
                     x = WD_deg_monthmean, 
                     xend = WD_deg_monthmean, 
                     yend = WD_rad_monthrho, 
                     group=month,
                     size=FCH4_monthmean ,
                     color=month_abb),
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
    
    
    geom_text(aes(x = WD_deg_monthmean, 
                  y = WD_rad_monthrho + .2 , 
                  label= month_abb, 
                  color=month_abb),
              size=3) +

    # make origin white point for cleaner plot
    geom_point(aes(x=0, y=0), size=4, shape=21, fill='white', color='grey90') +
    
    # facet_wrap(.~site) +
    coord_polar(theta = "x") +
    
    # mean resultant length, ρ, is between 0-1 and informs 
    # on the spread of circular variable.
    ylab('Rho (dispersion of direction') + 
    xlab("")+
    
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    
    scale_size_continuous(range=c(0.01, 4)) + 
    scale_color_brewer(palette = "Spectral", direction=-1)  +
    scale_x_continuous(limits=c(0,360),
                       breaks = 0:11/12*360,
                       labels = c("N", "", "", "E", "", "", "S", "", "", "W", "", ""),
                       name = "Wind Sectors")  +
    scale_y_continuous(breaks=pretty_breaks()) +
    
    guides(color=FALSE)


# save plot
ggsave("./output/polar_monthly_arrow.png", width=160, height=100, units="mm")



