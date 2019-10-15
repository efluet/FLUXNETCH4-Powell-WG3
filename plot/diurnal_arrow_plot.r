

diurnal_arrow_plot = 
    
    ggplot(flux_d) + 
    
    # Add background points
    stat_density_2d(data=flux,
                    aes(x=WD, y=FCH4, fill = stat(level)), alpha=0.5, geom = "polygon") +
    
    # Add arrows
    geom_segment(aes(y = 0, 
                     x = WD_deg_monthmean, 
                     xend = WD_deg_monthmean, 
                     yend = FCH4_monthmean, 
                     group=diurnal_phase,
                     size=1-WD_rad_monthrho,
                     color=diurnal_phase),
                 arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends='last')) +
    
    
    # Add arrow labels
    geom_text(aes(x = WD_deg_monthmean, 
                  y = FCH4_monthmean * 1.5, 
                  label= diurnal_phase, 
                  color=diurnal_phase),
              size=3) +
    
    # make origin white point for cleaner plot
    geom_point(aes(x=0, y=0), size=4, shape=21, fill='white', color='grey90', stroke=1.2) +
    
    # facet_wrap(.~site) +
    coord_polar(theta = "x") +
    
    # Labels
    xlab("") + ylab( expression ("FCH4 ("~nmol~m^-2 ~s^-1~")")) + 
    ggtitle(label=flux_d$site[1]) +
    
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    
    #scale_size_identity() + 
    scale_size_continuous(range=c(0.1, 2.5)) + 
    scale_fill_distiller(palette = "Greys", direction=1)  +
    scale_color_brewer(palette = "Set1", direction=-1)  +
    scale_x_continuous(limits=c(0,360),
                       breaks = 0:11/12*360,
                       labels = c("N", "", "", "E", "", "", "S", "", "", "W", "", ""),
                       name = "Wind direction (WD)")  +
    scale_y_continuous(limits=c(min(flux$FCH4),  quantile(flux$FCH4, .98)), breaks=base_breaks(), trans='log10') +
    
    guides(color=FALSE, fill=FALSE, size=FALSE)
