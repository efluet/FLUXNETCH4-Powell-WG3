# /----------------------------------------------------------------------------#
#/    summarize flux by wind sector (median flux, cumul measurements)
flux_bysector_cumul <-  comb_flux %>% 
    group_by(site, seccut) %>%
    summarise(freq = n(), 
              FCH4_F_median = median(FCH4_F, na.rm=T)) %>% 
    #ungroup() %>%
    mutate(FCH4_F_median_lin = (FCH4_F_median-min(FCH4_F_median))/(max(FCH4_F_median) - min(FCH4_F_median))) %>% 
    
    arrange(FCH4_F_median_lin) %>%   #sort by median flux
    mutate(FCH4_F_median_cumul=cumsum(FCH4_F_median_lin),
           FCH4_F_median_cumulnorm= FCH4_F_median_cumul/max(FCH4_F_median_cumul),
           freq_cumul = cumsum(freq),
           freq_cumulnorm = freq_cumul/max(freq_cumul))



# /----------------------------------------------------------------------------#
#/ Cumul plot  all towers stacked

ggplot(flux_bysector_cumul) +
    
    # 1:1 line
    geom_abline(aes(intercept = 0, slope = 1), color='grey90', size=1.4) +
    
    geom_point(aes(x=FCH4_F_median_lin, y=freq_cumulnorm, group=site, color=site)) +
    geom_line(aes(x=FCH4_F_median_lin, y=freq_cumulnorm, group=site, color=site)) +
    
    xlab("Median flux per wind sector\n(Scaled linearly for each site) ") + 
    ylab("Cumulative fraction of measurements") + 
    
    #scale_color_discrete(guide=FALSE) +
    scale_y_continuous(expand=c(0,0)) +  # limits = c(0, 1000),
    scale_x_continuous(expand=c(0,0)) +  # limits = c(0, 1000), 
    line_plot_theme +
    theme(legend.position = c(0.8,0.2))



ggsave("./output/cumulplot_bysite.png", width=100, height=100, units="mm")



# /----------------------------------------------------------------------------#
#/ Cumul plot - Points colored by WD

ggplot(flux_bysector_cumul) +
    
    # 1:1 line
    geom_abline(aes(intercept = 0, slope = 1), color='grey90', size=1.4) +
    
    # Line between points
    geom_line(aes(x=FCH4_F_median_lin, y=freq_cumulnorm, group=site), color='grey10') +
    
    # Background point, for outline
    geom_point(aes(x=FCH4_F_median_lin, y=freq_cumulnorm, group=site, shape=site),color="grey10", size=2.3) +
    
    # Colored points
    geom_point(aes(x=FCH4_F_median_lin, y=freq_cumulnorm, group=site, color=hsv(h=seccut/360, s=1, v=1), shape=site), size=2) +
    
    # Axis labels
    xlab("Median flux per wind sector\n(Scaled linearly for each site) ") + 
    ylab("Cumulative fraction of measurements") + 
    
    scale_color_identity(guide=FALSE) +  # This is needed for HSV colors to be accurate
    scale_shape_discrete(guide=FALSE) +
    
    scale_y_continuous(expand=c(0,0)) +  # limits = c(0, 1000),
    scale_x_continuous(expand=c(0,0)) +  # limits = c(0, 1000), 
    line_plot_theme


ggsave("./output/cumulplot_bysite_nwd.png", width=100, height=100, units="mm")


# /----------------------------------------------------------------------------#
#/    Make HSV color circle
#     Following this

ggplot() +
    geom_bar(aes(x=labs/360, y=1, fill=hsv(h=labs/360, s=1, v=1)), stat="identity") + 

    coord_polar(theta="x") +
    scale_fill_identity() +
    scale_y_continuous(breaks=NULL) +
    #scale_x_continuous(breaks=NULL) +
    scale_x_continuous(breaks = 0:11/12,#*360,
                       labels = c("N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW", "NNW"),
                       name = "") +

    xlab("") + ylab("") +
    line_plot_theme +
    theme(axis.line  = element_blank(),
          axis.text  = element_text(),
          axis.ticks = element_line())

ggsave("./output/color_circle.png", width=60, height=60, units="mm")


