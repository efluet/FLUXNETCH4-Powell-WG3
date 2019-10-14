

# /----------------------------------------------------------------------------#
#/   GGridges plot    -----
library(ggridges)

ggplot(comb_flux_bysector)+ 
    #aes(x=FCH4_F_median_lin, y=sites, height = freq, group = sites)) + 
    #geom_ridgeline(fill = "lightblue")
    geom_density_ridges(aes(x=seccut, y=freq, height=freq))


ggplot(comb_flux_bysector, 
       aes(x=seccut, y= site, height = freq, group=site)) + 
    geom_ridgeline(fill = "lightblue")


# /----------------------------------------------------------------------------#
#/  Prepare flux data              ------
quants <- seq(min(flux$FCH4, na.rm = T), 
              max(flux$FCH4, na.rm = T), 
              by = 500)


flux$FCH4_F_MOD  <- cut(flux$FCH4, breaks=length(quants), labels=quants)


flux_wd_ch4 <- flux %>%
    #mutate(WD = ifelse(WD>345, WD+15, WD)) %>%
    filter(!is.na(WD)) %>%   #  & WD.mod>=360)
    mutate(seccut  = as.numeric(as.character(cut(WD, 12, labels=labs))),
           fluxcut = cut(FCH4_F, 4)) %>% 
    group_by(seccut, fluxcut) %>%
    summarise(n=n(),
              FCH4_F_median = median(FCH4_F)) %>% 
    filter(!is.na(fluxcut)) %>% 
    ungroup()



flux_bysector <- flux_wd_ch4 %>% group_by(seccut) %>% summarise(n=sum(n))


# /----------------------------------------------------------------------------#
#/  Windrose plot  - by sectors                         ----------
ggplot() + 
    
    geom_bar(data= flux_wd_ch4, aes(x = seccut, y=n, fill = rev(fluxcut)), 
             stat="identity", width = 30.1,  position="stack") + 
    
    geom_bar(data= flux_bysector, aes(x = seccut, y=n),
             fill=NA, color="black", size=0.2, width = 30.1,
             stat="identity", position="stack") + 
    
    
    
    scale_x_continuous(breaks = 0:11/12*360,
                       labels = c("N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW", "NNW"),
                       name = "Wind Sectors") +
    
    coord_polar(theta = "x", start=0)+ #-pi/12) + 
    scale_fill_brewer(palette = "Spectral", direction=-1)  +
    theme_minimal()



# /----------------------------------------------------------------------------#
#/  Windrose plot  - scatter points              -------------

library(scales)

ggplot(comb_flux) + 
    
    geom_point(aes(x= WD, y=FCH4), shape=21, stat="identity") +
    
    geom_hline(aes(yintercept= 0), color='red') +
    
    scale_x_continuous(breaks = 0:11/12*360,
                       labels = c("N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW", "NNW"),
                       name = "Wind Sectors") +
    scale_y_continuous(breaks=pretty_breaks()) +
    
    facet_wrap(.~site, scales="free_y") +
    coord_polar(theta = "x", start=0)+ #-pi/12) + 
    scale_fill_brewer(palette = "Spectral", direction=-1)  +
    theme_minimal()



