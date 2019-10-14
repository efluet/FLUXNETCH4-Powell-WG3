# this script combines the flux data from multiple towers

# Set directories
indir <- "./data/"
outdir <- "./scripts/"
plot.CH4.windrose = T

# set sector size
#labs = seq(15, 345, 30) # 30 deg
labs = seq(5, 355, 10)  # 10 deg


# /----------------------------------------------------------------------------#
#/   Loop combining sites                                              ---------

# create empty df
comb_flux <- data.frame()

# get site list
sites <- list.files(indir) #identify all files in input folder
site_names <- substr(sites, 1, 5)

# exclude the 2 sites withoud WD column` `
sites <- sites[!sites %in% c('RUSAM.csv','RUVrk.csv','USMRM.csv')]


# loop through sites
for (site.act in sites[1:1]) {
    
    
    site.name <- substr(site.act, 1, 5)
 
    #create output folder for current site
    outdir.act <- paste0(outdir, site.name, "/")
    
    # load dataset
    flux <- read.csv(paste0(indir, site.act), na.strings="NaN") %>%
        
        # Add name column
        mutate(site= site.name ) %>%
         
        # format date
        mutate(TIMESTAMP_END = ymd_hms(TIMESTAMP_END, truncated = 2),
               month         = month(TIMESTAMP_END)) %>% 
        
        # Select columns relevant to flux & WD
        dplyr::select(site, WD, FCH4, TIMESTAMP_END, month) %>% 
        
        # Filter missing direction and flux
        filter(!is.na(WD),
               !is.na(FCH4)) %>%
        
        #  Convert degrees to Sin/Cosin component of Rad circle
        mutate(WD_rad   = (3.14159 * WD/180),
               WD_sin_y = sin(3.14159 * WD/180),
               WD_cos_x = cos(3.14159 * WD/180)) %>% 
        
        # assign wind sector code
        mutate(seccut  = as.numeric(as.character(cut(WD, length(labs), labels=labs)))) 
    

    
    
    # Append each tower to cumulative table
    comb_flux <- bind_rows(comb_flux, flux)
    
    print(paste0("Processing site ", site.name, "  Dims: ", dim(flux)))
    
}


# /----------------------------------------------------------------------------#

#comb_flux<- comb_flux %>% 





# #/  By sector
# flux_bysector<- flux %>%      
#     group_by(seccut) %>% 
#     summarise(freq = n(), 
#               FCH4_F_median = median(FCH4_F, na.rm=T)) %>% 
#     ungroup() %>%
#     # normalize flux
#     mutate(FCH4_F_median_lin = 
#                (FCH4_F_median-min(FCH4_F_median))/(max(FCH4_F_median) - min(FCH4_F_median)),
#            site= site.name)
# 
# # Append each tower to cumulative table
# comb_flux_bysector <- bind_rows(comb_flux_bysector, flux_bysector)