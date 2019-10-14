

# /----------------------------------------------------------------------------#
#/ K-means Clustering                                                     ------

comb_flux_forclust<- comb_flux %>% 
                        filter(site=="USOWC") %>% 
                        filter(!is.na(FCH4)) %>% 
                        # normalize all variables to 0-1 before clustering
                        mutate(FCH4norm = (FCH4-min(FCH4))/(max(FCH4) - min(FCH4))) %>% 
                        mutate(WD_sin_ynorm = (WD_sin_y-min(WD_sin_y))/(max(WD_sin_y) - min(WD_sin_y))) %>%
                        mutate(WD_cos_xnorm = (WD_cos_x-min(WD_cos_x))/(max(WD_cos_x) - min(WD_cos_x))) %>% 
                        filter(FCH4 > 50)
    

k <- kmeans(comb_flux_forclust[,c('FCH4norm', 'WD_sin_ynorm', 'WD_cos_xnorm')], 
            centers=5, iter.max = 50, nstart = 100)

comb_flux_forclust$cluster <- as.factor(k$cluster)


comb_flux_forclust

# /----------------------------------------------------------------------------#
#/ Spectral Clustering                         ------

# library(kernlab)
# 
# 
# sc <- specc(x= as.matrix(comb_flux_forclust), centers=2)
# 
# 
# comb_flux$sc <- sc
# 
# t$sc <- sc
# 
# 
# ## Cluster the spirals data set.
# data(spirals)
# 
# sc <- specc(spirals, centers=2)
# sc
# centers(sc)
# size(sc)
# withinss(sc)
# 
# plot(spirals, col=sc)


# /------------------------------------------------
#/
library(dbscan)
db <- dbscan(comb_flux_forclust[,c('FCH4norm', 'WD_sin_ynorm', 'WD_cos_xnorm')],
             eps= 0.06,  # distance
             minPts= 60,  # number of minimum points in the eps region; often set to be dimensionality of the data plus one or higher. 
             weights = NULL, 
             borderPoints = TRUE)

db
comb_flux_forclust$cluster <- as.factor(db$cluster)



# /----------------------------------------------------------------------------#
#/   WINDROSE PLOT of clusters

ggplot() + 
    
    geom_point(data=subset(comb_flux_forclust, cluster == 0),
               aes(x= WD, y=FCH4norm), color="grey80", shape=21, stat="identity") +
    
    geom_point(data=subset(comb_flux_forclust, cluster != 0),
               aes(x= WD, y=FCH4norm, color=as.factor(cluster)), shape=21, stat="identity") +
    
    geom_hline(aes(yintercept= 0), color='black', size=0.3) +
    
    scale_x_continuous(breaks = 0:11/12*360,
                       labels = c("N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW", "NNW"),
                       name = "Wind Sectors") +
    scale_y_continuous(breaks=pretty_breaks()) +
    
    #facet_wrap(~site) +
    coord_polar(theta = "x", start=0)+ #-pi/12) + 
    scale_fill_brewer(palette = "Spectral", direction=-1)  +
    theme_minimal() +
    theme(legend.position = "none")



ggsave("./output/windrose_cluster.png", width=100, height=100, units="mm")





# #///////////////////////////////////////////////////////////////////
# 
# t <- as.matrix(comb_flux_forclust[,c('FCH4norm', 'WD_sin_ynorm', 'WD_cos_xnorm')])
# t<-dist(t)
# jpc <- jpclust(t, k=10, kt=2)
# jpc
# 
# unique(jpc$cluster)
# comb_flux_forclust$cluster <- as.factor(jpc$cluster)
# 
# sNNclust(x, k, eps, minPts, borderPoints = TRUE, ...)
