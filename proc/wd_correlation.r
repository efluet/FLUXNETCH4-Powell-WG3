

lin_circ_reg <- function(sitename){

    c<- comb_flux %>% 
        filter(site==sitename) %>%  
        filter(!is.na(FCH4)) %>% 
        mutate(WDrad = deg2rad(WD))

    # calculate R2 and p-value of circ-lin correlation
    o <- data.frame(sqrt(circlin.cor(c$WDrad, c$FCH4)))

    o$site <- sitename

    return(o)
}

# map  function over list
df <- map_dfr(as.list(site_names), ~lin_circ_reg(.))



# plot the Rsquare
ggplot(df) +
    geom_bar(aes(x=reorder(site, R.squared), y=R.squared), width=0.75, stat='identity', fill='lightblue') +
    geom_point(data=subset(df, p.value<0.05), aes(x=site, y=R.squared+0.01), size=0.3, stat='identity') + 
    coord_flip() + 
    line_plot_theme +
    scale_y_continuous(expand=c(0,0)) +
    xlab('') + ylab('Lin-circ R squared')
    

ggsave("./output/lincirc_r2_barplot.png", width=100, height=150, units="mm")










# TODO: Test if linear variable X is normally distributed
# Basic properties:
# The sample multiple correlation for ciruclar-linear association is a function of the sample pearson’s correlation between cos(theta) and sin(theta), correlation between data vector and cos(theta), and correlation between data vector and sin(theta). So when rcs
# is small, R2x\tehta is large when either r2xc or r2xs is large. When rcs is large, R2xθ approaches zero.

# In circiular-linear correlation, the linear variable is implied to follow a sinusoidal pattern. This assumption is quite limited, and may explain why many of the genes detected significant in circ-linear case are not signficant in cir-cir case.
# Alternatively: Basic Cosine Regression Model
# Source: Pewsey, A., Ruxton, G. D., & Neuhäuser, M. (2013). Circular Statistics in R (Vol. First edition). Oxford, United Kingdom: OUP Oxford.

##############################################################################
# Randomisation test for circular-linear independence
# Example: ozone data
##############################################################################


# lvar: linear variable
# cvar: circular vriable, in radians
R2xtCorrCoeff <- function(lvar, cvar) {
    
    rxc <- cor(lvar, cos(cvar))
    rxs <- cor(lvar, sin(cvar))
    
    rcs <- cor(cos(cvar), sin(cvar))
    
    R2xtVal <- ((rxc*rxc)+(rxs*rxs)-(2*rxc*rxs*rcs))/(1-rcs*rcs)
    
    return(R2xtVal)
    
}



R2xtIndTestRand <- function(lvar, cvar, NR) {
    
    R2xtObs <- R2xtCorrCoeff(lvar, cvar) ; nxtrm <- 1
    
    for (r in 1:NR) {
        
        lvarRand <- sample(lvar)
        
        R2xtRand <- R2xtCorrCoeff(lvarRand,cvar)
        
        if (R2xtRand >= R2xtObs) {nxtrm <- nxtrm+1} }
    
    pval <- nxtrm/(NR+1)
    
    
    return(c(R2xtObs, pval)) # returns the R2, and p-value
    
}



ozone <- c(28.0,85.2,80.5,4.7,45.9,12.7,72.5,56.6,31.5,112.0,20.0,72.5,16.0,45.9,32.6,56.6,52.6,91.8,55.2)

winddeg <- c(327,91,88,305,344,270,67,21,281,8,204,86,333,18,57,6,11,27,84)

windrad <- winddeg*2*pi/360

R2xtIndTestRand(ozone, windrad, 9999)





# fit model
# we recommend testing the null hypothesis of independence using a randomization test in which R 2 x θ is the test statisti
# lmfit <- lm(data=c, formula= FCH4 ~ WD_cos_x + WD_sin_y)

# return a tibble of model fit
#return(glance(lmfit))