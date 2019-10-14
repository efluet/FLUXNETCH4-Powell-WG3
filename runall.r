# /----------------------------------------------------------------------------#
#/  Powell WG3 
#   Calculate a CH4 flux normalized to temperature & NEE, 
#   so that all the variation left is from spatial coverage.

#   Authors: Mathias Goedecke & Etienne Fluet-Chouinard, 2019
#   Institutions: Max Plank Inst. Jena;  Stanford University

#   Make stacked cumulative plots for all towers.
#   Run clustering on flux & direction. See how mny clusters is optimal.
#   Make wind rose of the frequency of each' stacked barplot..

#   Questions: 
#    - Is flux direction at each tower (non-)uniformly distributed?
#    - Is flux intensity related to wind direction? Across seasons or diurnal cycle?



# /----------------------------------------------------------------------------#
#/   Import libraries & themes
library(here)
here()

source("prep/import_libraries.r") 
source("plot/line_plot_theme.r") 


# /----------------------------------------------------------------------------#
#/ Read flux data from each site and bind into one df.
source("prep/compile_tower_flux.r")


# /----------------------------------------------------------------------------#
#/  First generation plots


# Calculate cumulative freq across wind sectors, and plot it
source("plot/cumulative_sector_plots.r")

# Make windrose plots: by sectors & with point density
source("plot/windrose_plot.r") 

# Cluster fluxes  & plot on windrose
source("proc/wd_flux_clustering.r") 


# /----------------------------------------------------------------------------#
#/  Circular statistics


# Linear-circular correlation of fluxes and wind direction (as polar coordiantes)
source("proc/wd_correlation.r") 

# TODO: Some ARMA time-series applied to the lin-circ model. 


# Average flux & WF per month, diurnal phase.
source("proc/temporal_avg_dir.r") 


