# /----------------------------------------------------------------------------#
#/ Calculate a CH4 flux normalized to temperature & NEE, so that all the variation left is from spatial coverage.
#  Authors: Mathias Goedecke & Etienne Fluet-Chouinard, 2019

# Make stacked cumulative plots for all towers.
# Run clustering on flux & direction. See how mny clusters is optimal.
# Make wind rose of the frequency of each' stacked barplot..

# IS THERE AN ASSOCIATION BETWEEN 
# IS THERE AN ASSOCIATION BETWEEN SEASONALITY AND FLUX-DIRECTION?
# Calculate cumulative flux per direction? 

library(here)
here()

source("scripts/import_libraries.r") 
source("scripts/line_plot_theme.r") 


# Read and bind files from each site
source("scripts/compile_tower_flux.r")

# Calculate cumulative freq across wind sectors, and plot it
source("scripts/cumulative_sector_plots.r")

# Make windrose plots: by sectors & with point density
source("scripts/windrose_plot.r") 

# Cluster fluxes  & plot on windrose
source("scripts/wd_flux_clustering.r") 


# Linear-circular correlation of fluxes and wind direction (as polar coordiantes)
# TODO: Some ARMA time-series applied to the lin-circ model. 
source("scripts/wd_correlation.r") 


source("scripts/temporal_avg_dir.r") 


