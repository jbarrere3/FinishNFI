#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name _targets.R  
#' @description R script to launch the target pipeline
#' @author Julien BARRERE
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Options and packages ----------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load targets
library(targets)
# Load functions
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))
# install if needed and load packages
packages.in <- c("dplyr", "ggplot2", "tidyr", "data.table", "sf")
for(i in 1:length(packages.in)) if(!(packages.in[i] %in% rownames(installed.packages()))) install.packages(packages.in[i])
# Targets options
options(tidyverse.quiet = TRUE)
tar_option_set(packages = packages.in)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Targets workflow --------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(
  # File names
  tar_target(FinnishNFI_tree_raw_file, "data/fin_nfi_sample.csv", format = "file"), 
  tar_target(sgdd_file, "data/sgdd_nfi_sample.csv", format = "file"),
  tar_target(wai_file, "data/wai_nfi_sample.csv", format = "file"),
  tar_target(species_file, "data/species.csv", format = "file"),
  
  # Load raw data
  tar_target(FinnishNFI_tree_raw, fread(FinnishNFI_tree_raw_file)), 
  tar_target(sgdd, fread(sgdd_file)), 
  tar_target(wai, fread(wai_file)), 
  tar_target(species, fread(species_file)), 
  
  # Format the data
  tar_target(FUNDIV_tree_FI, 
             format_FinnishNFI_tree_to_FUNDIV(FinnishNFI_tree_raw, species)), 
  tar_target(FUNDIV_plot_FI, 
             format_FinnishNFI_plot_to_FUNDIV(FinnishNFI_tree_raw, FUNDIV_tree_FI, species)),
  tar_target(climate_FI, process_climate(sgdd_file, wai_file, FUNDIV_plot_FI)),
  
  # Save the formatted files
  tar_target(FUNDIV_tree_FI_file, write_on_disk(FUNDIV_tree_FI, "output/FinnishNFI_tree.csv"), 
             format = "file"),
  tar_target(FUNDIV_plot_FI_file, write_on_disk(FUNDIV_plot_FI, "output/FinnishNFI_plot.csv"), 
             format = "file"),
  
  # Exploratory plots
  tar_target(fig_disturbed_plots, 
             plot_disturbed_plots(FinnishNFI_tree_raw, "fig/fig_disturbed_plots.png"), 
             format = "file"), 
  tar_target(fig_severity_per_disturbance_noFilter, 
             plot_severity_per_disturbance(FinnishNFI_tree_raw, FUNDIV_tree_FI, filter.in = c("0", "1", "2", "3", "A", "B"),
                                           "fig/fig_severity_per_disturbance_noFilter.png"), 
             format = "file"), 
  tar_target(fig_severity_per_disturbance_smallFilter, 
             plot_severity_per_disturbance(FinnishNFI_tree_raw, FUNDIV_tree_FI, filter.in = c("1", "2", "3", "A", "B"),
                                           "fig/fig_severity_per_disturbance_smallFilter.png"), 
             format = "file"), 
  tar_target(fig_severity_per_disturbance_strongFilter, 
             plot_severity_per_disturbance(FinnishNFI_tree_raw, FUNDIV_tree_FI, filter.in = c("2", "3", "B"),
                                           "fig/fig_severity_per_disturbance_strongFilter.png"), 
             format = "file")
)