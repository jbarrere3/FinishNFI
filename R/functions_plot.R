#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_plot.R  
#' @description R script containing all functions relative to data
#               visualisation
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#' Plot number of plots by disturbance type
#' @param FinnishNFI_tree_raw Finnish NFI raw tree table
plot_disturbed_plots <- function(FinnishNFI_tree_raw, file.in){
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- FinnishNFI_tree_raw %>%
    dplyr::select(plotcode, stand_level_dist_agent) %>%
    distinct() %>%
    mutate(disturbance.type = ifelse(is.na(stand_level_dist_agent), "D2", 
                                     substr(stand_level_dist_agent, 1, 1)), 
           disturbance = case_when(disturbance == "A" ~ "Abiotic", 
                                   disturbance == "B" ~ "Animals", 
                                   disturbance == "C" ~ "Fungi",
                                   disturbance == "D" ~ "Other")) %>%
    group_by(disturbance, disturbance.type) %>%
    summarize(n = n()) %>%
    ggplot(aes(x = disturbance.type, y = n)) + 
    geom_bar(stat = "identity", colour = "black") + 
    facet_wrap(~ disturbance, scales = "free") + 
    coord_flip() + 
    theme_bw() + 
    ylab("Number of plots impacted")
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 18, height = 16, units = "cm", dpi = 600)
  return(file.in)
  
}