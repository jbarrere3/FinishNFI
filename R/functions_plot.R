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
    mutate(stand_level_dist_agent = ifelse(is.na(stand_level_dist_agent), "D2", stand_level_dist_agent), 
           disturbance.type = substr(stand_level_dist_agent, 1, 1), 
           disturbance = case_when(disturbance.type == "A" ~ "Abiotic", 
                                   disturbance.type == "B" ~ "Animals", 
                                   disturbance.type == "C" ~ "Fungi",
                                   TRUE ~ "Other")) %>%
    group_by(disturbance, stand_level_dist_agent) %>%
    summarize(n = n()) %>%
    ggplot(aes(x = stand_level_dist_agent, y = n)) + 
    geom_bar(stat = "identity", colour = "black") + 
    facet_wrap(~ disturbance, scales = "free_y", nrow = 1) + 
    theme_bw() + 
    ylab("Number of plots impacted")
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 25, height = 10, units = "cm", dpi = 600)
  return(file.in)
  
}