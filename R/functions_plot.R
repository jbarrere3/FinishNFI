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
#' @param file.in Path to the file where to save the plot
plot_disturbed_plots <- function(FinnishNFI_tree_raw, file.in){
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- FinnishNFI_tree_raw %>%
    dplyr::select(plotcode, stand_level_dist_agent) %>%
    distinct() %>%
    mutate(stand_level_dist_agent = ifelse(is.na(stand_level_dist_agent), "0", stand_level_dist_agent), 
           disturbance.type = substr(stand_level_dist_agent, 1, 1), 
           disturbance = case_when(disturbance.type == "A" ~ "Abiotic", 
                                   disturbance.type == "B" ~ "Animals", 
                                   disturbance.type == "C" ~ "Fungi",
                                   TRUE ~ "Other")) %>%
    group_by(disturbance, stand_level_dist_agent) %>%
    summarize(n = n()) %>%
    ggplot(aes(x = stand_level_dist_agent, y = n)) + 
    geom_bar(stat = "identity", colour = "black") + 
    facet_wrap(~ disturbance, scales = "free_x", nrow = 1) + 
    theme_bw() + 
    ylab("Number of plots impacted") + 
    xlab("Disturbance agent")
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 25, height = 10, units = "cm", dpi = 600)
  return(file.in)
  
}


#' Plot number of plots by disturbance type
#' @param FinnishNFI_tree_raw Finnish NFI raw tree table
#' @param FUNDIV_tree_FI Finnish NFI tree table formatted for FUNDIV
#' @param file.in Path to the file where to save the plot
plot_severity_per_disturbance <- function(FinnishNFI_tree_raw, FUNDIV_tree_FI, file.in){
  
  ## - Create directories if needed
  create_dir_if_needed(file.in)
  
  ## - make the plot
  plot.out <- FinnishNFI_tree_raw %>%
    dplyr::select(plotcode, stand_level_dist_agent, stand_level_dist_time_since) %>%
    distinct() %>%
    filter(stand_level_dist_time_since != 4) %>%
    mutate(plotcode = as.character(plotcode), 
           disturbance.type = case_when(stand_level_dist_agent == "A1" ~ "storm", 
                                        stand_level_dist_agent == "A5" ~ "fire", 
                                        stand_level_dist_agent == "A2" ~ "snow", 
                                        substr(stand_level_dist_agent, 1, 1) == "B" ~ "other.animal", 
                                        substr(stand_level_dist_agent, 1, 1) == "C" ~ "other.fungi", 
                                        TRUE ~ "none")) %>%
    group_by(disturbance.type) %>%
    mutate(disturbance = paste0(disturbance.type, " (n = ", n(), ")")) %>%
    left_join((FUNDIV_tree_FI %>%
                 filter(treestatus != 1) %>%
                 mutate(dead = ifelse(treestatus %in% c(3:5), 1, 0)) %>%
                 group_by(plotcode) %>%
                 summarize(prop.dead = round(sum(dead)/n(), digits = 2))), 
              by = "plotcode") %>%
    ggplot(aes(x = prop.dead)) + 
    geom_histogram() + 
    facet_wrap(~ disturbance, scales = "free_y", nrow = 1) + 
    theme_bw() + 
    xlab("Proportion of trees killed")
  
  
  ## - save the plot
  ggsave(file.in, plot.out, width = 22, height = 15, units = "cm", dpi = 600)
  return(file.in)
  
}