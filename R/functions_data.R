#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### ----------------- SCRIPT INTRODUCTION ------------------ ####
#
#' @name functions_data.R  
#' @description R script containing all functions relative to data
#               importation and formatting
#' @author Julien Barrere
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#' Format Finnish NFI tree table to FUNDIV template
#' @param FinnishNFI_tree_raw Finnish NFI raw tree table
#' @param species table to convert species code to Latin name
format_FinnishNFI_tree_to_FUNDIV <- function(FinnishNFI_tree_raw, species){
  FinnishNFI_tree_raw %>%
    rename(Code = species, dbh1 = dbh) %>%
    mutate(plotcode = as.character(plotcode), 
           treecode = paste(plotcode, treecode, sep = "_"), 
           tree_type_1st_census = as.character(tree_type_1st_census), 
           tree_type_2nd_census = as.character(tree_type_2nd_census), 
           treestatus = case_when(tree_type_2nd_census %in% c("0", "1", "3", "7") ~ 2, 
                                  tree_type_2nd_census %in% c("A", "B", "D") ~ 4, 
                                  tree_type_2nd_census %in% c("E", "F", "G") ~ 3), 
           dbh2 = NA_real_,
           treestatus = ifelse((dbh1 < 100 & treestatus == 2), 1, treestatus), 
           ba1 = (pi*((dbh1/1000)/2)^2),
           ba2 = (pi*((dbh2/1000)/2)^2), 
           height1 = NA_real_, 
           height2 = NA_real_, 
           surveydate1 = as.numeric(substr(as.character(survey1), 1, 4)), 
           surveydate2 = as.numeric(substr(as.character(survey2), 1, 4)), 
           yearsbetweensurveys = surveydate2 - surveydate1, 
           ba_ha1 = as.numeric(gsub("\\,", "\\.", ba_per_ha)),
           ba_ha2 = NA_real_,
           weight1 = ba_ha1/ba1, 
           weight2 = NA_real_, 
           bachange_ha_yr = weight1*(ba2 - ba1)/yearsbetweensurveys, 
           country = "Finland", 
           Code = as.character(Code)) %>%
    filter(tree_type_1st_census %in% c("0", "1", "3", "7")) %>%
    left_join(species, by = "Code") %>%
    dplyr::select(treecode, plotcode, species = Fullname, treestatus, dbh1, dbh2, 
                  height1, height2, ba1, ba_ha1, ba2, ba_ha2, bachange_ha_yr, 
                  weight1, weight2, country)
}



#' Format Finnish NFI tree table to FUNDIV template
#' @param FinnishNFI_tree_raw Finnish NFI raw tree table
#' @param FUNDIV_tree_FI Finnish NFI tree table formatted for FUNDIV
#' @param species table to convert species code to Latin name
format_FinnishNFI_plot_to_FUNDIV <- function(FinnishNFI_tree_raw, FUNDIV_tree_FI, species){
  FinnishNFI_tree_raw %>%
    filter(x > 3040879 & x < 3681583 & y > 6570363 & y < 7888258) %>%
    st_as_sf(coords = c("x", "y"), crs = 2393) %>%
    st_transform(crs = 4326) %>%
    mutate(longitude = sf::st_coordinates(.)[,1],
           latitude = sf::st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    mutate(plotcode = as.character(plotcode), 
           cluster = NA_real_,
           country = "Finland", 
           surveydate1 = as.numeric(substr(as.character(survey1), 1, 4)), 
           surveydate2 = as.numeric(substr(as.character(survey2), 1, 4)), 
           yearsbetweensurveys = surveydate2 - surveydate1, 
           biome = NA_real_, 
           # format disturbance nature depending on disturbance code in Finnish NFI
           disturbance.nature = case_when(stand_level_dist_agent == "A1" ~ "storm", 
                                          stand_level_dist_agent == "A5" ~ "fire", 
                                          stand_level_dist_agent == "A2" ~ "other", 
                                          substr(stand_level_dist_agent, 1, 1) == "B" ~ "other", 
                                          substr(stand_level_dist_agent, 1, 1) == "C" ~ "other", 
                                          TRUE ~ "none"), 
           # consider no disturbance if the disturbance was before the 1st census
           disturbance.nature = ifelse(stand_level_dist_time_since != 4, disturbance.nature, "none"),
           # consider no disturbance if stand level severity is too low
           disturbance.nature = ifelse(stand_level_dist_sever %in% c("1", "2", "3", "A", "B"), 
                                       disturbance.nature, "none")) %>%
    dplyr::select(plotcode, cluster, country, longitude, latitude, disturbance.nature,
                  yearsbetweensurveys, surveydate1, surveydate2, biome) %>%
    distinct() %>%
    filter(plotcode %in% FUNDIV_tree_FI$plotcode) %>%
    left_join((FUNDIV_tree_FI %>%
                 mutate(harvest = case_when(treestatus == 3 ~ 1, TRUE ~ 0), 
                        dead = case_when(treestatus > 2 ~ 1, TRUE ~ 0)) %>%
                 group_by(plotcode) %>%
                 summarise(ba_ha1 = sum(ba_ha1, na.rm = T),
                           ba_ha2 = sum(ba_ha2, na.rm = T), 
                           n.harvest = sum(harvest, na.rm = T), 
                           percent.dead = round(sum(dead, na.rm = T)/n()*100, digits = 0)) %>%
                 mutate(management = case_when(n.harvest > 0 ~ 1, TRUE ~ 0)) %>%
                 dplyr::select(plotcode, ba_ha1, ba_ha2, management, percent.dead)), 
              by = "plotcode") %>%
    mutate(disturbance.nature = ifelse(percent.dead == 0, "none", disturbance.nature), 
           disturbance.severity = case_when((disturbance.nature != "none" & percent.dead %in% c(1:25)) ~ 1, 
                                            (disturbance.nature != "none" & percent.dead %in% c(26:50)) ~ 2, 
                                            (disturbance.nature != "none" & percent.dead %in% c(51:75)) ~ 3, 
                                            (disturbance.nature != "none" & percent.dead %in% c(76:100)) ~ 4,
                                            TRUE ~ 0)) %>%
    dplyr::select(plotcode, cluster, country, longitude, latitude, 
                  yearsbetweensurveys, surveydate1, surveydate2, biome, 
                  ba_ha1, ba_ha2, management, disturbance.severity, disturbance.nature)
}

#' Format climate FinnishNFI
#' @param sgdd_file file containing sgdd per plotcode
#' @param wai_file file containing wai per plotcode
#' @param FUNDIV_plot_FI Finnish NFI plot data formatted to FUNDIV template
process_climate <- function(sgdd_file, wai_file, FUNDIV_plot_FI){
  # Read sgdd and wai tables
  sgdd.table <- fread(sgdd_file) %>% mutate(plotcode = as.character(plotcode))
  wai.table <- fread(wai_file) %>% mutate(plotcode = as.character(plotcode))
  
  # final output
  out <- FUNDIV_plot_FI %>%
    # Compute mean sgddd for survey dates
    left_join(sgdd.table, by = "plotcode") %>%
    gather(key = "year", value = "SGDD", as.character(c(1985:2018))) %>%
    filter(year %in% c(surveydate1:surveydate2)) %>%
    mutate(SGDD = as.numeric(gsub("\\,", "\\.", SGDD))) %>%
    group_by(plotcode, surveydate1, surveydate2) %>%
    summarize(sgdd = mean(SGDD, na.rm = TRUE)) %>%
    # Compute mean WAi for surveydates
    left_join(wai.table, by = "plotcode") %>%
    gather(key = "year.character", value = "WAI", paste0(c(1983:2018), "_wai")) %>%
    mutate(year = as.numeric(substr(year.character, 1, 4))) %>%
    filter(year %in% c(surveydate1:surveydate2)) %>%
    mutate(WAI = as.numeric(gsub("\\,", "\\.", WAI))) %>%
    group_by(plotcode, sgdd) %>%
    summarize(wai = mean(WAI, na.rm = TRUE))
}

#' Function to get the path of a file, and create directories if they don't exist
#' @param file.in character: path of the file, filename included (ex: "plot/plot.png")
create_dir_if_needed <- function(file.in){
  
  path.in <- strsplit(file.in, "/")[[1]]
  if(length(path.in) > 1){
    for(i in 1:(length(path.in)-1)){
      if(i == 1) path.in_i <- path.in[i]
      else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
      if(!dir.exists(path.in_i)) dir.create(path.in_i)
    }
  }
}




#' Write a table on disk
#' @param table.in dataframe to write on the disk
#' @param file.in Name (and path) of the file on the disk
write_on_disk <- function(table.in, file.in){
  create_dir_if_needed(file.in)
  write.table(table.in, file = file.in, row.names = F)
  return(file.in)
}

