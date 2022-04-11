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
           weight1 = NA_real_, 
           weight2 = NA_real_, 
           ba_ha1 = ba1*weight1,
           ba_ha2 = ba2*weight1,
           bachange_ha_yr = weight1*(ba2 - ba1)/5, 
           country = "Finland", 
           Code = as.character(Code)) %>%
    filter(tree_type_1st_census %in% c("0", "1", "3", "7")) %>%
    left_join(species, by = "Code") %>%
    dplyr::select(treecode, plotcode, species = Fullname, treestatus, dbh1, dbh2, 
                  height1, height2, ba1, ba_ha1, ba2, ba_ha2, bachange_ha_yr, 
                  weight1, weight2, country)
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

