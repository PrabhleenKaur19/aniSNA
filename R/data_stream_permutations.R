obtain_permuted_network_versions <- function(species_raw, temporal_thresh, spatial_thresh, n_permutations, n_cores = 1){
  
  #Obtain randomized order of dates of observation
  species_permutations <- obtain_permutation_dates(species_raw, n_permutations, n_cores)
  
  permuted_networks_list <- vector(mode = "list", length = length(species_permutations))
  
  pb <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = length(species_permutations), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  for(i in 1:length(species_permutations)){
    species_raw$datetime <- species_permutations[[i]]
    species_raw$datetime <- as.POSIXct(species_raw$datetime, format = "%Y-%m-%d %H:%M")
    interactions <- get_interactions(species_raw = species_raw, temporal_thresh = temporal_thresh, spatial_thresh = spatial_thresh, n_cores = n_cores)
    permuted_networks_list[[i]] <- network_from_interactions(species_raw = species_raw, interactions = interactions, n_cores = n_cores)
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  
  return(permuted_networks_list)
}