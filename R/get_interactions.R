#' To obtain interactions from raw GPS observations
#'
#' @param species_raw A DataFrame consisting of GPS observations
#' @param temporal_thresh Temporal threshold in minutes with default 7 minutes
#' @param spatial_thresh Spatial threshold
#' @param n_cores Number of cores for parallel processing with default 1
#'
#' @return A dataframe consisting of five columns. The first two columns contain animal ids, third and fourth column contain timestamp of their observations and the final column contains the distance between the two individuals
#' @export
#'
#' @examples
get_interactions <- function(species_raw, temporal_thresh = 7, spatial_thresh, n_cores = 1) {

  # Sorting the observations by datetime values
  species_raw <- species_raw[with(species_raw, order(species_raw$datetime)), ][1:nrow(species_raw), ]

  # For each row i in data, we get corresponding subsequent rows that are within the user provided temporal and spatial threshold
  list_ijs <- parallel::mclapply(1:(nrow(species_raw) - 1), function(i) {
    interacting_pairs(
      i - 1, species_raw$datetime,
      species_raw$latitude_rad,
      species_raw$longitude_rad, temporal_thresh, spatial_thresh
    )
  },
  mc.cores = n_cores
  )

  # Drop the null lists
  keep <- !sapply(list_ijs, is.null)
  keep <- c(keep, FALSE)
  interactions_new <- data.frame(
    Animal_A = rep(species_raw$animal_id[keep], unlist(sapply(list_ijs[keep], function(x) ncol(x)))),
    Animal_B = species_raw$animal_id[unlist(sapply(list_ijs[keep], function(x) x[1, ]))],
    Timestamp_A = rep(species_raw$datetime[keep], unlist(sapply(list_ijs[keep], function(x) ncol(x)))),
    Timestamp_B = species_raw$datetime[unlist(sapply(list_ijs[keep], function(x) x[1, ]))],
    distance = unlist(sapply(list_ijs[keep], function(x) x[2, ])), stringsAsFactors = FALSE
  )
  interactions_new <- interactions_new[which(interactions_new$Animal_A != interactions_new$Animal_B), ]

  return(interactions_new)
}