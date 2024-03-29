# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Calculate distance between two pairs of radian coordinates
#' @param latf latitude from
#' @param lonf longitude from
#' @param latt latitude to
#' @param lont longitude to
#' 
#' @return distance value in meters
#' @export
distance_radian_coordinates <- function(latf, lonf, latt, lont) {
    .Call(`_aniSNA_distance_radian_coordinates`, latf, lonf, latt, lont)
}

#' Function to obtain pairs of interacting animals
#' @param i Index of the animal 
#' @param datetime DateTime vector
#' @param latitude latitude vector
#' @param longitude longitude vector
#' @param temporal_thresh time threshold in minutes
#' @param spatial_thresh spatial threshold in meters
#' 
#' @return A matrix consisting of two row. The first row corresponds to the interacting indices and the second row to the respective distances. 
#' @export
interacting_pairs <- function(i, datetime, latitude, longitude, temporal_thresh, spatial_thresh) {
    .Call(`_aniSNA_interacting_pairs`, i, datetime, latitude, longitude, temporal_thresh, spatial_thresh)
}

