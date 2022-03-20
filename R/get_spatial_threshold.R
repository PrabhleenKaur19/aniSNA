#' To obtain optimized spatial threshold for calculating interactions from raw GPS observations
#'
#' @param species_interactions A dataframe consisting of individual interactions within maximum possible distance 
#'
#' @return Spatial threshold in metres
#' @export
#'
#' @examples
get_spatial_threshold <- function(species_interactions) {
  max_distance <- max(species_interactions$distance)
  breaks <- seq(0, max_distance, by = 5)
  distance.cut <- cut(species_interactions$distance, breaks, right = FALSE)
  distance.freq <- table(distance.cut)

  plot(breaks[-1], as.numeric(distance.freq),
    type = "b",
    col = "blue",
    main = paste("Number observations in each interval", sep = ""),
    ylab = "Number of observations",
    xlab = "Distance Values (interval size = 5)"
  )

  nobs_wrt_distance <- as.data.frame(cbind(breaks[-1], as.numeric(distance.freq)))
  colnames(nobs_wrt_distance) <- c("distance", "interactions_count")

  lin.mod <- stats::lm(interactions_count ~ distance, data = nobs_wrt_distance)
  segmented.mod <- segmented::segmented(lin.mod, seg.Z = ~distance, psi = 30)
  return(segmented.mod$psi[2])
}
