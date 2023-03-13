#' To generate subsamples and obtain network metrics of the subsamples
#'
#' @param network An igraph graph object consisting of observed network
#' @param n_simulations Number of sub-samples to be obtained at each level
#' @param subsampling_proportion A vector depicting the levels (in proportion) at which subsamples to be taken
#' @param network_metrics A vector depicting names of global network metrics. Default = network_metrics = c("density", "mean_strength", "diameter", "transitivity")
#'
#' @return A list of network metrics of class "Subsampled_Network_Metrics". Each element of list is a matrix whose columns 
#'         correspond to subsampling_proportion and rows correspond to n_simulations. 
#'         The entries of the matrix provide values of the corresponding metric. 
#' @export
#'
#' @examples
#' 
#' data(elk_network_2010)
#' subsampled_network_metrics(elk_network_2010)
#' 
subsampled_network_metrics <- function(network, 
                              n_simulations = 100, 
                              subsampling_proportion = c(0.1, 0.30, 0.50, 0.70, 0.90),
                              network_metrics = c("density", "mean_strength", "diameter", "transitivity")) {
  
  subsampling_result <- list() 
  subsampling_result <- lapply(1:length(network_metrics), function(i){ 
    subsampling_result[[network_metrics[i]]] <- matrix(0, 
                                           n_simulations, 
                                           length(subsampling_proportion), 
                                           dimnames = list(as.character(c(1:n_simulations)), as.character(subsampling_proportion*100)))
    })
  names(subsampling_result) <- network_metrics
  
  for (i in 1:n_simulations) {
    for (j in 1:length(subsampling_proportion)) {
      random_sample_nodes <- as.vector(sample(igraph::V(network), size = subsampling_proportion[j] * igraph::gorder(network)))
      sub_network <- igraph::induced_subgraph(network, random_sample_nodes, impl = "auto")
      
      if("density" %in% network_metrics){subsampling_result$density[i, j] <- igraph::edge_density(sub_network)}
      if("mean_strength" %in% network_metrics){subsampling_result$mean_strength[i, j] <- mean(igraph::strength(sub_network))}
      if("diameter" %in% network_metrics){subsampling_result$diameter[i, j] <- igraph::diameter(sub_network, weights = NA)}
      if("transitivity" %in% network_metrics){subsampling_result$transitivity[i, j] <- igraph::transitivity(sub_network)}
      
    }
  }
  
  class(subsampling_result) <- "Subsampled_Network_Metrics"
  return(subsampling_result)
}



#' To plot sub-sampling results
#'
#' @param x A list of matrices obtained from network_subsamples function of class "Subsampled_Network_Metrics"
#' @param network An igraph graph object consisting of observed network
#' @param ... Further arguments are ignored
#'
#' @return No return value, called for side effects. The boxplots depict range of values, network metrics take when multiple subsamples are chosen from the observed sample.
#' 
#' @method plot Subsampled_Network_Metrics
#' 
#' @export
#'
#' @examples
#' 
#' data(elk_network_2010)
#' elk_subsamples <- subsampled_network_metrics(elk_network_2010)
#' plot(elk_subsamples, elk_network_2010)
#' 
plot.Subsampled_Network_Metrics <- function(x, network,...){
  
  subsampling_result = x
  
  if(!inherits(subsampling_result, "Subsampled_Network_Metrics")){
    stop("x passed is not of class 'Subsampled_Network_Metrics'")
  }
  
  metrics_list <- list("density" = function(network) return(igraph::edge_density(network)),
                       "mean_strength" = function(network) return(mean(igraph::strength(network))),
                       "transitivity" = function(network) return(igraph::transitivity(network)),
                       "diameter" = function(network) return(igraph::diameter(network, weights = NA))
  )
  
  for(i in 1:length(subsampling_result)){
    graphics::boxplot(
      x = subsampling_result[[i]], xaxt = "n", xlab = "Sub Sample Size(in %)", ylab = "Value", outline = FALSE,
      border = "black",
      col = "lightblue",
      ylim = c(min(subsampling_result[[i]], metrics_list[[names(subsampling_result)[i]]](network), na.rm = TRUE), max(subsampling_result[[i]], metrics_list[[names(subsampling_result)[i]]](network), na.rm = TRUE))
    )
    graphics::axis(side = 1, at = c(1:ncol(subsampling_result[[i]])), labels = colnames(subsampling_result[[i]]))
    graphics::title(names(subsampling_result)[i], adj = 0.5, line = 1)
    graphics::legend("bottomright", legend = "Observed Value",col = "red", lty = 1)
    graphics::abline(h = metrics_list[[names(subsampling_result)[i]]](network), col = "red")
  }
}

#' To obtain sub-networks of the observed network
#'
#' @param network An igraph object
#' @param n_subsamples Number of sub-networks to be obtained. (default = 1)
#' @param subsampling_proportion A value depicting the level (in proportion) at which sub-samples to be taken. (default = 0.5). 
#' This value should lie between 0 and 1 depicting the proportion of observed nodes to be included in the sub-network. 
#'
#' @return A list of size n_subsamples, where each element of the list is an igraph object representing a sub-network of the observed network. 
#' @export
#'
#' @examples
#' data(elk_network_2010)
#' obtain_network_subsamples(elk_network_2010, 1, 0.5)
obtain_network_subsamples <- function(network, n_subsamples = 1, subsampling_proportion = 0.5){
  
  subsamples <- vector(mode = "list", length = n_subsamples)
  
  for (i in 1:n_subsamples) {
      random_sample_nodes <- as.vector(sample(igraph::V(network), size = subsampling_proportion * igraph::gorder(network)))
      sub_network <- igraph::induced_subgraph(network, random_sample_nodes, impl = "auto")
      subsamples[[i]] <- sub_network
  }
  return(subsamples)
}

