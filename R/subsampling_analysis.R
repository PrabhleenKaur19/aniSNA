#' To generate subsamples and obtain network metrics of the subsamples
#'
#' @param network An igraph graph object consisting of observed network
#' @param n_simulations Number of sub-samples to be obtained at each level
#' @param subsampling_proportion A vector depicting proportions of sub-sampled nodes
#' @param network_metrics A vector depicting names of global network metrics 
#'
#' @return A list of network metrics. Each element of list is a matrix whose columns 
#'         correspond to subsampling_proportion and rows correspond to n_simulations. 
#'         The entries of the matrix provide values of the corresponding metric. 
#' @export
#'
#' @examples
network_subsamples <- function(network, 
                              n_simulations = 100, 
                              subsampling_proportion = c(0.1, 0.30, 0.50, 0.70, 0.90),
                              network_metrics = c("density", "mean_strength", "diameter", "transitivity")) {
  
  result <- list() 
  result <- lapply(1:length(network_metrics), function(i){ 
    result[[network_metrics[i]]] <- matrix(0, 
                                           n_simulations, 
                                           length(subsampling_proportion), 
                                           dimnames = list(as.character(c(1:n_simulations)), as.character(subsampling_proportion*100)))
    })
  names(result) <- network_metrics
  
  for (i in 1:n_simulations) {
    for (j in 1:length(subsampling_proportion)) {
      random_sample_nodes <- as.vector(sample(igraph::V(network), size = subsampling_proportion[j] * igraph::gorder(network)))
      sub_network <- igraph::induced_subgraph(network, random_sample_nodes, impl = "auto")
      
      if("density" %in% network_metrics){result$density[i, j] <- igraph::edge_density(sub_network)}
      if("mean_strength" %in% network_metrics){result$mean_strength[i, j] <- mean(igraph::strength(sub_network))}
      if("diameter" %in% network_metrics){result$diameter[i, j] <- igraph::diameter(sub_network)}
      if("transitivity" %in% network_metrics){result$transitivity[i, j] <- igraph::transitivity(sub_network)}
      
    }
  }
  
  return(result)
}



#' To plot sub-sampling results
#'
#' @param result A list of matrices obtained from network_subsamples function
#' @param network An igraph graph object consisting of observed network
#'
#' @return NULL
#' @export
#'
#' @examples
plot_subsamples <- function(result, network){
  
  for(i in 1:length(result)){
    graphics::boxplot(
      x = result[[i]], xaxt = "n", xlab = "Sub Sample Size(in %)", ylab = "Value", outline = FALSE,
      border = "black",
      col = "lightblue"
    )
    graphics::axis(side = 1, at = c(1:ncol(result[[i]])), labels = colnames(result[[i]]))
    graphics::title(names(result)[i], adj = 0.5, line = 1)
    graphics::legend("bottomright", legend = "Value of \nfull network",col = "red", lty = 1)
    
    if (names(result)[i] == "density") {
      graphics::abline(h = igraph::edge_density(network), col = "red")
    } else if (names(result)[i] == "mean_strength") {
      graphics::abline(h = mean(igraph::strength(network)), col = "red")
    } else if (names(result)[i] == "transitivity") {
      graphics::abline(h = igraph::transitivity(network), col = "red")
    } else {
      graphics::abline(h = igraph::diameter(network), col = "red")
    }
  }
}



