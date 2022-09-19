

#' To obtain width of confidence intervals using bootstrapped versions at each level of sub-sampling
#'
#' @param network An igraph object
#' @param n_versions Number of bootstrapped versions to be used
#' @param seed seed number
#' @param n.iter Number of iterations at each level
#' @param network_metrics Network metrics to be evaluated. This should be supplied as a character vector and the values 
#' should be chosen from "mean_degree", "mean_strength", "density", "diameter", "transitivity". (default = c("mean_degree", "mean_strength", "density", "diameter", "transitivity")).
#'
#'
#' @return A matrix of class Width_CI_matrix containing width of Confidence Intervals where each row corresponds to the sub-sample size and columns correspond to the chosen network metric.
#' @export
#'
#' @examples
width_CI <- function(network,
                     n_versions = 1000,
                     seed = 12345, 
                     n.iter = 10, 
                     network_metrics = c("mean_degree", "mean_strength", "density", "diameter", "transitivity")){
  
  sample_size_values <- seq(10, igraph::gorder(network), 10)
  mean_value_CI_len <- data.frame(temp = numeric(0))
  for(i in 1:length(network_metrics)){
    mean_value_CI_len[[network_metrics[i]]] <- numeric(0)
  }
  mean_value_CI_len <- mean_value_CI_len[,-1]
  
  j <- 1
  for(s in sample_size_values){
    metrics_CI_len <- CI_matrix(network, size_subnet = s, n_versions, n.iter, network_metrics)
    mean_value_CI_len[j,] <- apply(metrics_CI_len, 2, mean, na.rm=TRUE)
   #Scaling the values of Mean Degree, Mean Strength and Diameter
    #mean_value_CI_len[j,c("Mean_degree", "Mean_strength", "Diameter")] <-  mean_value_CI_len[j,c("Mean_degree", "Mean_strength", "Diameter")]/(s/10)
    j <- j+1
  }
  rownames(mean_value_CI_len) <- as.character(seq(10, igraph::gorder(network), 10))
  
  class(mean_value_CI_len) = "Width_CI_matrix"
  
  return(mean_value_CI_len)
}


#' Title To plot the results obtained from width_CI function
#'
#' @param x A matrix of width of Confidence Intervals obtained from width_CI function
#' @param ... Further arguments are ignored.
#'
#' @return NULL
#' @method plot Width_CI_matrix
#' @export
#'
#' @examples
plot.Width_CI_matrix <- function(x,...){
  
  width_CI_results <- x
  
  if(!inherits(width_CI_results,"Width_CI_matrix")){
    stop("Matrix passed is not of class 'Width_CI_matrix'")
  }
  
  subsample_size <- seq(10,10*nrow(width_CI_results), 10)
  col_vec <- c("red", "blue", "green", "yellow", "black")
  
    
  for(i in 1:ncol(width_CI_results)){
      plot(subsample_size, 
           width_CI_results[,i], 
           #ylim = c(0, max(width_CI_results[,i])),
           #xlim = c(0, max(subsample_size)),
           type = "b",
           col = col_vec[1],
           main = colnames(width_CI_results)[i],
           pch = 16,
           xlab = "Sample Size",
           ylab = "Width of Confidence Interval")
  }
}