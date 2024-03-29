% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsampling_analysis.R
\name{subsampled_network_metrics}
\alias{subsampled_network_metrics}
\title{To generate subsamples and obtain network metrics of the subsamples}
\usage{
subsampled_network_metrics(
  network,
  n_simulations = 100,
  subsampling_proportion = c(0.1, 0.3, 0.5, 0.7, 0.9),
  network_metrics_functions_list = c(edge_density = function(x) igraph::edge_density(x),
    diameter = function(x) igraph::diameter(x, weights = NA), transitivity = function(x)
    igraph::transitivity(x))
)
}
\arguments{
\item{network}{An igraph graph object consisting of observed network}

\item{n_simulations}{Number of sub-samples to be obtained at each level}

\item{subsampling_proportion}{A vector depicting the levels (in proportion) at which subsamples to be taken}

\item{network_metrics_functions_list}{A list consisting of function definitions of the network metrics that the user wants to evaluate. Each element in the list should have an assigned name.
Default = c("edge_density" = function(x) igraph::edge_density(x), "diameter" = function(x) igraph::diameter(x, weights = NA), "transitivity" = function(x) igraph::transitivity(x))}
}
\value{
A list of network metrics of class "Subsampled_Network_Metrics". Each element of list is a matrix whose columns 
        correspond to subsampling_proportion and rows correspond to n_simulations. 
        The entries of the matrix provide values of the corresponding metric.
}
\description{
To generate subsamples and obtain network metrics of the subsamples
}
\examples{

data(elk_network_2010)
elk_subsamples <- subsampled_network_metrics(elk_network_2010)
plot(elk_subsamples, elk_network_2010, 
network_metrics_functions_list = c("edge_density" = function(x) igraph::edge_density(x),
"diameter" = function(x) igraph::diameter(x, weights = NA),
"transitivity" = function(x) igraph::transitivity(x)))

}
