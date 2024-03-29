% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsampling_analysis.R
\name{plot.Subsampled_Network_Metrics}
\alias{plot.Subsampled_Network_Metrics}
\title{To plot sub-sampling results}
\usage{
\method{plot}{Subsampled_Network_Metrics}(
  x,
  network,
  network_metrics_functions_list = c(edge_density = function(x) igraph::edge_density(x),
    diameter = function(x) igraph::diameter(x, weights = NA), transitivity = function(x)
    igraph::transitivity(x)),
  ...
)
}
\arguments{
\item{x}{A list of matrices belonging to class "Subsampled_Network_Metrics" and is obtained from subsampled_network_metrics function}

\item{network}{An igraph graph object consisting of the observed network}

\item{network_metrics_functions_list}{This is the same argument that is passed for obtaining the results from the function subsampled_network_metrics. A list consisting of function definitions of the network metrics that the user wants to evaluate. Each element in the list should have an assigned name.
Default = c("edge_density" = function(x) igraph::edge_density(x), "diameter" = function(x) igraph::diameter(x, weights = NA), "transitivity" = function(x) igraph::transitivity(x))}

\item{...}{Further arguments are ignored}
}
\value{
No return value, called for side effects. The boxplots depict range of values, network metrics take when multiple subsamples are chosen from the observed sample.
}
\description{
To plot sub-sampling results
}
\examples{

data(elk_network_2010)
elk_subsamples <- subsampled_network_metrics(elk_network_2010)
plot(elk_subsamples, elk_network_2010, 
network_metrics_functions_list = c("edge_density" = function(x) igraph::edge_density(x),
"diameter" = function(x) igraph::diameter(x, weights = NA),
"transitivity" = function(x) igraph::transitivity(x)))

}
