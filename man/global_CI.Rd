% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_confidence_intervals.R
\name{global_CI}
\alias{global_CI}
\title{To obtain confidence intervals around the observed global network statistics}
\usage{
global_CI(
  network,
  n_versions = 100,
  network_metrics_functions_list = c(edge_density = function(x) igraph::edge_density(x),
    diameter = function(x) igraph::diameter(x, weights = NA), transitivity = function(x)
    igraph::transitivity(x)),
  CI_size = 0.95
)
}
\arguments{
\item{network}{An igraph object consisting of observed network.}

\item{n_versions}{Number of bootstrapped versions to be used. (default = 100)}

\item{network_metrics_functions_list}{A list consisting of function definitions of the network metrics that the user wants to evaluate. Each element in the list should have an assigned name.
Default = c("edge_density" = function(x) igraph::edge_density(x), "diameter" = function(x) igraph::diameter(x, weights = NA), "transitivity" = function(x) igraph::transitivity(x))}

\item{CI_size}{Size of confidence interval. Default is 0.95 that generates a 95\% confidence interval.}
}
\value{
A DataFrame consisting of three columns. The first column contains the value of observed network metric, the second and 
third column represent the lower and upper limit of 95% confidence interval respectively. The rows correspond to the network metrics chosen to evaluate.
}
\description{
To obtain confidence intervals around the observed global network statistics
}
\examples{
\donttest{
data(elk_network_2010)
global_CI(elk_network_2010, n_versions = 100, 
network_metrics_functions_list = c("edge_density" = function(x) igraph::edge_density(x),
"diameter" = function(x) igraph::diameter(x, weights = NA),
"transitivity" = function(x) igraph::transitivity(x)))
}
}
