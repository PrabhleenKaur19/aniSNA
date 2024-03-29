% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsampling_analysis.R
\name{obtain_network_subsamples}
\alias{obtain_network_subsamples}
\title{To obtain sub-networks of the observed network}
\usage{
obtain_network_subsamples(
  network,
  n_subsamples = 1,
  subsampling_proportion = 0.5
)
}
\arguments{
\item{network}{An igraph object}

\item{n_subsamples}{Number of sub-networks to be obtained. (default = 1)}

\item{subsampling_proportion}{A value depicting the level (in proportion) at which sub-samples to be taken. (default = 0.5). 
This value should lie between 0 and 1 depicting the proportion of observed nodes to be included in the sub-network.}
}
\value{
A list of size n_subsamples, where each element of the list is an igraph object representing a sub-network of the observed network.
}
\description{
To obtain sub-networks of the observed network
}
\examples{
data(elk_network_2010)
obtain_network_subsamples(elk_network_2010, 1, 0.5)
}
