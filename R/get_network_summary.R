
#' Calculates and prints network summary statistics
#'
#' @param network An undirected network with nodes representing animal IDs and edges representing associations between them.
#'
#' @return NULL
#' @export
#'
#' @examples
get_network_summary <- function(network)
{
  cat("The number of vertices are ", igraph::gorder(network), "\n") # can also use the function vcount()
  cat("The number of edges are ", igraph::gsize(network), "\n") # can also use the function ecount()
  cat("Vertex Attributes are : ", igraph::list.vertex.attributes(network), "\n")
  cat("Edge Attributes are : ", igraph::list.edge.attributes(network), "\n")
  cat("The edge density of the network is : ", igraph::edge_density(network), "\n")
  cat("The mean degree is ", mean(igraph::degree(network)), "\n")
  cat("The mean strength is ", mean(igraph::strength(network)), "\n")
  cat("The diameter is ", igraph::diameter(network, weights = NA), "\n")
  cat("The transitivity is ", igraph::transitivity(network), "\n")
  cat("The mean geodesic distance is ", igraph::mean_distance(network), "\n")
}