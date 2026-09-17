#' Dijkstra Shortest Path Algorithm
#'
#' @param graph A data frame with 3 columns: v1 (source), v2 (destination), and w (weight).
#' @param init_node A numeric scalar representing the starting node.
#'
#' @return A numeric vector containing the shortest distances from init_node to all other nodes.
#' @export
#'
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}
#'
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
dijkstra <- function(graph, init_node) {
  # Input validations
  if (!is.data.frame(graph)) {
    stop("The 'graph' argument must be a data frame.")
  }
  if (!all(c("v1", "v2", "w") %in% colnames(graph))) {
    stop("The graph data frame must contain columns 'v1', 'v2', and 'w'.")
  }
  if (!is.numeric(init_node) || length(init_node) != 1) {
    stop("The 'init_node' must be a single numeric value.")
  }

  nodes <- unique(c(graph$v1, graph$v2))

  if (!(init_node %in% nodes)) {
    stop("The specified 'init_node' does not exist in the graph.")
  }

  # Initialize distance matrix & unvisited tracker
  dist_vec <- rep(Inf, length(nodes))
  names(dist_vec) <- nodes
  dist_vec[as.character(init_node)] <- 0

  unvisited_set <- nodes

  while (length(unvisited_set) > 0) {
    # Find unvisited node with minimum distance
    sub_dists <- dist_vec[as.character(unvisited_set)]
    curr_node <- unvisited_set[which.min(sub_dists)]
    curr_dist <- dist_vec[as.character(curr_node)]

    # Break early if remaining nodes are unreachable
    if (is.infinite(curr_dist)) break

    # Check neighbors of current node
    neighbors <- graph[graph$v1 == curr_node, ]

    if (nrow(neighbors) > 0) {
      for (i in seq_len(nrow(neighbors))) {
        target_node <- neighbors$v2[i]
        edge_weight <- neighbors$w[i]
        alt_route <- curr_dist + edge_weight

        if (alt_route < dist_vec[as.character(target_node)]) {
          dist_vec[as.character(target_node)] <- alt_route
        }
      }
    }

    # Mark node as visited
    unvisited_set <- unvisited_set[unvisited_set != curr_node]
  }

  return(unname(dist_vec))
}
