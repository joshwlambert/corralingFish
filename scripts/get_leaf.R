get_leaf <- function(phy4object, leaf_name) {
  the_node <- which(phy4object@label == leaf_name)
  return(unname(edgeLength(phy4object)[getEdge(phy4object, the_node)]))
}
