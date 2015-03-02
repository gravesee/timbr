#' Process timbr into lumber
#' 
#' @param x A \code{\link{timbr}} object
#' @details Process a timbr object, which a special class consisting of a
#' matrix and model metadata, into a lumber object. Whereas the timbr object
#' describes what a tree looks like, the lumber object describes what the tree
#' can do. It provides facilities for prediction and interpretation.
#' 
#' @return A lumber object
harvest <- function(x) {
  stopifnot(is.timbr(x))
  
  nodes <- list() # store nodes in a list
  species <- attr(x, 'species')
  
  # recurse on the timbr object passing the nodeID and the parent node
  recurse <- function(x, n = 1, node = NULL) {
    
    if (x[n, 6] == -1) { # terminal node
      nodes[[n]] <<- node
      return(node)
    } else {
      
      lNode <- node(x, x[n, 1], species, node)
      rNode <- node(x, x[n, 2], species, node)      
      
      nodes[[x[n, 1]]] <<- recurse(x, n=x[n, 1], node = lNode)
      nodes[[x[n, 2]]] <<- recurse(x, n=x[n, 2], node = rNode)
      
      if (species$missing) {
        mNode <- node(x, x[n, 3], species, node)
        nodes[[x[n, 3]]] <<- recurse(x, n=x[n, 3], node = mNode)
      }      
      return(node)
    }
  }
  recurse(x)
  return(lumber(nodes[-1]))
}

#' Lumber constructor
#' 
#' @param x A list of \link{nodes}
#' @details The \code{lumber} object is created after harvesting
#' \code{\link{timbr}}. See \link{harvest} for details on processing timbr.
#' @return A processed timbr object, or lumber.
lumber <- function(x) {
  structure(
    x,
    nNodes = length(x),
    termNodes = which(vapply(x, '[[', numeric(1), 'nodeStatus') == -1),
    class='lumber')
}

printLumber <- function(x) {
    cat(sprintf("Total Nodes: %3s, Terminal: %3s\n",
                attr(x, 'nNodes'), length(attr(x, 'nTermNodes'))))
}