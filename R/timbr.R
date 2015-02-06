# transforms algo tree into timbr tree
#' @export
timbr <- function(object, ...) UseMethod("timbr")

# describes the forest of trees
#' @export
species <- function(object, ...) UseMethod("species")

#' @export
is.timbr <- function(object) {
  res <- FALSE
  if (attr(object, 'class') == 'timbr') res <- TRUE
  res
}

#' @export
print.timbr <- function(x, ...) {
  dims <- attr(x, 'dim')
  dnms <- attr(x, 'dimnames')
  attributes(x) <- NULL
  attributes(x) <- list(dim = dims, dimnames = dnms)
  print(x, ...)
}

# function to unpack a timbr structure
#' @export
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




