# class that takes the processed timbr and exposes it


# object that creates a store of processed timbr from a tree algorithm object
#' @export
lumberYard <- function(object, ...) {  
  species <- species(object)  
  structure(
    lapply(1:species$nTrees, function(i) harvest(timbr(object, i, species))),
    class  = 'lumberYard')
}

# x is a list of nodes
lumber <- function(x) {
  structure(
    x,
    nNodes = length(x),
    termNodes = which(vapply(x, '[[', numeric(1), 'nodeStatus') == -1),
    class='lumber')
}

# res <- lapply(1:mod$n.trees, function(x) harvest(timbr(mod, x)))

# print a lumber object
printLumber <- function(x) {
    cat(sprintf("Total Nodes: %3s, Terminal: %3s\n",
                attr(x, 'nNodes'), length(attr(x, 'nTermNodes'))))
}

# returns terminal nodes subset
getTerminalNodes <- function(x) {
  nodeStatus <- vapply(x, '[[', numeric(1), 'nodeStatus')
  return(lumber(x[nodeStatus]))
}

# prediction functions for lumber objects
#' @export
predict.lumberYard <- function(x, newdata, i=NULL, type='Terminal') {  
  if (is.null(i)) i <- 1:length(x)
  
  FUNCS <- list(
    'Terminal' = predictTerminalNodes,
    'All' = predictAllNodes,
    'IDs' = predictTerminalNodeID)
  
  do.call(cbind, lapply(x, FUNCS[[type]], newdata))  
}

# function to return terminal node prediction
predictTerminalNodes <- function(x, newdata) {  
  res <- numeric(nrow(newdata))
  for (node in x[attr(x, 'termNodes')]) {
    res[node$FUN(newdata)] <- node$nodePred
  }
  return(res)
}

# function that returns a flag for each node in each tree
predictAllNodes <- function(x, newdata) {  
  do.call(cbind, lapply(x, function(x) as.integer(x$FUN(newdata))))
}

predictTerminalNodeID <- function(x, newdata) {  
  res <- numeric(nrow(newdata))
  for (node in x[attr(x, 'termNodes')]) {
    res[node$FUN(newdata)] <- node$nodeID
  }
  return(res)
}



