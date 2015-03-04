#' Create a lumberYard from a decistion tree producint object
#' 
#' lumberYard wraps the timbr conversion and harvesting process into one, simple
#' function call. It is the object by which decision tree models can be broken
#' down into smaller pieces called nodes or rules.
#' 
#' @param object an object produced by a decision tree model aglorithm such as
#' randomForest or GBM. Any model such model can be used with lumberYard 
#' provided there is a timbr and species specification for the model.
#' @return a lumberYard object
#' 
#' @export lumberYard
lumberYard <- function(object, ...) {  
  species <- species(object)
  
  idx <- list()
  lumberList <- list()
  for (n in 1:species$nTrees) {
    x <- harvest(timbr(object, n, species))
    idx[[n]] <- cbind(n, seq_along(x))
    lumberList[[n]] <- x    
  }  
  
  structure(
    lumberList,
    index = do.call(rbind, idx),
    class  = 'lumberYard')
}

#' Predict function for lumberYard objects
#' 
#' A generic predict method has been implemented for lumberYard objects. Several
#' types of prediction are available. See params for details.
#' 
#' @param x a \code{\link{lumberYard}} object
#' @param newdata a dataset for which predictions are to be returned. Should
#' have the same columns and order as the dataset used to train the model.
#' @param i an optional numeric vector indicating a subset of nodeIDs to predict
#' @param type several forms of prediction are provided:
#' \enumerate{
#'  \item 'Terminal' - Used to predict the terminal node values.
#'  \item 'All' - Returns a matrix of 0s and 1s consisting of node membership.
#'  Note that all of the nodes are returned including internal ones.
#'  \item 'IDs' - Rather than the terminal node prediction, return the terminal
#'  node ID. Can be used to create different prediction values than those stored
#'  by the decistion tree model object used to create the lumberYard.
#'  \item 'Nodes' - When passed \code{i}, returns binary membership for only 
#'  those requested nodes.}
#'  @export predict.lumberYard
predict.lumberYard <- function(x, newdata, i=NULL, type='Terminal') {  
  if (is.null(i)) i <- 1:length(x)
  
  FUNCS <- list(
    'Terminal' = predictTerminalNodes,
    'All' = predictAllNodes,
    'IDs' = predictTerminalNodeID,
    'Nodes' = predictSpecificNodes)
  
  if (type == 'Nodes') {
    # only predict the requested nodes
    do.call(cbind, lapply(i, predictSpecificNodes, x, newdata))    
  } else {
    do.call(cbind, lapply(x, FUNCS[[type]], newdata))
  }
}

# function to return terminal node prediction
predictTerminalNodes <- function(x, newdata) {
  res <- numeric(nrow(newdata))
  for (node in x[attr(x, 'termNodes')]) {
    res[node$FUN(newdata)] <- node$nodePred
  }
  return(res)
}

# returns a 1/0 flag for every node in a tree
predictAllNodes <- function(x, newdata) {  
  do.call(cbind, lapply(x, function(x) as.integer(x$FUN(newdata))))
}

# returns the ID of the terminal node
predictTerminalNodeID <- function(x, newdata) {  
  res <- numeric(nrow(newdata))
  for (node in x[attr(x, 'termNodes')]) {
    res[node$FUN(newdata)] <- node$nodeID
  }
  return(res)
}

# returns a 1/0 flag for specified nodes
predictSpecificNodes <- function(i, x, newdata) {
  node <- getNode(x, i)
  as.integer(node$FUN(newdata))
}

# return specific node from lumberYard
getNode <- function(x, nodeID) {
  tree <- attr(x, 'index')[nodeID,1]
  node <- attr(x, 'index')[nodeID,2]
  x[[tree]][[node]]
}

#' Print specific nodes
#' 
#' Print the text representation of node membership and optionally print target
#' variable statistics if provided a dataset and target variable.
#' 
#' @param x a \code{\link{lumberYard}} object
#' @param nodes a numeric vector of nodeIDs
#' @param newdata a dataset for which predictions are to be returned. Should
#' have the same columns and order as the dataset used to train the model.
#' @param y an optional numeric vector indicating a subset of nodeIDs to predict
#' @return prints text representation to the console. Redirect using \code{sink}.
#' @export printNodes
printNodes <- function(x, nodes, newdata=NULL, y=NULL) {
  for (nodeID in nodes) {
    print(getNode(x, nodeID), newdata, y)
    cat('\n')
  }  
}