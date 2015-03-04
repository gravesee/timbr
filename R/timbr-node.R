#' Node class constructor
#' 
#' @param x a \link{timbr} object
#' @param n an integer corresponding to the node sequence number
#' @param species the species of timbr, e.g (randomForest, GBM)
#' @param parent the sequence number if the parent node
#' @return a node object consisting of node text, functions, and other metadata
node <- function(x, n, species, parent = NULL) {
  if (is.null(parent)) fn <- NULL else fn <- parent$FUN
  
  nodeText <- getNodeText(x, n, species)
  nodeFn   <- getNodeFunction(x, n, species)
  
  return(structure(
    list(
    nodeID = n,
    nodeText = paste(parent$nodeText, nodeText, sep = '\n'),
    FUN = combineFuncs(fn, nodeFn),
    nodeStatus = x[n,6],
    nodePred = x[n,7]),
    class = 'node'
  ))
}

#' Node membership function
#' 
#' @param x a \link{timbr} object
#' @param n an integer corresponding to the node sequence number
#' @param species the species of timbr, e.g (randomForest, GBM)
#' @return A function that returns a boolean value for the logical condition
#' represented by a node.
getNodeFunction <- function(x, n, species) {
  lin <- findLineage(x, n)
  var <- x[lin[1], 4]
  val <- x[lin[1], 5]
  dir <- lin[2]
  
  if (dir == 3) {
    text <- sprintf("function(x) is.na(x[, %s])", var)
    return(eval(parse(text = text)))
  }
  
  if (species$varTypes[var] == 'Numeric') {
    text <- sprintf("function(x) x[, %s] %s %s & !is.na(x[, %s])",
                    var, species$equality[[dir]], val, var)
    
  } else {
    lvls <- species$xLevels[[var]]
    ids <- if (species$varTypes[var] == 'Ordered')1:val else
      which(toBinary(val) == 1)
    
    mems <- if (dir == 1) lvls[ids] else lvls[-ids]
    text <- sprintf("function(x) x[, %s] %%in%% c('%s') & !is.na(x[, %s])",
                    var, paste(mems, collapse = "','"), var)
  }
  return(eval(parse(text = text)))
}

#' Node membership text
#' 
#' @param x a \link{timbr} object
#' @param n an integer corresponding to the node sequence number
#' @param species the species of timbr, e.g (randomForest, GBM)
#' @return Returns the text representing the logical condition of the parent
#' node
getNodeText <- function(x, n, species) {
  lin <- findLineage(x, n)
  
  var <- x[lin[1], 4]
  vnm <- species$varNames[var]
  val <- x[lin[1], 5]
  dir <- lin[2]
  
  if (dir == 3) return(sprintf("%s is Missing", vnm))
  
  if (species$varTypes[var] == 'Numeric') {
    eq   <- species$equality
    text <- sprintf("%s %s %s", vnm, eq[[dir]], val)
    
  } else {
    lvls <- species$xLevels[[var]]
    ids <- if (species$varTypes[var] == 'Ordered')1:val else
      which(toBinary(val) == 1)
    
    mems <- if (dir == 1) lvls[ids] else lvls[-ids]
    text <- sprintf("%s in c('%s')", vnm, paste(mems, collapse = "','"))
  }
  return(text)
}

#' Target variable statistics specification
#' 
#' @param x target variable
#' @return This function is used to aggregate a target variable over the levels
#' of a node. Returns the toal count, sum, and mean of the target variable.
node.print.func <- function(x) {      
  c(
    totN  = format(length(x), digits=0),
    sumY  = format(sum(x, na.rm=T), digits=0),
    meanY = format(mean(x, na.rm=T), digits=3)
  )
}

#' Print node
#' 
#' Print the logical conditions or rules necessary to pass through a node. If 
#' optional dataset and target variable are passed as well, the performance for
#' the node will be summarized in a table.
#' 
#' @param node
#' @param newdata A dataset consisting of the same columns and order as the
#' dataset used to train the underlying decision tree.
#' @param y A response variable to be aggregated showing how the node performs
#' @details if \code{newdata} and \code{y} are passed into the function as well,
#' a table of summarized performance will be printed after the node text.
print.node <- function(node, newdata=NULL, y=NULL) {
  cat(node$nodeText, '\n')
  
  # if newdata and y are passed, print stats
  if (!is.null(newdata) & !is.null(y)) {    
    df <- data.frame(y = as.matrix(y)[,1], node = node$FUN(newdata))    
    cat('\nPerformance:\n')
    print(aggregate(y ~ node, df, node.print.func))
  }
}
