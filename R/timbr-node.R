# create a node class object
# x is a timbr object
# n is which node
# parent is the parent node
node <- function(x, n, species, parent = NULL) {
  if (is.null(parent)) fn <- NULL else fn <- parent$FUN
  
  nodeText <- getNodeText(x, n, species)
  nodeFn   <- getNodeFunction(x, n, species)
  
  return(structure(list(
    nodeID = n,
    nodeText = paste(parent$nodeText, nodeText, sep = '\n'),
    FUN = combineFuncs(fn, nodeFn),
    nodeStatus = x[n,6],
    nodePred = x[n,7],
    class = 'node'
  )))
}

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


