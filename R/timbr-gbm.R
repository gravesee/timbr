# timbr interface to randomForest

#' @export
species.gbm <- function(x) {
  # bind necessary elements into a matrix
  varInfo <- cbind(
    ncat = ifelse(x$var.type == 0, 1, 0),
    num  = ifelse(sapply(x$var.levels, is.numeric), 1, 0))
  
  rownames(varInfo) <- x$var.names
  
  # 1 = ordered, 2 = numeric, 0 = factor
  varTypes <- factor(apply(varInfo, 1, sum), levels = 0:2,
                     labels = c('Factor', 'Ordered', 'Numeric'))
  
  nCat <- ifelse(varTypes == 'Numeric', 1, sapply(x$var.levels, length))  
  
  # names
  varNames <- x$var.names
  if (is.null(varNames)) varNames <- paste0('v', seq_along(varTypes))
  
  return(structure(list(
    family   = class(x),
    nTrees   = x$n.trees,
    varNames = varNames,
    varTypes = varTypes,
    equality = list('<', '>='),
    xLevels  = x$var.levels,
    nCat     = nCat,
    missing  = TRUE,
    class    = 'species'
  )))
}

# map gbm forest tree to timbr tree
#' @export
timbr.gbm <- function(x, i, species=NULL) {
  if (is.null(species)) species <- species(x)
  
  varTypes <- species$varTypes
  
  # look at each split var -- if factor, replace split val with c.splits
  # c.splits with values of -1 go left, values of 1 go right
  # in randomForest 1 goes left and 0 goes right
  splitVar <- ifelse(x$trees[[i]][[1]] != -1, x$trees[[i]][[1]] + 1, 0)
  splitVal <- ifelse(x$trees[[i]][[1]] != -1, x$trees[[i]][[2]], 0)  
  sapply(seq_along(splitVar), function(i) {
    if (splitVar[i] > 0) {
      if (varTypes[splitVar[i]] == 'Factor') {
        splitVal[i] <<- fromBinary(x$c.splits[[splitVal[i]+1]] == -1)
      } else if (varTypes[splitVar[i]] == 'Ordered') {
        splitVal[i] <<- splitVal[i] + 1
      }
    }
  })
  
  structure(cbind(
    left       = ifelse(x$trees[[i]][[3]] != -1, x$trees[[i]][[3]] + 1, 0),
    right      = ifelse(x$trees[[i]][[4]] != -1, x$trees[[i]][[4]] + 1, 0),
    missing    = ifelse(x$trees[[i]][[5]] != -1, x$trees[[i]][[5]] + 1, 0),
    splitVar   = splitVar,
    splitVal   = splitVal,
    nodeStatus = ifelse(x$trees[[i]][[1]] == -1, -1, 0),
    nodePred   = x$trees[[i]][[8]]),
    species    = species,
    class      = 'timbr')
}