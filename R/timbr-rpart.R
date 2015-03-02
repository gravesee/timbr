# timbr interface to rpart (WORK IN PROGRESS)

species.rpart <- function(x) {
  
  # varTypes
  classes <- attr(x$terms, 'dataClasses')
  varTypes <- ifelse(classes == "numeric", 2, ifelse(classes == "ordered", 1, 0))
  varTypes <- varTypes[-attr(x$terms, 'response')]
  
  # xlevels -- list for ALL variables of levels found in variables / Numeric 0
  xLevels <- lapply(seq_along(varTypes), function(x) {
    res <- attr(rp, 'xlevels')[[names(varTypes)[[x]]]]
    if (is.null(res)) 0 else res
  })
  names(xLevels) <- names(varTypes)
  
  # nCat
  nCat <- ifelse(varTypes == 'Numeric', 1, sapply(xLevels, length))
  
  # names
  varNames <- names(varTypes)
  if (is.null(varNames)) varNames <- paste0('v', seq_along(varTypes))
  
  return(structure(list(
    family   = class(x),
    nTrees   = 1,
    varNames = varNames,
    varTypes = varTypes,
    equality = list('<', '>='),
    xLevels  = xLevels,
    nCat     = nCat,
    missing  = TRUE,
    class    = 'species'
  )))
}

# map gbm forest tree to timbr tree
#' @export
timbr.rpart <- function(x, i, species=NULL) {
  if (is.null(species)) species <- species(x)
  
  varTypes <- species$varTypes
  
  # need to convert rpart row.names to binary
  ids <- sapply(as.numeric(row.names(x$frame)), timbr:::toBinary)
  children <- sapply(ids[-1], timbr:::fromBinary)
  parents  <- sapply(ids[-1], function(x) timbr:::fromBinary(tail(x, -1)))
  
  tmp <- cbind(parents, children)
  uid <- unique(parents)
  lr <- do.call(rbind, lapply(seq_along(uid), function(i) {
    ids <- tmp[which(tmp[,1] == uid[i]), 2]
    which(tmp[,2] %in% ids) + 1
  }))
  
  # combine LR with x$splits
  
  
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
    left       = lr[,1],
    right      = lr[,2],
    missing    = -1,
    splitVar   = splitVar,
    splitVal   = splitVal,
    nodeStatus = ifelse(x$trees[[i]][[1]] == -1, -1, 0),
    nodePred   = x$trees[[i]][[8]]),
    species    = species,
    class      = 'timbr')
}