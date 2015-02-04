# let's start with a randomForest interface
library(randomForest)
library(gbm)

data(titanic, package='mjollnir')

titanic[is.na(titanic$Age), 'Age'] <- median(titanic$Age, na.rm=T)
titanic[, 'Pclass'] <- ordered(titanic$Pclass)

# build a model predicting mpg
rf <- randomForest(titanic[,-1], factor(titanic[,1]), ntree = 1000, maxnodes = 4)
rf <- randomForest(mtcars[,-1], mtcars[,1], ntree = 1000, maxnodes = 4)
mod <- gbm.fit(titanic[,-1], titanic[,1], n.tree = 1000, n.minobsinnode = 5,
               distribution='bernoulli', interaction.depth=3)

#TODO: add RPART model

# pretty print the trees
getTree(rf, 1)
pretty.gbm.tree(mod, 1)


# build a class that contains everything needed to describe a tree
# should be 1-indexed, not 0
# terminal node signal

## PER ALGORITHM
# factor value membership
# list of variables
# number of levels

## PER TREE
# left, right, missing, var, val, status, prediction, EXTRAS: error, weight


#LEFT#    rf$forest$leftDaughter [NNODES, NTREES]
#LEFT#    rf$forest$rightDaughter [NNODES, NTREES]
#MISSING# Not allowed
#VAR#     rf$forest$bestvar [NNODES, NTREES]
#VAL#     rf$forest$xbestsplit [NNODES, NTREES]
#STATUS#  rf$forest$nodestatus [NNODES, NTREES]
#PREDS#   rf$forest$nodepred [NNODES, NTREES]

#####################
#### TIMBR LAYOUT ###
#####################

# terminal nodes are always -1 / others 0  
# index values should always be 1-indexed
# factor splits are stored as an integer

# transforms algo tree into timbr tree
timbr <- function(object, ...) UseMethod("timbr")

# describes the forest of trees
dendrology <- function(object, ...) UseMethod("dendrology")


# function that describes the forest
dendrology.randomForest <- function(x) {
  # TODO: should also check that only two levels are present for classification
  
  # bind necessary elements into a matrix
  varInfo <- cbind(
    ncat = ifelse(x$forest$ncat == 1, 1, 0),
    num  = ifelse(sapply(x$forest$xlevels, is.numeric), 1, 0))
  
  # 1 = ordered, 2 = numeric, 0 = factor
  varTypes <- factor(apply(varInfo, 1, sum), levels = 0:2,
                     labels = c('Factor', 'Ordered', 'Numeric'))

  return(structure(list(
    varTypes = varTypes,
    nCat     = x$forest$ncat, # 1 for numeric/ordered else nlevels
    missing  = FALSE,
    class    = 'dendrology'
  )))
}

dendrology.gbm <- function(x) {
  # bind necessary elements into a matrix
  varInfo <- cbind(
    ncat = ifelse(x$var.type == 0, 1, 0),
    num  = ifelse(sapply(x$var.levels, is.numeric), 1, 0))
  
  rownames(varInfo) <- x$var.names
  
  # 1 = ordered, 2 = numeric, 0 = factor
  varTypes <- factor(apply(varInfo, 1, sum), levels = 0:2,
                     labels = c('Factor', 'Ordered', 'Numeric'))
  
  nCat <- ifelse(varTypes != 'Factor', 1, sapply(x$var.levels, length))
  
  return(structure(list(
    varTypes = varTypes,
    nCat     = nCat,
    missing  = TRUE,
    class    = 'dendrology'
  )))
}

# map random forest tree to timbr tree
timbr.randomForest <- function(x, i, dendrology=NULL) {
  if (is.null(dendrology)) dendrology <- dendrology(x)
  cbind(
    # different for classifaction and regression
    left       = if (x$type == 'regression') x$forest$leftDaughter[,i] else
      x$forest$treemap[,1,i],
    right      = if (x$type == 'regression') x$forest$rightDaughter[,i] else
      x$forest$treemap[,2,i],
    # random forest cannot handle missing values
    missing    = -1,
    splitVar   = x$forest$bestvar[,i],
    splitVal   = x$forest$xbestsplit[,i],
    nodeStatus = ifelse(x$forest$nodestatus[,i] == -1, -1, 0),
    nodePred   = x$forest$nodepred[,i]
  )
}

# map gbm forest tree to timbr tree
timbr.gbm <- function(x, i, dendrology=NULL) {
  if (is.null(dendrology)) dendrology <- dendrology(x)
  
  varTypes <- dendrology$varTypes
  
  # look at each split var -- if factor, replace split val with c.splits
  # c.splits with values of -1 go left, values of 1 go right
  # in randomForest 1 goes left and 0 goes right
  splitVar <- ifelse(x$trees[[i]][[1]] != -1, x$trees[[i]][[1]] + 1, 0)
  splitVal <- ifelse(x$trees[[i]][[1]] != -1, x$trees[[i]][[2]], 0)  
  sapply(seq_along(splitVar), function(i) {
    if (splitVar[i] > 0) {
      if (varTypes[splitVar[i]] == 'Factor') {
        splitVal[i] <<- sum(sapply(which(x$c.splits[[splitVal[i]+1]] == -1) - 1,
                            function(x) 2^x))
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
    dendrology = dendrology,
    class = 'timbr')
}




for(x in 1:mod$n.trees) {
  tryCatch(timbr(mod, x), warning = function(w) print(x))
}




# sapply(which(intToBits(11) == 1) - 1, function(x) 2^x)