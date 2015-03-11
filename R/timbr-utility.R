# function turning number to 'binary' vector
toBinary <- function(y) {
  stopifnot(length(y) == 1, mode(y) == 'numeric')
  q1  <- (y / 2) %/% 1
  r   <- y - q1 * 2
  res <- c(r)
  while (q1 >= 1) {
    q2 <- (q1 / 2) %/% 1
    r  <- q1 - q2 * 2
    q1 <- q2
    res <- c(res, r)
  }
  res
}

# function turning vector of bits to number
fromBinary <- function(v) {
  sum(sapply(which(v == 1) - 1, function(x) 2^x))
}

# combine functions
combineFuncs <- function(f1, f2) {  
  if (is.null(f1)) f2 else function(x) f1(x) & f2(x)
}

# returns a length 2 vector of [parentID, dir]
# 1 - Left, 2 - Right, 3 - Missing
findLineage <- function(x, n) {
  which(x[,1:3] == n, arr.ind = TRUE)
}

#' Impute missing values 
#' 
#' Naive impuation using mode for factors and median for numerics. The function
#' returns a modified \code{data.frame} where missing values have been replaced
#' with modes for factors and medians for numerics. The returned object has the
#' imputed values added as the attribute "imputed".
#' 
#' @param x a data.frame object
#' @return a \code{data.frame} object with an appended \code{imputed} class and
#' attribute.
#' @export imputeMissing
imputeMissing <- function(x) {
  impute.values <- list()
  
  impute <- function(x) {
    # impute the mode
    if (class(x) == 'factor') {
      val <- levels(x)[which.max(tabulate(x))]
    } else {
      val <- median(x, na.rm=T)
    }
    x[is.na(x)] <- val
    impute.values[[length(impute.values) + 1]] <<- val
    x
  }
  
  df <- data.frame(lapply(x, impute))
  names(impute.values) <- colnames(df)
  structure(df, imputed = impute.values, class = c('data.frame', 'imputed'))
}

#' Get duplicate nodes from LumberYard object
#' 
#' Returns the index positions of nodes with identical node-text. Much faster
#' and memory efficient than running \code{which(duplicated(t(nodes))}.
#' 
#' @param x a \code{\link{lumberYard}}
#' @return a vector of boolean values the same length as the number of nodes in
#' the \code{lumberYard}.
#' 
#' @export duplicateNodes
duplicateNodes <- function(ly) {
  nodeText <- lapply(ly, function(x) lapply(x, '[[', 'nodeText'))
  duplicated(unlist(nodeText))
}