# function turning number to binar vector
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