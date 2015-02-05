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

# make it so get func only needs the tree, dir, node and forest
getFuncText <- function(x, n, dir, species) {
  var <- x[n, 4]
  val <- x[n, 5]  
  
  if (dir == 'm') {
    text <- sprintf("function(x) is.na(x[, %s])", var)
    return(text)
  }
  
  if (species$varTypes[var] == 'Numeric') {
    eq   <- list(l="<=", r=">")
    text <- sprintf("function(x) x[, %s] %s %s & !is.na(x[, %s])",
                    var, eq[[dir]], val, var)
    
  } else {
    eq   <- list(l="", r="!")
    if (species$varTypes[var] == 'Ordered') {

      mask <- rep(0, species$nCat[var])
      mask[1:val] <- 1
      mems <- paste(species$xLevels[[var]][mask], collapse="','")

    } else { # else a factor

      eq   <- list(l="", r="!")
      mems <- paste(species$xLevels[[var]][toBinary(val)], collapse="','")

    }
    text <- sprintf("function(x) %s(x[, %s] %%in%% c('%s')) & !is.na(x[, %s])",
                    eq[[dir]], var, mems, var)
  }
  return(text)
}

# function to unpack a timbr structure
#' @export
harvest <- function(x, i) {
  stopifnot(is.timbr(x))
  
  l <- list() # store the functions in a list to be returned
  species <- attr(x, 'species')
  
  recurse <- function(x, n = 1, fn=NULL) {    
    
    if (x[n, 6] == -1) { # terminal node
      l[[n]] <<- fn
      return(fn)
    } else {
      fl <- eval(parse(text = getFuncText(x, n, 'l', species)))
      fr <- eval(parse(text = getFuncText(x, n, 'r', species)))
      
      l[[x[n, 1]]] <<- recurse(x, n=x[n, 1], fn=combineFuncs(fn, fl))
      l[[x[n, 2]]] <<- recurse(x, n=x[n, 2], fn=combineFuncs(fn, fr))
      
      if (species$missing) {
        fm <- eval(parse(text = getFuncText(x, n, 'm', species)))
        l[[x[n, 3]]] <<- recurse(x, n=x[n, 3], fn=combineFuncs(fn, fm))
      }      
      return(fn)
    }
  }
  recurse(x)
  return(l)
}



