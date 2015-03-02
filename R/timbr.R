# generic dispatch for custom timbr function
timbr <- function(object, ...) UseMethod("timbr")

# generic dispatch for custom species function
species <- function(object, ...) UseMethod("species")

# standard timbr class membership function
is.timbr <- function(object) {
  res <- FALSE
  if (attr(object, 'class') == 'timbr') res <- TRUE
  res
}