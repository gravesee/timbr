\name{timbr-package}
\alias{timbr-package}
\alias{timbr}
\docType{package}
\title{
Unified interface to R's decistion tree models (short line)
}
\description{
timbr provides a unified interface to R's many decision tree models. Functions
written to operate on this interface can be used interchangeably regardless of
the underlying decision tree algorithm. A function extracting the nodes of a
random forest can be used on a gradient boosted model without writing code
customized to either model.
}
\details{
\tabular{ll}{
Package: \tab timbr\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2015-02-04\cr
License: \tab Proprietary\cr
}
~~ An overview of how to use the package, including the most important functions ~~
TODO
}
\author{
Eric E. Graves

Maintainer: Eric E. Graves <gravcon5@gmail.com>
}
\examples{
library(randomForest)
rf <- randomForest(mtcars[-1], mtcars[1])
ly <- lumbarYard(rf)
predict(ly, mtcars[-1])
}
