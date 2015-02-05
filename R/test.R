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

# test mod trees
# for(x in 1:mod$n.trees) {
#   tryCatch(timbr(mod, x), warning = function(w) print(x))
# }


# sapply(which(intToBits(11) == 1) - 1, function(x) 2^x)