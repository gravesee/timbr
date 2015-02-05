# let's start with a randomForest interface
library(randomForest)
library(gbm)

data(titanic, package='mjollnir')

titanic[is.na(titanic$Age), 'Age'] <- median(titanic$Age, na.rm=T)
titanic[, 'Pclass'] <- ordered(titanic$Pclass)

# build a model predicting mpg
rf <- randomForest(titanic[,-1], factor(titanic[,1]), ntree = 1000, maxnodes = 4)
rf <- randomForest(mtcars[,-1], mtcars[,1], ntree = 1000, maxnodes = 4)
mod <- gbm.fit(titanic[,-1], titanic[,1], n.tree = 1000, n.minobsinnode = 5, bag.fraction = 1,
               distribution='bernoulli', interaction.depth=2)

#TODO: add RPART model

# pretty print the trees
getTree(rf, 1)
pretty.gbm.tree(mod, 6)

# sapply(harvest(timbr(mod, 6))[-1], function(x) sum(x(titanic[,-1])))
# phat <- predict(mod, titanic[,-1], n.trees=1:10, single.tree=TRUE)
# table(phat[,6])
