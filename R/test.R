# # let's start with a randomForest interface
# library(randomForest)
# library(gbm)
# 
# ### Random Forest Left == '<='
# ### GBM Left == '<'
# 
# data(titanic, package='mjollnir')
# 
# titanic[is.na(titanic$Age), 'Age'] <- median(titanic$Age, na.rm=T)
# titanic[, 'Pclass'] <- ordered(titanic$Pclass)
# 
# # build a model predicting mpg
# rf <- randomForest(titanic[,-1], factor(titanic[,1]), ntree = 1000, maxnodes = 4)
# rf <- randomForest(mtcars[,-1], mtcars[,1], ntree = 1000, maxnodes = 4)
# 
# names(titanic) <- NULL
# mod <- gbm.fit(titanic[,-1], titanic[,1], n.tree = 1000, n.minobsinnode = 5, bag.fraction = 0.50,
#                distribution='bernoulli', interaction.depth=3)
# 
# #TODO: add RPART model
# 
# # pretty print the trees
# getTree(rf, 1)
# pretty.gbm.tree(mod, 6)
# 
# # sapply(harvest(timbr(mod, 6))[-1], function(x) sum(x(titanic[,-1])))
# # phat <- predict(mod, titanic[,-1], n.trees=1:10, single.tree=TRUE)
# # table(phat[,6])
# 
# my.pred  <- do.call(cbind, lapply(1:mod$n.trees, function(x) predictTerminalNodes(harvest(timbr(rf, x)), mtcars[,-1])))
# res.node <- do.call(cbind, lapply(1:mod$n.trees, function(x) predictTerminalNodeID(harvest(timbr(rf, x)), titanic[,-1])))
# 
# gbm.pred <- predict(mod, titanic[,-1], 1:mod$n.trees, single.tree = T)
# 
# rf.pred <- predict(rf, mtcars[,-1], predict.all = T)
# 
# phat1 <- apply(res2, 1, sum) + mod$initF
# phat2 <- predict(mod, titanic[,-1], mod$n.trees)
# 
# # find some mismatches
# ids <- which(gbm.pred[,1] != my.pred[,1])
# res.node[ids, 1]
# 
# 
