# 
# Author: csoares
###############################################################################

library(recommenderlab)

###
### get data
### 

data(Jester5k)
Jester5k

###
### modeling
###

# popular items
rec.model <- Recommender(Jester5k[1:1000], method="UBCF")
rec.model

names(getModel(rec.model))
getModel(rec.model)$topN

# make recommendations with popular model
rec.preds <- predict(rec.model, Jester5k[1001:1002], n=5)
rec.preds
as(rec.preds, "list")

rec.preds.top3 <- bestN(rec.preds, n=3)
rec.preds.top3 
as(rec.preds.top3, "list")

###
### evaluation
###

# define evaluation schema
eval <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, given=15, goodRating=5)
eval

# learn models
rec.model1 <- Recommender(getData(eval, "train"), "UBCF")
rec.model1

rec.model2 <- Recommender(getData(eval, "train"), "IBCF")
rec.model2

# make predictions
rec.preds1 <- predict(rec.model1, getData(eval, "known"), type="ratings")
rec.preds1

rec.preds2 <- predict(rec.model2, getData(eval, "known"), type="ratings")
rec.preds2

# evaluate predictions
error <- rbind(
		UBCF = calcPredictionAccuracy(rec.preds1, getData(eval, "unknown")),
		IBCF = calcPredictionAccuracy(rec.preds2, getData(eval, "unknown"))
)
error

###
### compare algorithms (recommendations)
###

# define evaluation schema
set.seed(2016)
eval <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, k=1, given=-5, goodRating=5)
eval

# run experiment
algorithms <- list(
		"random items" = list(name="RANDOM", param=NULL),
		"popular items" = list(name="POPULAR", param=NULL),
		"user-based CF" = list(name="UBCF", param=list(nn=50)),
		"item-based CF" = list(name="IBCF", param=list(k=50)),
		"SVD approximation" = list(name="SVD", param=list(k = 50))
)

# analyze results
results <- evaluate(eval, algorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))
results

names(results)
results[["user-based CF"]]

plot(results, annotate=c(1,3), legend="bottomright")
plot(results, "prec/rec", annotate=3, legend="topleft")

###
### compare algorithms (ratings)
###

results <- evaluate(eval, algorithms, type="ratings")
results
plot(results, ylim=c(0,100))

###
### compare algorithms (binary data)
###

# binarize data and choose users with 20+ ratings
Jester_b <- binarize(Jester5k, minRating=5)
Jester_b_20p_ratings <- Jester_b[rowCounts(Jester_b) > 20]
Jester_b_20p_ratings

# define evaluation schema
eval_b <- evaluationScheme(Jester_b_20p_ratings[1:1000], method="split", train=0.9, k=1, given=3)
eval_b

# run experiment
results_b <- evaluate(eval_b, algorithms, type="topNList", n=c(1,3,5,10,15,20))

# analyze results
plot(results_b, annotate=c(1,3), legend="topright")

