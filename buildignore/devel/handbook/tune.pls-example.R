data(liver.toxicity)
X <- liver.toxicity$gene[,1:200]
Y <- liver.toxicity$clinic
set.seed(42)
tune.res = tune.pls( X, Y, ncomp = 3,
                       measure.tune = "Q2",
                      folds = 5, nrepeat = 3, progressBar = TRUE)
tune.res$choice.ncomp
tune.res$choice.keepX
tune.res$choice.keepY
# plot the results
plot(tune.res)