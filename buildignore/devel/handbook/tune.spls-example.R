data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic
set.seed(42)
tune.res = tune.spls( X, Y, ncomp = 3,
                  test.keepX = c(5, 10, 15),
                  test.keepY = c(3, 6, 8), measure = "cor",
                  folds = 5, nrepeat = 3, progressBar = TRUE)
tune.res$choice.ncomp
tune.res$choice.keepX
tune.res$choice.keepY
# plot the results
plot(tune.res)