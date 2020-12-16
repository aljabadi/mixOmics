suppressMessages(devtools::load_all('/Users/alabadi/Projects/dev/R/_work/mixOmics/mixOmics_ajabadi/mixOmics_ajabadi'))
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic
# for pls1
y <- liver.toxicity$clinic[, "ALB.g.dL."]
folds <- 5
tune.pls1.liver <- pls(X = X, Y = y, ncomp = 4, mode = 'regression')
tune.pls2.liver <- pls(X = X, Y = Y, ncomp = 4, mode = 'regression')
# This code may take several min to run, parallelisation is possible
list.keepX <- c(5, 15)
list.keepY <- c(3, 8)


#' # PLS2

#' ### nrepeat > 2

set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls2.liver <- tune.spls(X, Y, test.keepX = list.keepX, 
                             test.keepY = list.keepY, ncomp = 2, 
                             nrepeat = 3, folds = 3, 
                             mode = 'regression', measure = 'cor')

#' #### print tune.pls2

tune.spls2.liver$measure.pred$mean.u <- sapply(tune.spls2.liver$measure.pred$value.u, function(x) {
    if (!any(is.na(x))) mean(x) else NA_real_
})
tune.spls2.liver$measure.pred$sd.u <- sapply(tune.spls2.liver$measure.pred$value.u, function(x) {
    if (!any(is.na(x))) sd(x) else NA_real_
})

#' #### plot tune.pls2

plot(tune.spls2.liver)
