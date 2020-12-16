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

#' # PLS1

#' ### nrepeat > 2
nrep <- 3
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls1.liver <- tune.spls(X, y, test.keepX = list.keepX, 
                              test.keepY = list.keepY, ncomp = 2, 
                              nrepeat = nrep, folds = 3, 
                              mode = 'regression', measure = 'MSE')
class(tune.spls1.liver)
#' #### print tune.pls1
tune.spls1.liver

#' #### plot tune.pls1
plot(tune.spls1.liver)

#' #### invalid measure
tune.spls1.liver <- tune.spls(X, y, test.keepX = list.keepX, 
                              test.keepY = list.keepY, ncomp = 2, 
                              nrepeat = nrep, folds = 3, 
                              mode = 'regression', measure = 'cor')

#' ### nrepeat <= 2
nrep <- 1
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls1.liver.nrep1 <- tune.spls(X, y, test.keepX = list.keepX, 
                                    test.keepY = list.keepY, ncomp = 2, 
                                    nrepeat = nrep, folds = 3, 
                                    mode = 'regression', measure = 'MAE')

#' #### print tune.pls2
tune.spls1.liver.nrep1

#' #### plot tune.pls2
plot(tune.spls1.liver.nrep1)

#' ### mode='canonical'
nrep <- 3
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls1.liver.canonical <- tune.spls(X, y, test.keepX = list.keepX, 
                                        test.keepY = list.keepY, ncomp = 2, 
                                        nrepeat = nrep, folds = 3, 
                                        mode = 'canonical', measure = 'Bias')

#' #### print tune.pls2
tune.spls1.liver.canonical

#' #### plot tune.pls2
plot(tune.spls1.liver.canonical)


#' # PLS2

#' ### nrepeat > 2
nrep <- 3
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls2.liver <- tune.spls(X, Y, test.keepX = list.keepX, 
                             test.keepY = list.keepY, ncomp = 2, 
                             nrepeat = nrep, folds = 3, 
                             mode = 'regression', measure = 'cor')

#' #### print tune.pls2
tune.spls2.liver

#' #### plot tune.pls2
plot(tune.spls2.liver)


#' ### nrepeat <= 2
nrep <- 1
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls2.liver.nrep1 <- tune.spls(X, Y, test.keepX = list.keepX, 
                              test.keepY = list.keepY, ncomp = 2, 
                              nrepeat = nrep, folds = 3, 
                              mode = 'regression', measure = 'cor')

#' #### print tune.pls2
tune.spls2.liver.nrep1

#' #### plot tune.pls2
plot(tune.spls2.liver.nrep1)

#' ### mode='canonical'
nrep <- 3
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls2.liver.canonical <- tune.spls(X, Y, test.keepX = list.keepX, 
                                        test.keepY = list.keepY, ncomp = 2, 
                                        nrepeat = nrep, folds = 3, 
                                        mode = 'canonical', measure = 'cor')

#' #### print tune.pls2
tune.spls2.liver.canonical

#' #### plot tune.pls2
plot(tune.spls2.liver.canonical)
