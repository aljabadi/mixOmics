suppressMessages(devtools::load_all('/Users/alabadi/Projects/dev/R/_work/mixOmics/mixOmics_ajabadi/mixOmics_ajabadi'))
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic
# for pls1
y <- liver.toxicity$clinic[, "ALB.g.dL."]
folds <- 5
tune.pls1.liver <- pls(X = X, Y = y, ncomp = 4, mode = 'regression')
tune.pls2.liver <- pls(X = X, Y = Y, ncomp = 4, mode = 'regression')

#' # PLS1

#' ### nrepeat > 2

set.seed(33)
nrep <- 3
Q2.pls1.liver <- perf(tune.pls1.liver, validation = 'Mfold', 
                      folds = folds, nrepeat = nrep)
#' #### print pls1
Q2.pls1.liver

#' #### plot pls1
plot(Q2.pls1.liver, criterion = 'RMSEP', sd = TRUE)
plot(Q2.pls1.liver, criterion = 'Q2', sd = TRUE)
plot(Q2.pls1.liver, criterion = 'Q2.total', sd = TRUE)
plot(Q2.pls1.liver, criterion = 'cor.tpred', sd = TRUE)

#' ### nrepeat <= 2

set.seed(33)
nrep <- 1
Q2.pls1.liver.rep1 <- perf(tune.pls1.liver, validation = 'Mfold', 
                      folds = folds, nrepeat = nrep)
plot(Q2.pls1.liver.rep1, criterion = 'RMSEP', sd = TRUE)
plot(Q2.pls1.liver.rep1, criterion = 'Q2')
plot(Q2.pls1.liver.rep1, criterion = 'Q2.total')
plot(Q2.pls1.liver.rep1, criterion = 'Q2.total', title = 'Q2 Total')
plot(Q2.pls1.liver.rep1, criterion = 'Q2.total', title = ' ') 
plot(Q2.pls1.liver, criterion = 'cor.tpred') ## error

#' # PLS2

#' ### nrepeat > 2

set.seed(33)
nrep <- 3
Q2.pls2.liver <- perf(tune.pls2.liver, validation = 'Mfold', 
                      folds = folds, nrepeat = nrep)
debugonce(plot)
plot(Q2.pls2.liver, criterion = 'RMSEP', sd = TRUE)

#' #### print pls2
Q2.pls2.liver

#' #### pplot pls2
plot(Q2.pls2.liver, criterion = 'Q2', sd = TRUE, cex = 1.3)
plot(Q2.pls2.liver, criterion = 'Q2.total', sd = TRUE) 
plot(Q2.pls2.liver, criterion = 'cor.tpred', sd = TRUE)

#' ### nrepeat <= 2
set.seed(33)
nrep <- 1
Q2.pls2.liver.rep1 <- perf(tune.pls2.liver, validation = 'Mfold', 
                      folds = folds, nrepeat = nrep)
plot(Q2.pls2.liver.rep1, criterion = 'RMSEP', sd = TRUE)
plot(Q2.pls2.liver.rep1, criterion = 'Q2')
plot(Q2.pls2.liver.rep1, criterion = 'Q2.total')
plot(Q2.pls2.liver.rep1, criterion = 'cor.tpred')

#' # plot args
plot(Q2.pls2.liver.rep1, criterion = 'Q2.total')
plot(Q2.pls2.liver.rep1, criterion = 'Q2.total', pch = 16, pch.size = 3, col = color.mixo(2))

#' ### other customisation examples
plot(Q2.pls2.liver.rep1, criterion = 'Q2.total') +
    geom_text(x = 1, y = 0.0975, label = 'Q2 = 0.0975', hjust=-0.5, vjust=-0.5) 