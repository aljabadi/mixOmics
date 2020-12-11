# library(mixOmics)
packageVersion('mixOmics')
#>  ‘6.15.27’
data(liver.toxicity)
X <- liver.toxicity$gene[,1:300]
Y <- liver.toxicity$clinic
y <- liver.toxicity$clinic[, "ALB.g.dL."]
list.keepX <- c(5:10, seq(15, 25, 5))
# list.keepX  # Inspect the keepX grid
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls1.MAE <- tune.spls(X, y, ncomp= 2,
                            test.keepX = list.keepX,
                            validation = 'Mfold',
                            folds = 3,
                            nrepeat = 3, progressBar = TRUE,
                            measure = 'RSS')


plot(tune.spls1.MAE)
plot(tune.spls1.MAE, measure = 'cor')

tune.spls2.MAE <- tune.spls(X, Y, ncomp= 2,
                            test.keepX = list.keepX,
                            test.keepY = c(2,4,7),
                            validation = 'Mfold',
                            folds = 3,
                            nrepeat = 3, progressBar = TRUE,
                            measure = 'RSS')

plot(tune.spls2.MAE)
plot(tune.spls2.MAE, measure = 'cor')
