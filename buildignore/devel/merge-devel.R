## 
## vignette build is breaking
## it is due to changes to devel and Handbook
## the Hanbook changes did create a successful handbook build
## but it seems like the same changes are breaking the vignette
## we should gather all these changes and build the Handbook and the package at the same time on every change
## we should also increase the examples / unit tests
## then we can push to orihin and upstream
## we can also modularise the handbook changes
list.keepX <- c(2:10, 15, 20)
# tuning based on correlations
set.seed(30) # for reproducbility in this vignette, otherwise increase nrepeat
tune.spls.cor <- tune.spls(X, Y, ncomp = 3,
                           test.keepX = list.keepX,
                           validation = "Mfold", folds = 5,
                           nrepeat = 10, progressBar = FALSE,
                           measure = 'cor')