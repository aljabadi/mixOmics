
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic

## Not run: 
set.seed(42)
system.time({
    tune.cor = tune.spls(X, Y, ncomp=2, test.keepX = seq(2,10,2),test.keepY = seq(2,10,2), measure.tune = "cor", method = 'spls',
                         nrepeat=3, progressBar = TRUE, folds =3)
})

system.time({
    tune.cor = tune.spls(X, Y, ncomp=2, test.keepX = seq(2),test.keepY = seq(2,4,2), measure.tune = "cor", method = 'spls',
                         nrepeat=8, progressBar = TRUE, folds =3, BPPARAM = BiocParallel::MulticoreParam(workers = 1))
})

microbenchmark::microbenchmark(
    Serial_Param = tune.spls(X, Y, ncomp=2, test.keepX = seq(2),test.keepY = seq(2,4,2), measure.tune = "cor", method = 'spls',
                             nrepeat=8, progressBar = TRUE, folds =3, BPPARAM = BiocParallel::MulticoreParam(workers = 1)),
    Multicore_Param = tune.spls(X, Y, ncomp=2, test.keepX = seq(2),test.keepY = seq(2,4,2), measure.tune = "cor", method = 'spls',
                                nrepeat=8, progressBar = TRUE, folds =3, BPPARAM = BiocParallel::MulticoreParam(workers = 4))
, times = 1)

system.time({
    tune.cor = tune.spls(X, Y, ncomp=2, test.keepX = seq(2,4,2),test.keepY = seq(2,4,2), measure.tune = "cor", method = 'spls',
                         nrepeat=8, progressBar = TRUE, folds =3, BPPARAM = BiocParallel::MulticoreParam(workers = 1))
})