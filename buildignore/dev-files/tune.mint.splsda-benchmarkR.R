test.keepX = seq(1, 101, 4)
mb_tune.mint <- microbenchmark::microbenchmark(

    serial = tune.mint.splsda(
        X = data,
        Y = type.id,
        ncomp = 2,
        near.zero.var = FALSE,
        study = exp,
        test.keepX = test.keepX
    ),
    parallel = tune.mint.splsda(
        X = data,
        Y = type.id,
        ncomp = 2,
        near.zero.var = FALSE,
        study = exp,
        test.keepX = test.keepX, 
        cpus = 4
    ),
    times = 3
)