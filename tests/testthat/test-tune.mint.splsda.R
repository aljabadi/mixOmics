context("test-tune.mint.splsda")

test_that("tune.mint.splsda works", code = {
    data(stemcells)
    data = stemcells$gene
    type.id = stemcells$celltype
    exp = stemcells$study
    test.keepX = c(20, 40)
    
    RNGversion(.mixo_rng())
    
    set.seed(42)
    out = tune.mint.splsda(
        X = data,
        Y = type.id,
        ncomp = 2,
        near.zero.var = FALSE,
        study = exp,
        test.keepX = test.keepX
    )
    expect_is(out, "tune.mint.splsda")
    expect_equal(out$choice.ncomp$ncomp, 1)
    errs <- signif(unname(out$error.rate[,1]), 2)
    dput(errs)
    expect_equal(errs, c(0.39, 0.39))
})

test_that("tune.mint.splsda works with custom alpha", code = {
    data(stemcells)
    data = stemcells$gene
    type.id = stemcells$celltype
    exp = stemcells$study
    test.keepX = c(20, 40)
    
    RNGversion(.mixo_rng())
    
    set.seed(42)
    out = tune.mint.splsda(
        X = data,
        Y = type.id,
        ncomp = 2,
        near.zero.var = FALSE,
        study = exp,
        test.keepX = test.keepX,
        signif.threshold = 0.9
    )
    expect_is(out, "tune.mint.splsda")
    errs <- signif(unname(out$error.rate[,1]), 2)
    expect_equal(errs, c(0.39, 0.39))
    expect_equal(out$choice.ncomp$ncomp, 2)
})
