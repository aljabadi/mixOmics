load_mixo <- function(which = c('devel', 'stable'),
                      devel_path = '/Users/alabadi/Projects/dev/R/_work/mixOmics/mixOmics_ajabadi/mixOmics_ajabadi',
                      stable_path = '/Library/Frameworks/R.framework/Versions/4.0/Resources/library')
{
    which <- match.arg(which)
    if (which == 'devel')
    {
        cat("loading devel from branch: ", system("git rev-parse --abbrev-ref HEAD", intern = TRUE))
        if (!requireNamespace('mixOmics'))
            detach("package:mixOmics", unload = TRUE)
        
        suppressMessages(devtools::load_all(devel_path))
    } else 
    {
        cat("loading stable version: ", as.character(packageVersion('mixOmics')))
        if (!requireNamespace('mixOmics'))
            devtools::unload(devel_path)
        suppressMessages(library(mixOmics, lib.loc = stable_path))
    }
    invisible(TRUE)
}

load_mixo(which = 'devel')
load_mixo(which = 'stable')

data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic
y <- liver.toxicity$clinic[, "ALB.g.dL."]


## ----pls1-Q2, fig.cap='(ref:pls1-Q2)'----------------
pls1.liver <- pls(X = X, Y = y, ncomp = 4, mode = 'regression')
pls2.liver <- pls(X = X, Y = Y, ncomp = 4, mode = 'regression')
spls1.liver <- spls(X = X, Y = y, keepX = rep(10, 4), ncomp = 4, mode = 'regression')
spls2.liver <- spls(X = X, Y = Y, keepX = rep(10, 4), ncomp = 4, mode = 'regression')

perf.wrapper <- function(obj) {
    perf(obj, validation = 'Mfold', folds = 3, nrepeat = 3, progressBar = TRUE)
}
perf.pls1.liver  = perf.wrapper(pls1.liver )
perf.pls2.liver  = perf.wrapper(pls2.liver )
perf.spls1.liver = perf.wrapper(spls1.liver)
perf.spls2.liver = perf.wrapper(spls2.liver)

debugonce(plot)
plot(perf.pls1.liver , criterion = 'Q2')
plot(perf.pls2.liver , criterion = 'Q2')
plot(perf.spls1.liver, criterion = 'Q2')
plot(perf.spls2.liver, criterion = 'Q2')

plot(perf.pls1.liver , criterion = 'Q2.total')
plot(perf.pls2.liver , criterion = 'Q2.total')
plot(perf.spls1.liver, criterion = 'Q2.total')
plot(perf.spls2.liver, criterion = 'Q2.total')

# tune.spls1.liver <- spls(X = X, Y = y, keepX = rep(10, 4), keepY = rep(10, 4), ncomp = 4, mode = 'regression')
set.seed(33)  # for reproducibility with this handbook, remove otherwise
debugonce(perf)
Q2.pls1.liver <- perf(tune.pls1.liver, validation = 'Mfold', folds = 3, nrepeat = 4, progressBar = TRUE)
Q2.spls1.liver <- perf(tune.spls1.liver, validation = 'Mfold', folds = 3, nrepeat = 4, progressBar = TRUE)
Q2.pls1.liver
Q2.spls1.liver
plot(Q2.pls1.liver, criterion = 'Q2')
plot(Q2.spls1.liver, criterion = 'cor.tpred')
plot(Q2.spls1.liver, criterion = 'cor.upred')
plot(Q2.spls1.liver, criterion = 'RSS.upred')
plot(Q2.spls1.liver, criterion = 'RSS.tpred')
plot(Q2.pls1.liver, criterion = 'RSS.tpred')
plot(Q2.pls1.liver, criterion = 'Q2')


class(result) = c("perf", paste(c("perf", method), collapse = "."))