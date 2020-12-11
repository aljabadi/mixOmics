#' Tuning functions for PLS and sPLS models
#' 
#' Computes M-fold or Leave-One-Out Cross-Validation scores on a user-input
#' grid to determine optimal values for the sparsity parameters in \code{spls}.
#' 
#' These functions can be used to tune the parameters in the
#' \code{spls} as well as \code{pls} functions (if the the number of selected variables are not given). (number of components and the number of variables in
#' \code{keepX} to select). If the the number of selected variables are not given
#' @template X
#' @param Y numeric vector or matrix of continuous
#'   responses (for multi-response models) \code{NA}s are allowed.
#' @param test.keepX numeric vector for the different number of variables to
#'   test from the \eqn{X} data set. If NULL (default), a \code{pls} model is
#'   tuned.
#' @param test.keepY numeric vector for the different number of variables to
#'   test from the \eqn{Y} data set. If NULL (default), a \code{pls} model is
#'   tuned.
#' @template ncomp
#' @template nrepeat
#' @template folds
#' @param mode Character string. What type of algorithm to use, (partially)
#'   matching one of \code{"regression"}, \code{"canonical"}, or
#'   \code{"classic"}. Note that \code{mode = 'invariant'} is not suuported. See
#'   \code{\link{spls}} for details on modes.
#' @param measure.tune The measure to optimise for tuning for \code{spls}
#'   models. One of \code{c('cor', 'RSS')}. See details.
#' @template BPPARAM
#' @template progressBar
#'
#' @return A list.
#' @export
#'
#' @examples
#' TODO
tune.spls <- 
    function(X,
             Y,
             method = c('pls', 'spls'),
             test.keepX = NULL,
             test.keepY = NULL,
             ncomp,
             nrepeat,
             folds = 10,
             mode = c('regression', 'canonical', 'classic'),
             measure.tune = if (method == 'pls') NULL else c('cor', 'RSS'),
             BPPARAM = BiocParallel::SerialParam(),
             progressBar = FALSE
    ) {
        
        
        out = list()
        mode <- match.arg(mode)
        
        method <- match.arg(method)
        spls.model <- (method == 'spls')
        
        test.keepX <- .change_if_null(arg = test.keepX, default = ncol(X))
        test.keepY <- .change_if_null(arg = test.keepY, default = ncol(Y))
        
        test.keepX <- unique(test.keepX)
        test.keepY <- unique(test.keepY)
        
        progressBar.spls <- progressBar & (spls.model)
        progressBar.pls <- progressBar & (!spls.model)
        comps <- seq_len(ncomp)
        if (spls.model) {
            # TODO check test.keepX and test.keepY
            measure.tune <- match.arg(measure.tune, choices = c('cor', 'RSS'))
            
        }else{
            measure.tune <- NULL
            if ((test.keepX != ncol(X)) | (test.keepY != ncol(Y)))
                stop("'test.keepX' and 'test.keepY' can only be provided with method = 'spls'", call. = FALSE)
            comps <- seq_len(ncomp)
            test.keepX <- ncol(X)
            test.keepY <- ncol(Y)
            
        }
        
        cor.tpred <- 
            cor.upred <- 
            RSS.tpred <- 
            RSS.upred <- 
            array(dim      =    c(length(test.keepX), 
                                  length(test.keepY), 
                                  nrepeat),
                  dimnames = list(paste0('keepX_', test.keepX),
                                  paste0('keepY_', test.keepY),
                                  paste0('repeat_', seq_len(nrepeat))))
        
        Q2.tot.ave = matrix(nrow = ncomp, ncol = nrepeat,
                            dimnames = list(paste0('comp_', seq_len(ncomp)), paste0('repeat_', seq_len(nrepeat))))
        
        
        choice.keepX = choice.keepY = NULL
        # TODO store values in a tidy format such as below -- unify pls and spls
        eg <- expand.grid(comp = seq_len(ncomp), keepX = test.keepX,
                          keepY = test.keepY, rep = seq_len(nrepeat),
                          measure = c('cor', 'RSS'), variate = c('u', 't'), value = NA)
        
        cor.pred = RSS.pred = list()
        # .tune.spls.repeat <- function(test.keepX, test.keepY, X, Y, choice.keepX, choice.keepY, comp, mode, validation, folds)
        # {
        #     out <- list()
        #     for(keepX in 1:length(test.keepX)){
        #         for(keepY in 1:length(test.keepY)){
        #             # sPLS model, updated with the best keepX
        #             pls.res = spls(X = X, Y = Y, 
        #                            keepX = c(choice.keepX, test.keepX[keepX]), 
        #                            keepY = c(choice.keepY, test.keepY[keepY]), 
        #                            ncomp = comp, mode = mode)
        #             # fold CV
        #             res.perf <- .perf.mixo_pls_cv(pls.res, validation = 'Mfold', folds = folds)
        #             out[[paste0('keepX_', keepX)]][[paste0('keepY_', keepY)]] <- list(
        #                 t.pred.cv = res.perf$t.pred.cv,
        #                 u.pred.cv = res.perf$u.pred.cv,
        #                 X.variates =  pls.res$variates$X,
        #                 Y.variates =  pls.res$variates$Y,
        #                 Q2.total = res.perf$Q2.total
        #             )
        #         }
        #     }
        #     out
        # }
        
        for (comp in comps){
            if (progressBar) {
                cat( sprintf("\ntuning component: %s\n", comp))
                pb <- txtProgressBar(min = 0, max = length(test.keepX)*length(test.keepY), style = 3)
            }
            
            # cv.repeat.res <- bplapply(seq_len(nrepeat), 
            #                         FUN = function(k){ 
            #                             if (use_progressBar) {
            #                                 setTxtProgressBar(pb = pb, value = k)
            #                             }
            #                             .tune.spls.repeat(test.keepX = test.keepX, test.keepY = test.keepY, X = X, Y = Y, 
            #                                               choice.keepX = choice.keepX, choice.keepY = choice.keepY, comp = comp, 
            #                                               mode = mode, validation = 'Mfold', folds = folds)}, BPPARAM = BPPARAM)
            
            # cv.repeat.res <- list()
            progress_count <- 0
            for(keepX in 1:length(test.keepX)){
                for(keepY in 1:length(test.keepY)){
                    if (progressBar.spls) {
                        progress_count <- progress_count + 1
                        setTxtProgressBar(pb = pb, value = progress_count)
                    }
                    
                    pls.res = spls(X = X, Y = Y, 
                                   keepX = c(choice.keepX, test.keepX[keepX]), 
                                   keepY = c(choice.keepY, test.keepY[keepY]), 
                                   ncomp = comp, mode = mode)
                    res.perf <- perf.mixo_spls(pls.res, validation = 'Mfold', 
                                               folds = folds, progressBar = progressBar.pls, 
                                               nrepeat = nrepeat, BPPARAM = BPPARAM)
                    
                    for(k in seq_len(nrepeat)){
                        ## what should we do here now that nrepeat is used?
                        t.pred.cv <-  res.perf$t.pred.cv[,k]
                        u.pred.cv <-  res.perf$u.pred.cv[,k]
                        X.variates <-   pls.res$variates$X
                        Y.variates <-   pls.res$variates$Y
                        Q2.total <- res.perf$Q2.total
                        
                        # if (spls.model)
                        # {
                        # extract the predicted components: 
                        # if(measure.tune == 'cor' ){
                        cor.tpred[keepX, keepY, k] = cor(t.pred.cv[, comp], X.variates[, comp])
                        cor.upred[keepX, keepY,k] = cor(u.pred.cv[, comp], Y.variates[, comp])
                        # }
                        # if(measure.tune == 'RSS'){
                        # RSS: no abs values here
                        RSS.tpred[keepX, keepY, k] = sum((t.pred.cv[, comp] - X.variates[, comp])^2)/(nrow(X) -1) 
                        RSS.upred[keepX, keepY, k] = sum((u.pred.cv[, comp] - Y.variates[, comp])^2)/(nrow(X) -1)
                        # }
                        # covariance between predicted variates
                        ##cov.variate.pred[keepX, keepY, k] = cov(t.pred.cv[, comp], u.pred.cv[, comp])
                        #     
                        # } else {
                        #     
                        # extract Q2.total for a PLS, we could extract other outputs such as R2, MSEP etc (only valid for regression)
                        Q2.tot.ave[, k] = Q2.total 
                        #     
                        #     # extract the predicted components per dimension, take abs value
                        #     cor.tpred[, k] = diag(abs(cor(t.pred.cv, X.variates)))
                        #     cor.upred[, k] = diag(abs(cor(u.pred.cv, Y.variates)))
                        #     
                        #     # RSS: no abs values here
                        #     RSS.tpred[, k] = apply((t.pred.cv - X.variates)^2, 2, sum)/(nrow(X) -1)
                        #     RSS.upred[, k] = apply((u.pred.cv - Y.variates)^2, 2, sum)/(nrow(X) -1)
                        # }
                        
                    }  # end repeat
                } #end keepY
            } # end keepX
            cat('\t')
            
            ## add mean and sd across repeats for output
            .get_mean_and_sd <- function(arr) {
                list(values = arr, 
                     mean   = apply(arr, c(1,2), mean), 
                     sd     = apply(arr, c(1,2), sd))
            } 
            cor.pred$u[[paste0('comp_', comp)]] = .get_mean_and_sd(cor.upred)
            cor.pred$t[[paste0('comp_', comp)]] = .get_mean_and_sd(cor.tpred)
            RSS.pred$u[[paste0('comp_', comp)]] = .get_mean_and_sd(RSS.upred)
            RSS.pred$t[[paste0('comp_', comp)]] = .get_mean_and_sd(RSS.tpred)
            
            t.test.arr <- function(arr, is_cor) {
                
                .atanh.transform <- function(x) {
                    out <- atanh(x)
                    out <- apply(out, c(1,2,3), function(x) {
                        if (x > 0.99)
                            return(x + runif(1)/1e4) ## avoid constant vector into t.test
                        return(x)
                    })
                    
                    out
                }
                if (is_cor) {
                    # arr <- .atanh.transform(arr)
                } else {
                    arr <- -arr
                }
                choice.keepX_i <- 1
                choice.keepY_j <- 1
                for (keepX_i in seq_len(dim(arr)[1])[-1]) {
                    for (keepY_j in seq_len(dim(arr)[2])[-1])
                    {
                        x <- arr[choice.keepX_i, choice.keepY_j, ]
                        y <- arr[keepX_i, keepY_j, ]
                        if (is_cor &
                            mean(x) < atanh(0.98))
                            ## how can we skip if already too well?
                        {
                            t.test.res <- t.test(x,
                                                 y,
                                                 alternative = 'less',
                                                 paired = FALSE)
                            if (t.test.res$p.value < 0.05)
                            {
                                choice.keepX_i <- keepX_i
                                choice.keepY_j <- keepY_j
                            }
                        }
                        
                    }
                }
                return(c(ind.choice.keepX = choice.keepX_i, 
                         ind.choice.keepY = choice.keepY_j))
            }
            
            
            if (spls.model)
            {
                # choose the best keepX and keepY based on type.tune
                if(mode != 'canonical'){  #regression, invariant, classic
                    # define best keepX and keepY based on u
                    if(measure.tune == 'cor'){
                        cor.component = cor.pred$u[[paste0('comp_', comp)]]
                        # index = which(cor.component == max(cor.component), arr.ind = TRUE)
                        index <- t.test.arr(arr = cor.component$values, is_cor = TRUE)
                    }else{ # if type.tune = 'RSS'
                        RSS.component = RSS.pred$u[[paste0('comp_', comp)]]
                        # index = which(RSS.component == min(RSS.component), arr.ind = TRUE)
                        index <- t.test.arr(arr = RSS.component$values, is_cor = FALSE)
                    }
                    choice.keepX = c(choice.keepX, test.keepX[index['ind.choice.keepX']])
                    choice.keepY = c(choice.keepY, test.keepY[index['ind.choice.keepY']])
                    
                }else{  # mode = 'canonical'
                    if(measure.tune == 'cor'){
                        cor.component.t = cor.pred$t[[paste0('comp_', comp)]]
                        cor.component.u = cor.pred$u[[paste0('comp_', comp)]]
                        index.t = t.test.arr(arr = cor.component.t$values, is_cor = TRUE)
                        index.u = t.test.arr(arr = cor.component.u$values, is_cor = TRUE)
                    }else{ # if type.tune = 'RSS'
                        RSS.component.t = RSS.pred$t[[paste0('comp_', comp)]]
                        RSS.component.u = RSS.pred$u[[paste0('comp_', comp)]]
                        index.t = t.test.arr(arr = RSS.component.t$values, is_cor = FALSE)
                        index.u = t.test.arr(arr = RSS.component.u$values, is_cor = FALSE)
                    }
                    choice.keepX = c(choice.keepX, test.keepX[index.t['ind.choice.keepX']])
                    choice.keepY = c(choice.keepY, test.keepY[index.u['ind.choice.keepY']])
                } # canonical
            }
        } # end comp
        if (spls.model)
        {
            out$choice.keepX = choice.keepX
            out$choice.keepY = choice.keepY
        } else
        {
            out$Q2.tot.ave = apply(Q2.tot.ave, 1, mean)
        }
        
        # } # end sPLS
        
        out$cor.pred = cor.pred
        out$RSS.pred = RSS.pred
        
        ## eval args which we methods might need in the call
        asis.args.ind <- match(x = c('X', 'Y', 'BPPARAM'), table = names(formals()))
        mc <- mget(names(formals())[-asis.args.ind], sys.frame(sys.nframe()))
        mc <- as.call(c(as.list(match.call())[asis.args.ind], mc))
        out <- c(list(call = mc), out)
        class(out) <- if (spls.model) c('tune.spls', 'tune.pls') else c('tune.pls')
        return(out)
    } 
