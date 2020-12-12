#############################################################################################################
# Authors:
#   Florian Rohart, The University of Queensland, The University of Queensland Diamantina Institute, Translational Research Institute, Brisbane, QLD
#   Kim-Anh Le Cao, The University of Queensland, The University of Queensland Diamantina Institute, Translational Research Institute, Brisbane, QLD
#   Benoit Gautier, The University of Queensland, The University of Queensland Diamantina Institute, Translational Research Institute, Brisbane, QLD
#   Francois Bartolo, Institut National des Sciences Appliquees et Institut de Mathematiques, Universite de Toulouse et CNRS (UMR 5219), France
#
# created: 2013
# last modified: 05-10-2017
#
# Copyright (C) 2013
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#############################################################################################################


# ========================================================================================================
# tune.spls: chose the optimal number of parameters per component on a spls method
# ========================================================================================================
# TODO details on nrepeat
# TODO details on pls vs spls
# TODO tidy outputs preferrably in an array
#' Tuning functions for sPLS and PLS functions
#' 
#' @template description/tune
#' 
#' @template section/folds
#' @template section/nrepeat
#' @template section/measure-pls
#' @template section/t-test-process
#' 
#' @section more:
#' See also \code{?perf} for more details.
#' 
#' @inheritParams spls
#' @template arg/test.keepX-X.matrix
#' @template arg/test.keepY
#' @template arg/validation
#' @template arg/folds
#' @template arg/nrepeat
#' @param measure One of c('cor', 'RSS') indicating the tuning measure. See
#'   details.
#' @template arg/progressBar
#' @template arg/BPPARAM
#' @return A list that contains: \item{cor.pred}{The correlation of predicted vs
#'   actual components from X (t) and Y (u) for each
#'   component}\item{RSS.pred}{The Residual Sum of Squares of predicted vs
#'   actual components from X (t) and Y (u) for each component}
#'   \item{choice.keepX}{returns the number of variables selected for X (optimal
#'   keepX) on each component.} \item{choice.keepY}{returns the number of
#'   variables selected for Y (optimal keepY) on each component.}
#'   \item{choice.ncomp}{returns the optimal number of components for the model
#'   fitted with \code{$choice.keepX} and \code{$choice.keepY} } \item{call}{The
#'   functioncal call including the parameteres used.}
#' @author Kim-Anh Lê Cao, Al J Abadi, Benoit Gautier, Francois Bartolo,
#' Florian Rohart,
#' @seealso \code{\link{splsda}}, \code{\link{predict.splsda}} and
#' http://www.mixOmics.org for more details.
#' @references mixOmics article:
#' 
#' Rohart F, Gautier B, Singh A, Lê Cao K-A. mixOmics: an R package for 'omics
#' feature selection and multiple data integration. PLoS Comput Biol 13(11):
#' e1005752
#' 
#' PLS and PLS citeria for PLS regression: Tenenhaus, M. (1998). La regression
#' PLS: theorie et pratique. Paris: Editions Technic.
#' 
#' Chavent, Marie and Patouille, Brigitte (2003). Calcul des coefficients de
#' regression et du PRESS en regression PLS1. Modulad n, 30 1-11. (this is the
#' formula we use to calculate the Q2 in perf.pls and perf.spls)
#' 
#' Mevik, B.-H., Cederkvist, H. R. (2004). Mean Squared Error of Prediction
#' (MSEP) Estimates for Principal Component Regression (PCR) and Partial Least
#' Squares Regression (PLSR). Journal of Chemometrics 18(9), 422-429.
#' 
#' sparse PLS regression mode:
#' 
#' Lê Cao, K. A., Rossouw D., Robert-Granie, C. and Besse, P. (2008). A sparse
#' PLS for variable selection when integrating Omics data. Statistical
#' Applications in Genetics and Molecular Biology 7, article 35.
#' 
#' One-sided t-tests (suppl material):
#' 
#' Rohart F, Mason EA, Matigian N, Mosbergen R, Korn O, Chen T, Butcher S,
#' Patel J, Atkinson K, Khosrotehrani K, Fisk NM, Lê Cao K-A&, Wells CA&
#' (2016). A Molecular Classification of Human Mesenchymal Stromal Cells. PeerJ
#' 4:e1845.
#' @keywords regression multivariate
#' @export
#' @examples
#' 
#' \dontrun{
#' data(liver.toxicity)
#' X <- liver.toxicity$gene
#' Y <- liver.toxicity$clinic
#' set.seed(42)
#' tune.res = tune.spls( X, Y, ncomp = 3,
#'                   test.keepX = c(5, 10, 15),
#'                   test.keepY = c(3, 6, 8), measure = "cor",
#'                   folds = 5, nrepeat = 3, progressBar = TRUE)
#' tune.res$choice.ncomp
#' tune.res$choice.keepX
#' tune.res$choice.keepY
#' # plot the results
#' plot(tune.res)
#' }
# change this so that it simply wraps perf
tune.spls <- 
    function(X,
             Y,
             test.keepX = NULL,
             test.keepY = NULL,
             ncomp,
             validation = c('Mfold', 'loo'),
             nrepeat = 1,
             folds,
             mode = c('regression', 'canonical', 'classic'),
             measure.tune = c('cor', 'RSS'), ## only if spls model
             BPPARAM = SerialParam(),
             progressBar = FALSE
             ) {
        out = list()
        mode <- match.arg(mode)
        
        X <- .check_numeric_matrix(X, block_name = 'X')
        Y <- .check_numeric_matrix(Y, block_name = 'Y')
        check_cv <- .check_cv_args(validation = validation, 
                                   nrepeat = nrepeat, folds = folds, 
                                   N = nrow(X))
        validation <- check_cv$validation
        nrepeat <- check_cv$nrepeat
        folds <- check_cv$folds
        
        test.keepX <- .change_if_null(arg = test.keepX, default = ncol(X))
        test.keepY <- .change_if_null(arg = test.keepY, default = ncol(Y))
        
        test.keepX <- unique(test.keepX)
        test.keepY <- unique(test.keepY)
        
        spls.model <- !(length(test.keepX) == 1 & length(test.keepY) == 1)
        measure.tune <- match.arg(measure.tune, choices = c('cor', 'RSS'))
        
        choice.keepX = choice.keepY = NULL
        measure.table.cols <- c('comp', 'keepX', 'keepY', 'repeat', 't', 'u', 'cor', 'RSS')
        if (spls.model)
        {
          measure.pred <- expand.grid(keepX = test.keepX, 
                                      keepY = test.keepY,
                                      V = c('u', 't'),
                                      measure = c('cor', 'RSS'),
                                      comp = seq_len(ncomp),
                                      optimum = FALSE)
        } else {
          # Q2 only
          measure.pred <- expand.grid(
                                      comp = seq_len(ncomp),
                                      optimum = FALSE)
        }
        measure.pred <- data.frame(measure.pred)
        measure.pred <- cbind(measure.pred, value = I(rep(list(data.frame(matrix(NA_real_, ncol= 1, nrow = nrepeat))), 
                                    times = nrow(measure.pred))))
        # measure.pred$value <- lapply(measure.pred$value, as.data.frame)
        
        use_progressBar <- progressBar & (is(BPPARAM, 'SerialParam'))
        n_keepA <- length(test.keepX) * length(test.keepY)
        
        
        if (spls.model) {
          measure.pred[
            measure.pred$keepX == test.keepX[1] &
              measure.pred$keepY == test.keepY[1]
            ,]$optimum <- TRUE
        } else {
          measure.pred[
            measure.pred$comp == 1
            ,]$optimum <- TRUE
        }

        
            for (comp in seq_len(ncomp)){
              # TODO tune.pls progressBar
                if (use_progressBar) {
                    n_tested <- 0
                    cat(sprintf("\ntuning component: %s\n", comp))
                }
                    for(keepX in 1:length(test.keepX)){
                        for(keepY in 1:length(test.keepY)){
                            if (use_progressBar) {
                                n_tested <- n_tested + 1
                                prog_level <- n_tested / n_keepA
                                .progressBar(prog_level, title = 'of features tested')
                            }
                            pls.model <- spls(X = X, Y = Y, 
                                              keepX = c(choice.keepX, test.keepX[keepX]), 
                                              keepY = c(choice.keepY, test.keepY[keepY]), 
                                              ncomp = comp, mode = mode)
                            pls.perf <- perf(pls.model, validation = validation, folds = folds, nrepeat = nrepeat)
                            
                            for (measure in c('cor', 'RSS', 'Q2.total'))
                            {
                              measure.t <- pls.perf[[paste0(measure,'.tpred')]]
                              # measure.t <- unlist(measure.t)
                              measure.u <- pls.perf[[paste0(measure,'.upred')]]
                              Q2.total <-  pls.perf$Q2.total
                              measure.pred[measure.pred$comp == comp & 
                                             measure.pred$keepX == test.keepX[keepX] &
                                             measure.pred$keepY == test.keepY[keepY] &
                                             measure.pred$V == 't' &
                                             measure.pred$measure == measure.tune
                                           ,]$value[[1]] <- t(data.frame(measure.t))
                              
                              
                              measure.pred[measure.pred$comp == comp & 
                                             measure.pred$keepX == test.keepX[keepX] &
                                             measure.pred$keepY == test.keepY[keepY] &
                                             measure.pred$V == 'u' &
                                             measure.pred$measure == measure.tune
                                           ,]$value[[1]] <- t(data.frame(measure.u))
                              
                              measure.pred[measure.pred$comp == comp & 
                                             measure.pred$keepX == test.keepX[keepX] &
                                             measure.pred$keepY == test.keepY[keepY] &
                                             measure.pred$V == 'u' &
                                             measure.pred$measure == 'Q2_total'
                                           ,]$value[[1]] <- t(data.frame(Q2.total))
                                for (v in c('u', 't'))
                                {
                                  value <- measure.pred[measure.pred$comp == comp & 
                                                          measure.pred$keepX == test.keepX[keepX] &
                                                          measure.pred$keepY == test.keepY[keepY] &
                                                          measure.pred$V == v &
                                                          measure.pred$measure == measure.tune
                                                        ,]$value[[1]]
                                  optimum <- measure.pred[measure.pred$comp == comp & 
                                                            measure.pred$optimum == TRUE &
                                                            measure.pred$V == v &
                                                            measure.pred$measure == measure.tune
                                                          ,]$value[[1]]
         
                                  if (nrepeat > 2) {
                                    cat('value\n')
                                    cat(value)
                                    cat('\n')
                                    cat('optimum\n')
                                    cat(optimum)
                                    cat('\n')
                                    # browser()
                                    t.test.res <- tryCatch(t.test(x = optimum, y = value, alternative = ifelse(measure.tune == 'cor', 'greater', 'less')), error = function(e) e)
                                    if (is (t.test.res, 'error')) browser()
                                    improved <- t.test.res$p.value < 0.05
                                  } else {
                                    improved <- mean(optimum) < mean(value)
                                  }
                                  
                                  if (improved)
                                  {
                                    measure.pred[measure.pred$comp == comp & 
                                                   measure.pred$keepX == test.keepX[keepX] &
                                                   measure.pred$keepY == test.keepY[keepY] &
                                                   measure.pred$V == v &
                                                   measure.pred$measure == measure.tune
                                                 ,]$optimum <- TRUE
                                  }
                                }
                                
                              # TODO drop Q2 as we dropped tune.pls
                            }
                            # Q2.total <-  Reduce('+', Q2.total)/nrepeat
                        } # end keepY
                    } #end keepX
                browser()
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
                    if (dim(arr)[3] < 3) ## low nrepeat, no t.test
                    {
                        extremum <- ifelse(is_cor, max, min)
                        ind.optimum <- which(arr == extremum(arr), arr.ind = TRUE)
                        
                        return(c(ind.choice.keepX = ind.optimum[1], 
                                 ind.choice.keepY = ind.optimum[2]))
                        
                    }
                    choice.keepX_i <- 1
                    choice.keepY_j <- 1
                    for (keepX_i in seq_len(dim(arr)[1])[-1]) {
                        for (keepY_j in seq_len(dim(arr)[2])[-1])
                        {
                            x <- arr[choice.keepX_i, choice.keepY_j, ]
                            y <- arr[keepX_i, keepY_j, ]
                            t.test.res <- t.test(x,
                                                 y,
                                                 alternative = ifelse(is_cor, 'less', 'greater'),
                                                 paired = FALSE)
                            if (t.test.res$p.value < 0.05)
                            {
                                choice.keepX_i <- keepX_i
                                choice.keepY_j <- keepY_j
                            }
                            
                        }
                    }
                    return(c(ind.choice.keepX = choice.keepX_i, 
                             ind.choice.keepY = choice.keepY_j))
                }
                
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
            } # end comp
  
        names(choice.keepX) <- names(choice.keepY) <- paste0('comp', seq_len(ncomp))
        out$choice.keepX = choice.keepX
        out$choice.keepY = choice.keepY

        out$cor.pred = cor.pred
        out$RSS.pred = RSS.pred
        ### evaluate all for output except X and Y to save memory
        ## eval all but X and Y
        mc <- mget(names(formals())[-1:-2], sys.frame(sys.nframe()))
        ## replace function, X and Y with unevaluated call
        mc <- as.call(c(as.list(match.call())[1:3], mc))
        out <- c(list(call = mc), out)
        class(out) <- c('tune.pls', 'tune.spls')
        return(out)
    } 
