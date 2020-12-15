
## --------------------------- plot.tune.(s)pls --------------------------- ##
#' Plot for model performance
#' 
#' Function to plot performance criteria, such as classification error rate or
#' balanced error rate on a tune.splsda result.
#' 
#' \code{plot.tune.splsda} plots the classification error rate or the balanced
#' error rate from x$error.rate, for each component of the model. A lozenge
#' highlights the optimal number of variables on each component.
#' 
#' \code{plot.tune.block.splsda} plots the classification error rate or the
#' balanced error rate from x$error.rate, for each component of the model. The
#' error rate is ordered by increasing value, the yaxis shows the optimal
#' combination of keepX at the top (e.g. `keepX on block 1'_`keepX on block
#' 2'_`keepX on block 3')
#' 
#' @param x an \code{tune.splsda} object.
#' @param optimal If TRUE, highlights the optimal keepX per component
#' @param sd If 'nrepeat' was used in the call to 'tune.splsda', error bar
#' shows the standard deviation if sd=TRUE
#' @param col character (or symbol) color to be used, possibly vector. One
#' color per component.
#' @param \dots Further arguments sent to \code{\link{xyplot}} function.
#' @return none
#' @author Kim-Anh Lê Cao, Florian Rohart, Francois Bartolo, AL J Abadi
#' @seealso \code{\link{tune.mint.splsda}}, \code{\link{tune.splsda}}
#' \code{\link{tune.block.splsda}} and http://www.mixOmics.org for more
#' details.
#' @keywords regression multivariate hplot
#' @name plot.tune
#' @method plot tune.spls1
#' @importFrom reshape2 melt
#' @export
plot.tune.spls1 <-
    function(x, optimal = TRUE, sd = TRUE, col, ...)
    {
        # to satisfy R CMD check that doesn't recognise x, y and group (in aes)
        y = Comp = lwr = upr = NULL
        
        if (!is.logical(optimal))
            stop("'optimal' must be logical.", call. = FALSE)
        
        
        error <- x$error.rate
        if(sd & !is.null(x$error.rate.sd))
        {
            error.rate.sd = x$error.rate.sd
            ylim = range(c(error + error.rate.sd), c(error - error.rate.sd))
        } else {
            error.rate.sd = NULL
            ylim = range(error)
        }
        
        select.keepX <- x$choice.keepX[colnames(error)]
        comp.tuned = length(select.keepX)
        
        legend=NULL
        measure = x$measure
        
        if (length(select.keepX) < 10)
        {
            #only 10 colors in color.mixo
            if(missing(col))
                col = color.mixo(seq_len(comp.tuned))
        } else {
            #use color.jet
            if(missing(col))
                col = color.jet(comp.tuned)
        }
        if(length(col) != comp.tuned)
            stop("'col' should be a vector of length ", comp.tuned,".")
        
        if(measure == "overall")
        {
            ylab = "Classification error rate"
        } else if (measure == "BER")
        {
            ylab = "Balanced error rate"
        } else if (measure == "MSE"){
            ylab = "MSE"
        }else if (measure == "MAE"){
            ylab = "MAE"
        }else if (measure == "Bias"){
            ylab = "Bias"
        }else if (measure == "R2"){
            ylab = "R2"
        }else if (measure == "AUC"){
            ylab = "AUC"
        }
        
        #legend
        names.comp = substr(colnames(error),5,10) # remove "comp" from the name
        if(length(x$choice.keepX) == 1){
            #only first comp tuned
            legend = "1"
        } else if(length(x$choice.keepX) == comp.tuned) {
            # all components have been tuned
            legend = c("1", paste("1 to", names.comp[-1]))
        } else {
            #first components were not tuned
            legend = paste("1 to", names.comp)
        }
        
        
        # creating data.frame with all the information
        df = melt(error)
        colnames(df) = c("x","Comp","y")
        df$Comp = factor(df$Comp, labels=legend)
        
        p = ggplot(df, aes(x = x, y = y, color = Comp)) +
            labs(x = "Number of selected features", y = ylab) +
            theme_bw() +
            geom_line()+ geom_point()
        p = p+ scale_x_continuous(trans='log10') +
            scale_color_manual(values = col)
        
        # error bar
        if(!is.null(error.rate.sd))
        {
            dferror = melt(error.rate.sd)
            df$lwr = df$y - dferror$value
            df$upr = df$y + dferror$value
            
            #adding the error bar to the plot
            p = p + geom_errorbar(data=df,aes(ymin=lwr, ymax=upr))
        }
        
        if(optimal)
        {
            index = NULL
            for(i in seq_len(comp.tuned))
                index = c(index, which(df$x == select.keepX[i] & df$Comp == levels(df$Comp)[i]))
            
            # adding the choseen keepX to the graph
            p=p + geom_point(data=df[index,],size=7, shape = 18)
            p = p + guides(color = guide_legend(override.aes =
                                                    list(size=0.7,stroke=1)))
        }
        
        p
    }

#' @rdname plot.tune
#' @method plot tune.splsda
#' @export
plot.tune.splsda <- plot.tune.spls1

## --------------------------- plot.perf.(s)pls --------------------------- ##
#' Plot for model performance
#' 
#' Function to plot performance criteria, such as MSEP, RMSEP, \eqn{R^2},
#' \eqn{Q^2} for s/PLS methods, and classification performance for supervised
#' methods, as a function of the number of components.
#' 
#' \code{plot.perf} creates one plot for each response variable in the model,
#' laid out in a multi panel display.  It uses \code{\link{xyplot}} for
#' performing the actual plotting.
#' 
#' More details about the prediction distances in \code{?predict} and the
#' supplemental material of the mixOmics article (Rohart et al. 2017).
#' 
#' @param x an \code{perf} object.
#' @param criterion character string. What type of validation criterion to plot
#' for \code{pls} or \code{spls}. One of \code{"MSEP"}, \code{"RMSEP"},
#' \code{"R2"} or \code{"Q2"}. See \code{\link{perf}}.
#' @param dist prediction method applied in \code{perf} for \code{plsda} or
#' \code{splsda}. See \code{\link{perf}}.
#' @param measure Two misclassification measure are available: overall
#' misclassification error \code{overall} or the Balanced Error Rate \code{BER}
#' @param col character (or symbol) color to be used, possibly vector. One
#' color per distance \code{dist}.
#' @param weighted plot either the performance of the Majority vote or the
#' Weighted vote.
#' @param study Indicates which study-specific outputs to plot. A character
#' vector containing some levels of \code{object$study}, "all.partial" to plot
#' all studies or "global" is expected. Default to "global".
#' @param overlay parameter to overlay graphs; if 'all', only one graph is
#' shown with all outputs; if 'measure', a graph is shown per distance; if
#' 'dist', a graph is shown per measure.
#' @param legend.position position of the legend, one of "vertical" (only one
#' column) or "horizontal" (two columns).
#' @param xlab,ylab titles for \eqn{x} and \eqn{y} axes.  Typically character
#' strings, but can be expressions (e.g., \code{expression(R^2)}).
#' @param LimQ2 numeric value. Signification limit for the components in the
#' model. Default is \code{LimQ2 = 0.0975}.
#' @param LimQ2.col character string specifying the color for the \code{LimQ2}
#' line to be plotted. If \code{"none"} the line will not be plotted.
#' @param cTicks integer vector. Axis tickmark locations for the used number of
#' components. Default is \code{1:ncomp} (see \code{\link{perf}}).
#' @param layout numeric vector of length two giving the number of rows and
#' columns in a multi panel display. If not specified, \code{plot.perf} tries
#' to be intelligent.
#' @param sd If 'nrepeat' was used in the call to 'perf', error bar shows the
#' standard deviation if sd=TRUE
#' @param \dots Further arguments sent to \code{\link{xyplot}} function.
#' @return none
#' @author Ignacio González, Florian Rohart, Francois Bartolo, Kim-Anh Lê Cao, Al J Abadi
#' @seealso \code{\link{pls}}, \code{\link{spls}}, \code{\link{plsda}},
#' \code{\link{splsda}}, \code{\link{perf}}.
#' @references
#' 
#' Rohart F, Gautier B, Singh A, Lê Cao K-A. mixOmics: an R package for 'omics
#' feature selection and multiple data integration. PLoS Comput Biol 13(11):
#' e1005752
#' @keywords regression multivariate hplot
#' @name plot.perf
#' @method plot perf.pls1.mthd
#' @export
#' @example ./examples/plot.perf-examples.R
plot.perf.pls1.mthd <-
    function (x,
              criterion = "MSEP",
              #c("MSEP", "RMSEP", "R2", "Q2"),
              xlab = "number of components",
              ylab = NULL,
              LimQ2 = 0.0975,
              LimQ2.col = "darkgrey",
              cTicks = NULL,
              layout = NULL,
              ...
    )
    {
        x$RMSEP <- sqrt(x$MSEP)
        if (x$method == 'pls2')
        {
            
        } else
        {
            
        }
        # if (!any(criterion %in% c("MSEP", "RMSEP", "R2", "Q2","Q2.total")) || length(criterion) > 1)
        #     stop("Choose one validation criterion among MSEP, RMSEP, R2 or Q2. Or Q2.total for pls2.")
        # 
        y = x[[criterion]]
        
        Q2.total = NULL
        if ((criterion == "Q2") & is.list(y)) {
            Q2.total = y$Q2.total
            y = y$variables
        }
        
        if (is.null(ylab)) {
            ylab <- criterion
            if (ylab == 'R2')
                ylab <-  expression(R^~2)
            else if (ylab == 'Q2')
                ylab <- expression(Q^~2)
        }
        
        nResp = nrow(y)  # Number of response variables
        nComp = ncol(y)  # Number of components
        
        #def.par = par(no.readonly = TRUE)
        
        if (nResp > 1) {
            if (is.null(layout)) {
                nRows = min(c(3, nResp))
                nCols = min(c(3, ceiling(nResp / nRows)))
                layout = c(nRows, nCols)
            }
            else {
                if (length(layout) != 2 || !is.numeric(layout) || any(is.na(layout)))
                    stop("'layout' must be a numeric vector of length 2.")
                nRows = layout[1]
                nCols = layout[2]
            }
            
            if (nRows * nCols < nResp) devAskNewPage(TRUE)
            ynames = rownames(y)
        } else {
            ynames = "Y"
        }
        
        val = comps = vector("numeric")
        varName = vector("character")
        
        for (i in 1:nResp) {
            val = c(val, y[i, ])
            comps = c(comps, 1:nComp)
            varName = c(varName, rep(ynames[i], nComp))
        }
        
        df = data.frame(val = val, comps = comps, varName = varName)
        if (is.null(cTicks)) cTicks = 1:ncol(y)
        yList = list(relation = "free")
        
        
        if (criterion == "Q2" | criterion == "Q2.total")
        {
            plt = xyplot(val ~ comps | varName, data = df, xlab = xlab, ylab = ylab,
                         scales = list(y = yList, x = list(at = cTicks)),
                         as.table = TRUE, layout = layout,
                         panel = function(x, y) {
                             if (LimQ2.col != "none") panel.abline(h = LimQ2, col = LimQ2.col)
                             panel.xyplot(x, y, ...)})
            plot(plt)
            
            if (!is.null(Q2.total)) {
                devAskNewPage(TRUE)
                Q2.df = data.frame(Q2 = Q2.total, comps = 1:nComp, varName = rep("Total", nComp))
                xyplot(Q2 ~ comps | varName, data = Q2.df, xlab = xlab, ylab = ylab,
                       scales = list(y = yList, x = list(at = cTicks)), as.table = TRUE,
                       panel = function(x, y) {
                           if (LimQ2.col != "none") panel.abline(h = LimQ2, col = LimQ2.col)
                           panel.xyplot(x, y, ...)})
            }
        } else {
            plt = xyplot(val ~ comps | varName, data = df, xlab = xlab, ylab = ylab,
                         scales = list(y = yList, x = list(at = cTicks)),
                         as.table = TRUE, layout = layout, ...)
            plot(plt)
            
        }
        
        if (nResp > 1) {
            if (nRows * nCols < nResp) devAskNewPage(FALSE)
        }
        
        #par(def.par)
        
        
    }

#' @rdname plot.perf
#' @method plot perf.spls1.mthd
#' @export
plot.perf.spls1.mthd <- plot.perf.pls1.mthd