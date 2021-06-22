#' Plot the values for signature variables
#'
#' Plots the standardaised 'expression' values for the variables for
#' a given block on a given component.
#' 
#' @param object An object of class \code{block.splsda} or \code{block.spls}
#' @param block Name of the block to use
#' @param comp Integer, the component to use
#' @param group Factor, the grouping variable (not required for DA objects)
#' @param top_n Integer, only show this number of top features
#' @template arg/col.per.group
#' @param contrib The contribution type. one of c('max', 'min')
#' @param global Logical indicating whether to seggregate all features or show
#'   the global plots.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' see ?block.splsda and ?block.spls
plotSig <-
    function(object,
             block,
             comp = 1,
             group = NULL,
             top_n = NULL,
             col.per.group = NULL,
             contrib = c('max', 'min'),
             global = FALSE,
             title = NULL)
    {
        
    contrib <- match.arg(contrib)
    
    blocks <- names(object$X)
    if (!block %in% blocks)
        stop(message = sprintf("block must be one of: %s", paste0(blocks, collapse = ', ')))
    df <- data.frame(object$X[[block]], check.names = FALSE)
    
    ## group factor
    group <- .get.group(group, object, n_ind = nrow(df))
    
    col.group <- .get.cols.and.group(col.per.group = col.per.group, 
                                     group = group)
    group <- col.group$group
    col.per.group <- col.group$col.per.group
    vars <- selectVar(object, block=block, comp=comp)[[1]]$value
    if (contrib == 'max')
    {
        vars <- vars[vars$value.var > 0, drop=FALSE, ]
    } else
    {
        vars <- vars[vars$value.var < 0, drop=FALSE, ]
    }
    if (is.null(top_n))
    {
        top_n <- nrow(vars)
    } else {
        top_n <- min(as.integer(top_n), nrow(vars))
    }
    
    
    if (top_n < 3)
    {
        message("Too few variables match the provided criteria. Aborting the plot.")
        return(NULL)
    }
    vars <- vars[seq_len(top_n),,drop=FALSE]
    var.names <- rownames(vars)
    if (!all(var.names %in% colnames(df)))
        .stop("Unexpected error.\n")
    df <- df[,var.names]
    df$group <- group
    df <-
        melt(df,
             id.vars = 'group',
             variable.name = 'feature',
             value.name = 'value')
    df$feature <- factor(df$feature, levels = var.names, ordered = TRUE)
    
    if (global)
    {
        df$feature <- 'plotSig'
    }

    p <- ggplot(df, aes(group, value, fill=group)) +  
        geom_violin(adjust=0.9) +
        geom_boxplot(width=0.1) + 
        scale_fill_manual(values = col.per.group) + 
        theme_classic() + 
        labs(x='', 
             y='value (standardised)', 
             title = title) + 
        theme(legend.position = 'none', 
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
              plot.title = element_text(hjust = 0.5), 
              strip.background = element_rect(colour="black", fill="grey80")) + 
        facet_wrap(.~feature)
    p
}
