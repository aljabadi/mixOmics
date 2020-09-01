## get row.sideColors, col.sideColors, col.names, row.names

.get_cim_names <- function(mapping = "X",
                           mat = NULL,
                           row.names = NULL,
                           col.names = NULL)
    ## col.name length
{
    xy <- (mapping == "XY")
    
    rn_len <- ifelse(xy, p ,n) ## row.name length
    col_names_block <- ifelse(mapping == "X", "X", "Y") ## block for col.names
    cn_len <- ifelse(col_names_block == "X", p, q) ## col.name length
    
    .custom_stop <- function(what, length){
        stop(sprintf("'%s' must be a character vector of length %s", what, length), call. = FALSE)
    }

    if (is.logical(row.names))
    {
        if (isTRUE(row.names))
            row.names = ifelse (xy, mat$names$colnames$X , mat$names$sample)
        else
            row.names = rep("", rn_len)
    } else {
        if (length(row.names) != rn_len)
            .custom_stop('row.names', rn_len)
    }
    
    if (is.logical(col.names))
    {
        if (isTRUE(col.names))
            col.names = mat$names$colnames[[col_names_block]]
        else
            col.names = rep("", cn_len)
    } else {
        if (length(col.names) != cn_len)
            .custom_stop('col.names', cn_len)
    }
    
    
    if (!is.null(row.sideColors))
    {
        row.sideColors = as.matrix(row.sideColors)
        if (nrow(row.sideColors) != rn_len)
            .custom_stop('row.sideColors', rn_len)
    
    if (!is.null(col.sideColors))
    {
        col.sideColors = as.matrix(col.sideColors)
        if (nrow(col.sideColors) != cn_len)
            .custom_stop('col.sideColors', cn_len)
    }
    
    return(list(row.names = row.names, col.names = col.names, 
                col.sideColrs=col.sideColrs, row.sideColors=row.sideColors))
    
    }
}