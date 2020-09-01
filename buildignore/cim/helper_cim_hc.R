## do hc for all mappings
.helper_cim_hc <- function(mapping) {
    
    #-- cheking center and scale
    if (!is.logical(center)) {
        if (!is.numeric(center) || (length(center) != ncol(mat[[mapping]])))
            stop("'center' should be either a logical value or a numeric
                    vector of length equal to the number of columns of '%s'.",
                 call. = FALSE)
    }
    if (!is.logical(scale)) {
        if (!is.numeric(scale) || (length(scale) != ncol(mat[[mapping]])))
            stop(sprintf("'scale' should be either a logical value or a numeric
                    vector of length equal to the number of columns of '%s'.", mapping),
                 call. = FALSE)
    }
    
    object = scale(mat[[mapping]], center = center, scale = scale)
    matr = as.matrix(mat$variates[[mapping]][, comp])
    
    if ((cluster == "both") || (cluster == "row")) {
        Rowv = rowMeans(matr)
        
        if (dist.method[1] == "correlation")
            dist.mat = as.dist(1 - cor(t(as.matrix(matr)),
                                       method = "pearson"))
        else
            dist.mat = dist(matr, method = dist.method[1])
        
        hcr = hclust(dist.mat, method = clust.method[1])
        ddr = as.dendrogram(hcr)
        ddr = reorder(ddr, Rowv)
        rowInd = order.dendrogram(ddr)
        object <<- object[rowInd, ]
        row.names <<- row.names[rowInd]
        
        if (!is.null(row.sideColors))
            row.sideColors <<- as.matrix(row.sideColors[rowInd, ])
    }
    
    if ((cluster == "both") || (cluster == "column")) {
        Colv = rowMeans(cord)
        
        if (dist.method[2] == "correlation")
            dist.mat = as.dist(1 - cor(t(cord.X), method = "pearson"))
        else
            dist.mat = dist(cord.X, method = dist.method[2])
        
        hcc = hclust(dist.mat, method = clust.method[2])
        ddc = as.dendrogram(hcc)
        ddc = reorder(ddc, Colv)
        colInd = order.dendrogram(ddc)
        object <<- object[, colInd]
        col.names <<- col.names[colInd]
        
        if (!is.null(col.sideColors))
            col.sideColors <<- as.matrix(col.sideColors[colInd, ])
        
    }
}