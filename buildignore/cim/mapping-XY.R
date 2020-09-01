if (mapping == "XY") {
    object = XY.mat
    
    cut=list()
    if (threshold != 0) {
        cut[[1]] = unlist(lapply(1:nrow(object),
                                 function(x){any(abs(object[x,]) > threshold)}))
        object = object[cut[[1]],]
        if (dist.method[1] != "correlation")
            cord.X = cord.X[cut[[1]],]
        
        
        if (is.null(nrow(object)) || nrow(object) == 0)
            stop("threshold value very high. No variable was selected.",
                 call. = FALSE)
        
        
        cut[[2]] = unlist(lapply(1:ncol(object),
                                 function(x){any(abs(object[,x]) > threshold)}))
        object = object[,cut[[2]]]
        if (dist.method[2] != "correlation")
            cord.Y = cord.Y[cut[[2]],]
        
        
        if (is.null(ncol(object)) || ncol(object) == 0)
            stop("threshold value very high. No variable was selected.",
                 call. = FALSE)
        
    }
    
    if ((cluster == "both") || (cluster == "row")) {
        #Rowv = rowMeans(XY.mat)
        Rowv = rowMeans(cord.X)
        
        if (dist.method[1] == "correlation")
            dist.mat = as.dist(1 - cor(t(as.matrix(object)),
                                       method = "pearson"))
        
        else
            dist.mat = dist(cord.X, method = dist.method[1])
        
        if (threshold > 0 ) {
            row.names = row.names[cut[[1]]]
            if (!is.null(row.sideColors))
                row.sideColors = as.matrix(row.sideColors[cut[[1]], ])
        }
        
        hcr = hclust(dist.mat, method = clust.method[1])
        ddr = as.dendrogram(hcr)
        ddr = reorder(ddr, Rowv)
        rowInd = order.dendrogram(ddr)
        object = object[rowInd, ]
        row.names = row.names[rowInd]
        
        if (!is.null(row.sideColors))
            row.sideColors = as.matrix(row.sideColors[rowInd, ])
    }
    
    if ((cluster == "both") || (cluster == "column")) {
        Colv = rowMeans(cord.Y)
        
        if (dist.method[2] == "correlation")
            dist.mat = as.dist(1 - cor(as.matrix(object),
                                       method = "pearson"))
        else
            dist.mat = dist(cord.Y, method = dist.method[2])
        
        if (threshold > 0 ) {
            col.names = col.names[cut[[2]]]
            if (!is.null(col.sideColors))
                col.sideColors = as.matrix(col.sideColors[cut[[2]], ])
        }
        
        hcc = hclust(dist.mat, method = clust.method[2])
        ddc = as.dendrogram(hcc)
        ddc = reorder(ddc, Colv)
        colInd = order.dendrogram(ddc)
        object = object[, colInd]
        col.names = col.names[colInd]
        
        if (!is.null(col.sideColors))
            col.sideColors = as.matrix(col.sideColors[colInd, ])
    }
}






