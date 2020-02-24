#############################################################################################################
# Authors:
#   Ignacio Gonzalez, Genopole Toulouse Midi-Pyrenees, France
#   Kim-Anh Le Cao, French National Institute for Agricultural Research and
#   ARC Centre of Excellence ins Bioinformatics, Institute for Molecular Bioscience, University of Queensland, Australia
#   Sebastien Dejean, Institut de Mathematiques, Universite de Toulouse et CNRS (UMR 5219), France
#
# created: 2009
# last modified:
#
# Copyright (C) 2009
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

nipals = function (X, ncomp = 1, reconst = FALSE, max.iter = 500, tol = 1e-09,
                   center = FALSE, scale = FALSE,  startcol = 0,
                   force.na = TRUE, gramschmidt = FALSE, verbose = FALSE)
{
    ## proudly taken from the mighty nipals::nipals
    x <- as.matrix(X)
    nvar <- ncol(x)
    nobs <- nrow(x)
    x.orig <- x
    col.na.count <- apply(x, 2, function(x) sum(!is.na(x)))
    if (any(col.na.count == 0)) 
        stop("At least one column is all NAs")
    row.na.count <- apply(x, 1, function(x) sum(!is.na(x)))
    if (any(row.na.count == 0)) 
        stop("At least one row is all NAs")
    if (center) {
        cmeans <- colMeans(x, na.rm = TRUE)
        x <- sweep(x, 2, cmeans, "-")
    }
    else cmeans <- NA
    if (scale) {
        csds <- apply(x, 2, sd, na.rm = TRUE)
        x <- sweep(x, 2, csds, "/")
    }
    else csds <- NA
    TotalSS <- sum(x * x, na.rm = TRUE)
    PPp = matrix(0, nrow = nvar, ncol = nvar)
    TTp = matrix(0, nrow = nobs, ncol = nobs)
    eig <- rep(NA, length = ncomp)
    R2cum <- rep(NA, length = ncomp)
    loadings <- matrix(nrow = nvar, ncol = ncomp)
    scores <- matrix(nrow = nobs, ncol = ncomp)
    iter <- rep(NA, length = ncomp)
    x.miss <- is.na(x)
    has.na <- any(x.miss)
    if (force.na) 
        has.na <- TRUE
    for (h in 1:ncomp) {
        if (is.function(startcol)) {
            scol <- which.max(apply(x, 2, startcol))
        }
        else if (startcol == 0L) {
            scol <- which.max(apply(x, 2, function(x) sum(abs(x), 
                                                          na.rm = TRUE)))
        }
        else {
            scol <- startcol
        }
        if (verbose >= 1) 
            cat("PC ", h, " starting column: ", scol, sep = "")
        if (has.na) {
            x0 <- x
            x0[x.miss] <- 0
            th <- x0[, scol]
        }
        else {
            th <- x[, scol]
        }
        pciter <- 1
        continue <- TRUE
        while (continue) {
            if (has.na) {
                T2 <- matrix(th * th, nrow = nobs, ncol = nvar)
                T2[x.miss] <- 0
                ph <- crossprod(x0, th)/colSums(T2)
            }
            else {
                ph = crossprod(x, th)/sum(th * th)
            }
            if (gramschmidt && h > 1) {
                ph <- ph - PPp %*% ph
            }
            ph <- ph/sqrt(sum(ph * ph, na.rm = TRUE))
            th.old <- th
            if (has.na) {
                P2 <- matrix(ph * ph, nrow = nvar, ncol = nobs)
                P2[t(x.miss)] <- 0
                th = x0 %*% ph/colSums(P2)
            }
            else {
                th = x %*% ph/sum(ph * ph)
            }
            if (gramschmidt && h > 1) {
                th <- th - TTp %*% th
            }
            if (sum((th - th.old)^2, na.rm = TRUE) < tol) 
                continue = FALSE
            pciter <- pciter + 1
            if (pciter == max.iter) {
                continue <- FALSE
                warning("Stopping after ", max.iter, " iterations for PC ", 
                        h, ".\n")
            }
            if (verbose >= 1) 
                cat(".")
        }
        if (verbose >= 1) 
            cat("\n")
        x <- x - (th %*% t(ph))
        loadings[, h] <- ph
        scores[, h] <- th
        eig[h] <- sum(th * th, na.rm = TRUE)
        iter[h] <- pciter
        if (gramschmidt) {
            PPp = PPp + tcrossprod(ph)
            TTp = TTp + tcrossprod(th)/eig[h]
        }
        R2cum[h] <- 1 - (sum(x * x, na.rm = TRUE)/TotalSS)
    }
    R2 <- c(R2cum[1], diff(R2cum))
    eig = sqrt(eig)
    scores = sweep(scores, 2, eig, "/")
    if (reconst) {
        xhat <- tcrossprod(sweep(scores, 2, eig, "*"), loadings)
        if (scale) 
            xhat <- sweep(xhat, 2, csds, "*")
        if (center) 
            xhat <- sweep(xhat, 2, cmeans, "+")
        rownames(xhat) <- rownames(x.orig)
        colnames(xhat) <- colnames(x.orig)
    }
    else {
        xhat <- NULL
    }
    rownames(scores) <- rownames(x)
    colnames(scores) <- paste("PC", 1:ncol(scores), sep = "")
    rownames(loadings) <- colnames(x)
    colnames(loadings) <- paste("PC", 1:ncol(loadings), sep = "")
    out <- list(eig = eig, t = scores, p = loadings)
    if (reconst) {
        out$rec <- xhat
    }
    return(out)
}
