## ---- fails
data("nutrimouse")
X <- nutrimouse$gene
Y <- nutrimouse$lipid
y <- Y[, "C18.0"]

pls1.liver <- pls(X = X, Y = y, ncomp = 4)
pls2.liver <- pls(X = X, Y = Y, ncomp = 4)
spls1.liver <- spls(X = X, Y = y, keepX = rep(10, 4), ncomp = 4)
spls2.liver <- spls(X = X, Y = Y, keepX = rep(10, 4), ncomp = 4)

## ---- fails
data("nutrimouse")
X <- data.matrix(nutrimouse$gene, rownames.force = TRUE)
Y <- data.matrix(nutrimouse$lipid, rownames.force = TRUE)
y <- Y[, "C18.0"]

pls1.liver <- pls(X = X, Y = y, ncomp = 4)
pls2.liver <- pls(X = X, Y = Y, ncomp = 4)
spls1.liver <- spls(X = X, Y = y, keepX = rep(10, 4), ncomp = 4)
spls2.liver <- spls(X = X, Y = Y, keepX = rep(10, 4), ncomp = 4)

## ---- works
data("nutrimouse")
X <- data.matrix(nutrimouse$gene, rownames.force = TRUE)
Y <- data.matrix(nutrimouse$lipid, rownames.force = TRUE)
y <- Y[, "C18.0"]

pls1.liver <- pls(X = X, Y = y, ncomp = 4)
pls2.liver <- pls(X = X, Y = Y, ncomp = 4)
spls1.liver <- spls(X = X, Y = y, keepX = rep(10, 4), ncomp = 4)
spls2.liver <- spls(X = X, Y = Y, keepX = rep(10, 4), ncomp = 4)
