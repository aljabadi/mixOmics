## ----pls-outline, eval=TRUE, echo=FALSE,  fig.cap='(ref:pls-outline)', out.width = '70%'----
knitr::include_graphics("Figures/Part3/PLS.pdf")


## ----pls-shape, eval=TRUE, echo=FALSE, fig.cap='(ref:pls-shape)', out.width = '50%'----
library(mixOmics)
coul <- color.mixo(1:3)

plot(0, type="n", xlim=c(0,110), ylim=c(85,112), axes=FALSE,
     xlab="",ylab="", main="PLS overview")
box()

# PLS1
rect(xleft = 20, xright = 50, ybottom = 98, ytop = 108, col=coul[1])
rect(xleft = 52, xright = 55, ybottom = 98, ytop = 108, col=coul[1])
text(5, 102, "PLS1", font = 2)

# PLS2
rect(xleft = 20, xright = 50, ybottom = 85, ytop = 95, col=coul[1])
rect(xleft = 52, xright = 73, ybottom = 85, ytop = 95, col=coul[1])
text(5, 90, "PLS2", font = 2)

text(30, 110, "X", font = 2)
text(60, 110, "Y", font = 2)

# legend
rect(xleft = 85, xright = 87, ybottom = 108, ytop = 110, col=coul[1])
text(100, 109, "Quantitative", cex=0.75)


## ----PLS-mode-overview, eval=TRUE, echo=FALSE, fig.cap='(ref:PLS-mode-overview)', fig.align="center", out.width = '90%'----
knitr::include_graphics("Figures/Part3/PLS-mode-crop.pdf")


## ----------------------------------------------------
library(mixOmics)
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic


## ----------------------------------------------------
head(data.frame(rownames(X), rownames(Y)))


## ---- eval = FALSE, fig.show = 'hide', message = FALSE----
## pls.result <- pls(X, Y)     # 1 Run the method
## plotIndiv(pls.result)       # 2 Plot the samples
## plotVar(pls.result)         # 3 Plot the variables


## ---- eval = FALSE, fig.show = 'hide', message = FALSE----
## spls.result <- spls(X, Y, keepX = c(10, 20), keepY = c(3, 2), ncomp = 2)
## plotIndiv(spls.result)
## plotVar(spls.result)


## ----------------------------------------------------
y <- liver.toxicity$clinic[, "ALB.g.dL."]


## ----pls1-Q2, fig.cap='(ref:pls1-Q2)'----------------
tune.pls1.liver <- pls(X = X, Y = y, ncomp = 4, mode = 'regression')
set.seed(33)  # for reproducibility with this handbook, remove otherwise
Q2.pls1.liver <- perf(tune.pls1.liver, validation = 'Mfold', 
                      folds = 10, nrepeat = 5)
#! plot(Q2.pls1.liver, criterion = 'Q2')

plot(Q2.pls1.liver$Q2.tot.ave, xlab = 'Dimension', ylab = 'Q2 total',
     main = 'PLS1: Q2 total averaged')
abline(h = 0.0975, col = 'red')


## ----spls1-MAE, eval = FALSE, fig.cap='(ref:spls1-MAE)'----
#! to update
# Set up grid of values:
list.keepX <- c(5:10, seq(15, 50, 5))
# list.keepX  # Inspect the keepX grid
set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls1.MAE <- tune.spls(X, y, ncomp= 2,
                          test.keepX = list.keepX,
                          validation = 'Mfold',
                          folds = 3,
                          nrepeat = 3, progressBar = TRUE,
                          measure = 'RSS')
plot(tune.spls1.MAE)


## ----------------------------------------------------
#! choice.ncomp <- tune.spls1.MAE$choice.ncomp$ncomp
# Optimal number of variables to select in X based on the MAE criterion
#! choice.keepX <- tune.spls1.MAE$choice.keepX[1:choice.ncomp]  # we stop at choice.ncomp

#! hard coded here to compile, to remove once bug fixed above
choice.ncomp <- 1
choice.keepX <- 20

choice.ncomp
choice.keepX


## ----------------------------------------------------
spls1.liver <- spls(X, y, ncomp = choice.ncomp, keepX = choice.keepX, mode = "regression")


## ---- eval = FALSE-----------------------------------
## selectVar(spls1.liver, comp = 1)$X$name


## ----------------------------------------------------
spls1.liver$prop_expl_var$X
tune.pls1.liver$prop_expl_var$X


## ----spls1-ext, fig.cap='(ref:spls1-ext)', message=FALSE----
spls1.liver.c2 <- spls(X, y, ncomp = 2, keepX = c(rep(choice.keepX, 2)), 
                       mode = "regression")

plotIndiv(spls1.liver.c2,
          group = liver.toxicity$treatment$Time.Group,
          pch = as.factor(liver.toxicity$treatment$Dose.Group),
          legend = TRUE, legend.title = 'Time', legend.title.pch = 'Dose')



## ----spls1-comp1, fig.cap='(ref:spls1-comp1)'--------
# Define factors for colors matching plotIndiv above
time.liver <- factor(liver.toxicity$treatment$Time.Group, levels = c('18', '24', '48', '6'))
dose.liver <- factor(liver.toxicity$treatment$Dose.Group, levels = c('50', '150', '1500', '2000'))
# Set up colors and symbols
col.liver <- color.mixo(time.liver)
pch.liver <- as.numeric(dose.liver)

plot(spls1.liver$variates$X, spls1.liver$variates$Y,
     xlab = 'X component', ylab = 'y component / scaled y',
     col = col.liver, pch = pch.liver)
legend('topleft', col = color.mixo(1:4), legend = levels(time.liver),
       lty = 1, title = 'Time')
legend('bottomright', legend = levels(dose.liver), pch = 1:4,
       title = 'Dose')

cor(spls1.liver$variates$X, spls1.liver$variates$Y)


## ----------------------------------------------------
set.seed(33)  # For reproducibility with this handbook, remove otherwise
# PLS1 model and performance
pls1.liver <- pls(X, y, ncomp = choice.ncomp, mode = "regression")
perf.pls1.liver <- perf(pls1.liver, validation = "Mfold", folds =10, 
                        nrepeat = 5, progressBar = FALSE)


# sPLS1 performance
perf.spls1.liver <- perf(spls1.liver, validation = "Mfold", folds = 10, 
                         nrepeat = 5, progressBar = FALSE)

#!perf.pls1.liver$MSEP; perf.spls1.liver$MSEP


## ----------------------------------------------------
dim(Y)


## ----pls2-Q2, fig.cap='(ref:pls2-Q2)'----------------
tune.pls2.liver <- pls(X = X, Y = Y, ncomp = 5, mode = 'regression')
set.seed(33)  # For reproducibility with this handbook, remove otherwise
Q2.pls2.liver <- perf(tune.pls2.liver, validation = 'Mfold', folds = 10, nrepeat = 5)

#! update with Q2 plot?
plot(Q2.pls2.liver$Q2.tot.ave, xlab = 'Components', ylab = 'Q2 total',
     ylim = c(-4, 1))
abline(h = 0.0975, col = 'red')


## ---- echo = TRUE, eval = FALSE----------------------
## # This code may take several min to run, parallelisation is possible
## list.keepX <- c(seq(5, 50, 5))
## list.keepY <- c(3:10)
## 
## set.seed(33)  # For reproducibility with this handbook, remove otherwise
## tune.spls.liver <- tune.spls(X, Y, test.keepX = list.keepX, test.keepY = list.keepY, ncomp = 2, nrepeat = 1, folds = 10, mode = 'regression', measure.tune = 'cor')


## ---- echo = FALSE, eval = TRUE, warning=FALSE-------
#! here put eval = TRUE
# parallel version to run for this hadbook, not shown
list.keepX <- c(seq(5, 50, 5))
list.keepY <- c(3:10)

set.seed(33)  # For reproducibility with this handbook, remove otherwise
tune.spls.liver <- tune.spls(X, Y, test.keepX = list.keepX, test.keepY = list.keepY, ncomp = 2, nrepeat = 1, folds = 10, mode = 'regression',
                             measure.tune = 'cor',                               # use two CPUs for faster computation
                             BPPARAM = BiocParallel::SnowParam(workers = 2))


## ---- eval = FALSE-----------------------------------
## #! here put eval = TRUE once debugged
## # Optimal parameters
## tune.spls.liver$choice.keepX
## tune.spls.liver$choice.keepY
## 
## # Graphical output
## plot(tune.spls.liver)


## ----------------------------------------------------
choice.ncomp <- 2
choice.keepX <- c(5, 5)
choice.keepY <- c(3, 3)

spls2.liver <- spls(X, Y, ncomp = choice.ncomp, 
                    keepX = choice.keepX,
                    keepY = choice.keepY,
                    mode = "regression")


## ----------------------------------------------------
spls2.liver$prop_expl_var


## ---- eval = FALSE-----------------------------------
## selectVar(spls2.liver, comp = 1)$X$value


## ----------------------------------------------------
vip.spls2.liver <- vip(spls2.liver)
vip.spls2.liver[selectVar(spls2.liver, comp = 1)$X$name,1]


## ----------------------------------------------------
perf.spls2.liver <- perf(spls2.liver, validation = 'Mfold', folds = 10, nrepeat = 5)
#! stability output not working anymore
#perf.spls2.liver$features


## ----spls2-plotIndiv, fig.cap='(ref:spls2-plotIndiv)'----
plotIndiv(spls2.liver, ind.names = FALSE, 
          group = liver.toxicity$treatment$Time.Group, 
          pch = as.factor(liver.toxicity$treatment$Dose.Group), 
          col.per.group = color.mixo(1:4),
          legend = TRUE, legend.title = 'Time', 
          legend.title.pch = 'Dose')


## ----spls2-plotArrow, fig.cap='(ref:spls2-plotArrow)'----
plotArrow(spls2.liver, ind.names = FALSE, 
          group = liver.toxicity$treatment$Time.Group,
          col.per.group = color.mixo(1:4),
          legend.title = 'Time.Group')


## ----spls2-plotVar, fig.cap='(ref:spls2-plotVar)'----
plotVar(spls2.liver, cex = c(3,4), var.names = c(FALSE, TRUE))


## ----spls2-plotVar2, fig.cap='(ref:spls2-plotVar2)'----
plotVar(spls2.liver,
        var.names = list(X.label = liver.toxicity$gene.ID[,'geneBank'],
                         Y.label = TRUE), cex = c(3,4))


## ----------------------------------------------------
# Define red and green colors for the edges
color.edge <- color.GreenRed(50)
# X11()  # to open a new window for Rstudio
network(spls2.liver, comp = 1:2,
        cutoff = 0.7,
        shape.node = c("rectangle", "circle"),
        color.node = c("cyan", "pink"),
        color.edge = color.edge,
        # To save the plot, otherwise comment out:
        save = 'pdf', name.save = 'network_liver')


## ----spls2-network, eval=TRUE, echo=FALSE,fig.cap='(ref:spls2-network)'----
knitr::include_graphics("network_liver.pdf")


## ----------------------------------------------------
# X11()  # To open a new window if Rstudio complains
cim(spls2.liver, comp = 1:2, xlab = "clinic", ylab = "genes",
    # To save the plot, otherwise comment out:
    save = 'pdf', name.save = 'cim_liver')


## ----spls2-cim, eval=TRUE, echo=FALSE, fig.cap='(ref:spls2-cim)'----
knitr::include_graphics("cim_liver.pdf")


## ---- eval = TRUE------------------------------------
# Comparisons of final models (PLS, sPLS)
pls.liver <- pls(X, Y, mode = 'regression', ncomp = 2)
perf.pls.liver <-  perf(pls.liver, validation = 'Mfold', folds = 10, nrepeat = 5)

spls.liver <- spls(X, Y, keepX = c(5,5), keepY = c(3,3), mode = 'regression', ncomp = 2)
perf.spls.liver <-  perf(spls.liver, validation = 'Mfold', folds = 10, nrepeat = 5)


## ----pls-perf-spls2, eval = TRUE, eval = FALSE, fig.cap='(ref:pls-perf-spls2)', out.width = '70%'----
## # not shown in handbook
## plot(c(1,2), perf.pls.liver$cor.pred$u, col = 'blue', pch = 16,
##      ylim = c(0.6,1),
##      xlab = 'component', ylab = 't or u Cor',
##      main = 's/PLS performance based on Correlation')
## points(perf.pls.liver$cor.pred$t, col = 'red', pch = 16)
## points(perf.spls.liver$cor.pred$u, col = 'blue', pch = 17)
## points(perf.spls.liver$cor.pred$t, col = 'red', pch = 17)
## legend('bottomleft', col = c('blue', 'red', 'blue', 'red'), pch = c(16, 16, 17, 17), c('u PLS', 't PLS', 'u sPLS', 't sPLS'))


## ----------------------------------------------------
data(linnerud)
X <- linnerud$exercise
Y <- linnerud$physiological
dim(X)
dim(Y)


## ----------------------------------------------------
pls.linn <- pls(X, Y, ncomp = 2, mode = "regression", scale = TRUE)


## ----------------------------------------------------
# New test individuals:
indiv1 <- c(14, 190, 35)
indiv2 <- c(2, 40, 45)

X_test <- rbind(indiv1, indiv2)
colnames(X_test) <- colnames(X)


## ----------------------------------------------------
Y_pred <- predict(pls.linn, X_test)


## ----------------------------------------------------
Y_pred$predict


## ----------------------------------------------------
# First, plot the individuals from the learning set in the X-space'
# graphics style is needed here to overlay plots
plotIndiv(pls.linn, style = 'graphics', rep.space = 'X-variate',
          title =  'Linnerud PLS regression: sample plot')

# Then, add the test set individuals coordinates that are predicted
points(Y_pred$variates[, 1], Y_pred$variates[, 2], pch = 17,
       col = c('red', 'blue'))
text(Y_pred$variates[, 1], Y_pred$variates[, 2], c("ind1", "ind2"), pos = 1,
     col = c('red', 'blue'))


## ----pls-loc-reg, eval=TRUE, echo=FALSE, fig.cap='(ref:pls-loc-reg)', fig.align="center", out.width = '70%'----
knitr::include_graphics("Figures/Part3/pls-loc-reg.pdf")

