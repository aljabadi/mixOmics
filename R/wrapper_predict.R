## for list of data and outcome Y, keep out n_test_per_class samples fro each class
## and perform block.splsda using keepX variables on each component, 
## then use the kept out samples for prediction and output the
## result from 'predict.block.splsda'
wrapper_predict <- function(data, Y, ncomp=NULL, n_test_per_class = 2, keepX=5, dist = "all", version = "dev") {
    
    unloadNamespace("mixOmics")
    if (version == "dev") {
        devtools::load_all(".")
    } else {
        library(mixOmics)
    }
    ## ---- get 'n_test_per_class' unique samples per class
    Y <- as.character(Y)
    
    if (is.null(ncomp)) {
        ncomp <- max(length(unique(Y))-2, 2)
    }
    
    ind_test <- vector(mode = "logical", length = length(Y))
    Y.tmp <- Y
    for (n_per_class in seq_len(n_test_per_class)) {
        ind_test <- ind_test | !duplicated(factor(Y.tmp))
        Y.tmp[ind_test] <- "foobareggspam"
    }
    
    ind_test
    
    X_train <- lapply(data, function(x){
        x[!ind_test,]
    })
    X_test <- lapply(data, function(x){
        x[ind_test,]
    })
    Y_train <- Y[!ind_test]
    Y_test <- Y[ind_test]
    
    keepX <- lapply(X_train, function(x) rep(keepX, ncomp))
    model <- block.splsda(X=X_train,Y=Y_train,
                          ncomp=ncomp,keepX=keepX)
    pred_res <- predict(model, newdata=X_test, dist = dist)
    
    accuracy <- sapply(pred_res$WeightedVote, function(pred_matrix){
        sum(Y_test %in% pred_matrix[,ncomp])/length(Y_test)
    })
    
    list(
        result = predict(model, newdata=X_test, dist = dist),
        accuracy = accuracy
    )
}