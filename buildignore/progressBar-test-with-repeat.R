progressBar=TRUE
iter <- 0
tol <- 1e-5
max.iter <- 50000
cat("Progress (approximate):\n")
repeat {
    iter <- iter + 1
    diffz <- exp(-iter/400)/exp(-1)/1e5
    if (iter == 1 & progressBar)
    {
        diffz0 = diffz
        prog.max <- 0
    }
    Sys.sleep(3/500)
    if (progressBar)
    {
        prog <- max((log(diffz) - log(tol))/ (log(diffz0) - log(tol)), iter/max.iter)
        prog <- 1 - ((log(diffz) - log(tol))/ (log(diffz0) - log(tol)))
        .progressBar(prog, title = 'Convergence (approximate)', breakline = FALSE)
            
    }
    if (diffz < tol | iter > max.iter)
        break
    
}
cat("\n")