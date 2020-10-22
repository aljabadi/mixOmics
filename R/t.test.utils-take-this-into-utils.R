#' Tranform bounded performance measures for t.test
#'
#' Error values are first turned into accuracy measures so that t.test would
#' compare against the 'less' alternative. The values are then arctanh tranformed
#' so that they would be unbounded. The values are capped at atanh(0.99) in favour
#' of better variable selection.
#' 
#' @param value Correlation or error rate value matrix
#' @param type Type of values (correlation of error rate)
#'
#' @return A transformed matrix.
#' @noRd
#' @examples
t.test_transform <- function(value, type = c('cor', 'error')) {
    type <- match.arg(type)
    
    acc <- value
    if (type == 'error') {
        ## errors to accuracy
        acc <- 1 - acc
    }
    ## atanh tranform
    acc <- atanh(acc)
    ## 99% accuracy as the limit, as atanh increases rapidly
    ## near 1 and the gain is not that important in practice
    acc[acc > atanh(0.99)] <- atanh(0.99)
    acc
}