## ---------------------------- .check_pls_Y ---------------------------- ##
#' Check Y arg for PLS(DA) analyses
#'
#' @param Y Y argument from user
#' @param DA logical indicating whether Y should become a factor
#'
#' @return A matrix or factor, or a condition
#'
#' @noRd
#' @keywords Internal
#' @examples
#' \dontrun{
#' .get_pls_Y(c(1,2,3))
#' .get_pls_Y(c(1,2,3), DA = TRUE)
#' .get_pls_Y(cars)
#' .get_pls_Y(cars, DA = TRUE)
#' .get_pls_Y(letters)
#' }
.check_pls_Y <- function(Y, DA = FALSE)
{
    if (isFALSE(DA)) {
        ## ---- NOT DA
        if (is.matrix(Y))
        {
            return(Y)
        } else if (is.numeric(Y) || is.data.frame(Y))
        {
            return(as.matrix(Y))
        } else {
            stop("'Y' must be a matrix", call. = FALSE)
        }
    } else {
        ## ---- DA
        if (is.null(dim(Y)))
        {
            return(factor(Y))
        } else {
            stop("'Y' must be a factor or vector", call. = FALSE)
        }
    }
    ## should never get here
    stop("something went wrong!", call. = FALSE)
}
