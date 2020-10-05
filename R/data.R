#' Dataset for variance component analysis
#'
#' Looking at the effect of Machine and Metrology on the variation in the response.
#'
#' @format A data frame with 54 rows and 3 variables:
#' \describe{
#'   \item{Machine}{3 Levels of different machines used for production}
#'   \item{Metrology}{3 Levels of different metrology used for measurement}
#'   \item{Response}{Measured value of the characteristic under investigation}
#' }
#' @source Simulated data
"dt"


#' Dataset for single factor variance component analysis, used in vignette
#'
#' @format A data frame with 36 rows and 2 variables:
#' \describe{
#'   \item{Group}{3 Level Factor}
#'   \item{Response}{Measured value of the characteristic under investigation}
#' }
#' @source Simulated data
"dt2"


#' Summary table from dt, used for vignette
#'
#' @format A data frame with 9 rows and 5 variables:
#' \describe{
#'   \item{Machine}{3 Levels of different machines used for production}
#'   \item{Metrology}{3 Levels of different metrology used for measurement}
#'   \item{rowVariance}{Sample variance of the respone}
#'   \item{rowN}{Sample size of the response at each factor combination}
#'   \item{popVar}{Sample variance rescaled to population variance by multypling by (N-1)/N}
#' }
#' @source Simulated data
"VarTable"
