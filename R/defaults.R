#' Default priors.
#' 
#' @return a \code{brmsprior} \code{data.frame}.
#' 
#' @importFrom brms prior
#' @export
#' 
default_priors <- c(prior(normal(0, 10), class = "Intercept"), prior(normal(0, 1), class = "b"))
