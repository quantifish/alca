#' Data to wide.
#' 
#' @param x The \code{data.frame} to .
#' @param names_from The coordinate reference system to use: integer with the EPSG code, or character with \code{proj4string}.
#' @param values_from Other arguments passed on to \code{coord_sf}.
#' @param ... Other arguments passed on to \code{coord_sf}.
#' @return a \code{data.frame} with the data appended as a matrix.
#' 
#' @import dplyr
#' @export
#' 
data_to_wide <- function(x, names_from = "length", values_from = "count", ...) {
  
  df_wide <- x %>%
    pivot_wider(names_from = names_from, values_from = values_from, names_sort = TRUE, names_prefix = "A.", values_fill = 0, ...)
  
  Y <- df_wide %>% 
    select(all_of(starts_with("A."))) %>% 
    as.matrix()
  colnames(Y) <- NULL
  
  df <- df_wide %>% 
    select(!starts_with("A.")) %>% 
    data.frame()
  df$size <- rowSums(Y)
  df$Y <- Y
  
  return(df)
}


#' Default priors.
#' 
#' @return a \code{brmsprior} \code{data.frame}.
#' 
#' @importFrom brms prior
#' @export
#' 
default_priors <- c(prior(normal(0, 10), class = "Intercept"), prior(normal(0, 1), class = "b"))
