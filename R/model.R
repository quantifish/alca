#' Data to wide.
#' 
#' @param x The \code{data.frame} to .
#' @param names_from The coordinate reference system to use: integer with the EPSG code, or character with \code{proj4string}.
#' @param values_from Other arguments passed on to \code{coord_sf}.
#' @param ... Other arguments passed on to \code{coord_sf}.
#' @return a \code{data.frame} with the data appended as a matrix.
#' 
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @export
#' 
data_to_wide <- function(x, names_from = "length", values_from = "count", sum_to_one = FALSE, ...) {
  
  df_wide <- x %>%
    pivot_wider(names_from = names_from, values_from = values_from, names_sort = TRUE, names_prefix = "A.", values_fill = 0, ...)
  
  Y <- df_wide %>% 
    select(all_of(starts_with("A."))) %>% 
    as.matrix()
  colnames(Y) <- NULL
  if (sum_to_one) Y <- Y / rowSums(Y)
  
  df <- df_wide %>% 
    select(!all_of(starts_with("A."))) %>% 
    data.frame()
  df$size <- rowSums(Y)
  df$Y <- Y
  
  return(df)
}


#' Find the midpoints of a vector of bins
#' 
#' @param fit The \code{brms} model fit.
#' @return a \code{function} containing initial values.
#' 
#' @importFrom stringr str_sub str_extract
#' @export
#' 
cut_midpoint <- function(x) {
  (as.numeric(str_sub(str_extract(x, '(\\(|\\[).*,'), 2, -2)) + as.numeric(str_sub(str_extract(x, ',.*(\\)|\\])'), 2, -2))) / 2
}


#' Aggregate the tails of a matrix
#' 
#' @param fit The \code{brms} model fit.
#' @return a \code{function} containing initial values.
#' 
#' @export
#' 
aggregate_lf_tails <- function(M, lb = 1, ub) {
  if (lb > 1) {
    m1 <- rowSums(M[,1:lb])
  } else {
    m1 <- M[,lb]
  }
  
  m2 <- M[,(lb + 1):(ub - 1)]
  
  if (ub < ncol(M)) {
    m3 <- rowSums(M[,ub:ncol(M)])
  } else {
    m3 <- M[,ub]
  }
  
  MM <- cbind(m1, m2, m3)
  colnames(MM) <- NULL
  return(MM)
}


#' Get initial values.
#' 
#' @param fit The \code{brms} model fit.
#' @return a \code{function} containing initial values.
#' 
#' @import dplyr
#' @importFrom nlme fixef
#' @export
#' 
get_init <- function(fit) {
  fe <- fixef(fit) %>% 
    data.frame()
  init <- as.list(fe$Estimate)
  names(init) <- rownames(fe)
  init_fun <- function() init
  return(init_fun)
}
