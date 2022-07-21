#' Data to wide.
#' 
#' @param x A \code{data.frame}.
#' @param names_from The column name to change to wide format.
#' @param values_from The values to change to wide format.
#' @param levels The factor levels if wanting to pad missing values with zeros.
#' @param sum_to_one Force the data in each row to sum to one.
#' @return a \code{data.frame} with the data in wide format appended as a matrix.
#' 
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @export
#' 
data_to_wide <- function(x, names_from = "length", values_from = "count", 
                         levels = NULL, sum_to_one = FALSE) {
  
  if (!is.null(levels)) {
    x[[names_from]] <- factor(x[[names_from]], levels = levels)
  }
  
  df_wide <- x %>%
    pivot_wider(names_from = names_from, values_from = values_from, 
                names_expand = TRUE, names_sort = TRUE, names_prefix = "A.", values_fill = 0)
  
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
#' @param x The \code{brms} model fit.
#' @return a \code{function} containing initial values.
#' 
#' @importFrom stringr str_sub str_extract
#' @export
#' 
cut_midpoint <- function(x) {
  (as.numeric(str_sub(str_extract(x, '(\\(|\\[).*,'), 2, -2)) + as.numeric(str_sub(str_extract(x, ',.*(\\)|\\])'), 2, -2))) / 2
}


#' Logit function
#' 
#' @param x The \code{brms} model fit.
#' @return a \code{function} containing initial values.
#' 
#' @importFrom stringr str_sub str_extract
#' @export
#' 
logit <- qlogis


#' Logistic function
#' 
#' @param x The \code{brms} model fit.
#' @return a \code{function} containing initial values.
#' 
#' @importFrom stringr str_sub str_extract
#' @export
#' 
logistic <- plogis


#' Aggregate the tails of a matrix
#' 
#' @param M A \code{matrix}.
#' @param lb The row of the matrix to aggregate into.
#' @param ub The row of the matrix to aggregate into.
#' @return A \code{matrix}.
#' 
#' @export
#' 
aggregate_composition_tails <- function(M, lb = 1, ub) {
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


#' Get initial values from a previous model fit.
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


#' Get initial values from the data.
#' 
#' @param fit The \code{brms} model fit.
#' @return a \code{function} containing initial values.
#' 
#' @import dplyr
#' @importFrom nlme fixef
#' @export
#' 
get_init_data <- function(x) {
  fe <- fixef(fit) %>% 
    data.frame()
  init <- as.list(fe$Estimate)
  names(init) <- rownames(fe)
  init_fun <- function() init
  return(init_fun)
}
