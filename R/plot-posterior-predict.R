#' Plot fixed effects.
#' 
#' @param fits A \code{list} of \code{brmsfit} objects.
#' @param labels The coordinate reference system to use: integer with the EPSG code, or character with \code{proj4string}.
#' @param effect Other arguments passed on to \code{coord_sf}.
#' @return a \code{ggplot}.
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom brms posterior_predict
#' @importFrom bayesplot ppc_bars_data
#' @export
#' 
plot_posterior_predict <- function(object, ndraws = 1000, fit_colour = "blue") {
  
  yrep <- posterior_predict(object, ndraws = ndraws)
  
  ppc <- ppc_bars_data(y = as.numeric((object$data$age)), yrep = yrep, group = object$data$fyear) %>%
    rename(age = x, fyear = group)
  
  ggplot(data = ppc, aes(x = age)) +
    geom_col(aes(y = y_obs)) +
    geom_pointrange(aes(y = m, ymin = l, ymax = h), colour = fit_colour, alpha = 0.5, shape = 1) +
    labs(x = "Age", y = "Count") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(fyear ~ ., ncol = 3)
}


#' Plot thresholds.
#' 
#' @param fits A \code{list} of \code{brmsfit} objects.
#' @param labels The coordinate reference system to use: integer with the EPSG code, or character with \code{proj4string}.
#' @param effect Other arguments passed on to \code{coord_sf}.
#' @return a \code{ggplot}.
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom brms posterior_predict
#' @importFrom bayesplot ppc_bars_data
#' @export
#' 
plot_threshold <- function(object, years = c(1990, 1999, 2006, 2012, 2018, 2019), ncol = 2) {
  
  newdata <- data.frame(fyear = sort(unique(object$data$fyear)), 
                        length = mean(object$data$length), 
                        x = mean(object$data$x), y = mean(object$data$y)) %>%
    mutate(cyear = as.numeric(as.character(fyear)))
  
  mu <- fitted(object = object, newdata = newdata, scale = "linear") %>%
    bind_cols(newdata)
  
  fe <- fixef(object) %>%
    data.frame() %>%
    mutate(var = rownames(.))
  
  tau <- fe %>%
    filter(str_detect(var, "Intercept")) %>%
    mutate(fyear = strex::str_first_number(var), bin = strex::str_nth_number(var, n = 2)) %>%
    filter(fyear %in% years)
  
  disc <- fe %>%
    filter(str_detect(var, "disc")) %>%
    mutate(fyear = strex::str_first_number(var))
  
  x <- seq(from = -7, to = 7, length.out = 500)
  
  df <- tibble(fyear = sort(unique(fit$data$fyear)), 
               mu = mu$Estimate, 
               sigma = 1 / exp(c(0, disc$Estimate))) %>% 
    expand(nesting(fyear, mu, sigma), x = x) %>% 
    mutate(d = dnorm(x = x, mean = mu, sd = sigma)) %>%
    filter(fyear %in% years)
  
  ggplot(data = df, aes(x = x, y = d, fill = fyear, colour = fyear)) +
    geom_area(alpha = 1/2, position = "identity") +
    geom_vline(data = tau, aes(xintercept = Estimate), linetype = 3) +
    scale_x_continuous(expression(Phi)) +
    scale_y_continuous(NULL, breaks = NULL) +
    facet_wrap(fyear ~ ., ncol = ncol) +
    theme(legend.position = "none")
}
