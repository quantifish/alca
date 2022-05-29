#' Plot fixed effects.
#' 
#' @param fits A \code{list} of \code{brmsfit} objects.
#' @param labels The coordinate reference system to use: integer with the EPSG code, or character with \code{proj4string}.
#' @param effect Other arguments passed on to \code{coord_sf}.
#' @return a \code{ggplot}.
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom nlme fixef
#' @importFrom tidyr separate
#' @importFrom readr parse_number
#' @importFrom stringr str_detect
#' @export
#' 
plot_fixef <- function(fits, labels = "Males", effect = "Intercept", 
                       xlab = "Midpoint of tail width (mm)") {
  
  fe <- NULL  
  for (i in 1:length(fits)) {
    df <- fixef(object = fits[[i]]) %>% 
      data.frame() %>% 
      mutate(par = rownames(.)) %>%
      separate(.data$par, into = c("bin", "par"), sep = "_") %>%
      mutate(bin = parse_number(.data$bin), label = labels[i]) %>% 
      filter(str_detect(.data$par, effect))
    fe <- bind_rows(fe, df)
  }
  
  p <- ggplot(data = fe, aes(x = .data$bin, y = .data$Estimate, colour = .data$label)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    geom_pointrange(aes(ymin = .data$Q2.5, ymax = .data$Q97.5), alpha = 0.75) +
    facet_wrap(label ~ .) +
    labs(x = xlab, y = "Coefficient") +
    theme(legend.position = "none")  
  
  return(p)
}
