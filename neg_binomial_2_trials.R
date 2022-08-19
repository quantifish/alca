stan_funs <- "
    real neg_binomial_2_trials_lpmf(int[] y, vector mu, real phi, int[] T) {
        int N = size(mu);
        vector[N] mu2;
        vector[N] phi2;
        for (i in 1:N) {
          mu2[i] = mu[i] * T[i];
          phi2[i] = phi * T[i];
        }
        return neg_binomial_2_lpmf(y | mu2, phi2);
    }
    
    int[] neg_binomial_2_trials_rng(vector mu, real phi, int[] T) {
        int N = size(mu);
        vector[N] mu2;
        vector[N] phi2;
        for (i in 1:N) {
          mu2[i] = mu[i] * T[i];
          phi2[i] = phi * T[i];
        }
        return neg_binomial_2_rng(mu2, phi2);
    }
"

stanvars <- stanvar(scode = stan_funs, block = "functions")

log_lik_neg_binomial_2_trials <- function(i, prep){
  mu <- prep$dpars$mu[, i]
  phi <- prep$dpars$phi
  trials <- prep$data$trials[i]
  y <- prep$data$Y[i]
  neg_binomial_2_trials_lpmf(y, mu, phi, trials)
}

posterior_predict_neg_binomial_2_trials <- function(i, prep, ...) {
  mu <- pmax(prep$dpars$mu[, i], 1E-10)
  phi <- pmax(prep$dpars$phi, 1E-10)
  trials <- pmax(prep$data$trials[i], 1)
  return(neg_binomial_2_trials_rng(mu, phi, trials))
}

posterior_epred_neg_binomial_2_trials <- function(prep) {
  return(brms:::posterior_epred_negbinomial2(prep))
}

neg_binomial_2_trials <- custom_family(
  name = "neg_binomial_2_trials", 
  dpars = c("mu", "phi"),
  links = c("log", "log"), 
  type = "int", 
  lb = c(0, 0),
  vars = "trials",
  loop = FALSE,
  log_lik = log_lik_neg_binomial_2_trials,
  posterior_predict = posterior_predict_neg_binomial_2_trials,
  posterior_epred = posterior_epred_neg_binomial_2_trials
)
