# Author: Desmond Chen
# Date: 2024-11-22
# np: lower bound of sample size we are searching through.
# ni: upper bound of sample size we are searching through.
# power_calc_fn: the power calculating function within the environment, make sure the results are in a named list with power value listed under power "return(list(power=final_power))". 
# dsearch: sets the stopping precision for the binary search; larger values result in finer resolution and more iterations. 
# pwr: nominal/expected power.
# proportion_vector: for platform/basket trials only, proportion of each basket/arm. 

# Helper function: binary search with recursion
n_rec <- function(np, ni, power_calc_fn, dsearch = 5, pwr, p, n, ...) {
  if (ni - np <= dsearch) {
    return(c(np:ni))
  } else if (p > pwr) {
    ni <- n
    n <- ceiling((np + ni) / 2)
    p <- power_calc_fn(total_individuals = n, ...)$power
    n_rec(np = np, ni = ni, power_calc_fn = power_calc_fn, dsearch = dsearch, pwr = pwr, p = p, n = n, ...)
  } else if (p < pwr) {
    np <- n
    n <- ceiling((np + ni) / 2)
    p <- power_calc_fn(total_individuals = n, ...)$power
    n_rec(np = np, ni = ni, power_calc_fn = power_calc_fn, dsearch = dsearch, pwr = pwr, p = p, n = n, ...)
  }
}

# Main function: sample size finding
find_sample_size <- function(np = 0, ni = 2000, dsearch = 5, power_calc_fn, proportion_vector = 1, pwr, ...) {
  n <- ceiling((np + ni) / 2)
  p1 <- power_calc_fn(total_individuals = n, ...)$power
  x <- n_rec(np = np, ni = ni, power_calc_fn = power_calc_fn, dsearch = dsearch, pwr = pwr, p = p1, n = n, ...)
  p <- 0
  i <- 1
  while (p < pwr & i < dsearch) {
    p <- power_calc_fn(total_individuals = x[i], ...)$power
    i <- i + 1
  }
  return(ceiling(x[i] * proportion_vector))
}

