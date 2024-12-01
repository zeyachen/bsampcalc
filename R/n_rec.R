

n_rec <- function(
    np,
    ni,
    power_calc_fn,
    dsearch = 1,
    pwr,
    p,
    n,
    ...
) {
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
