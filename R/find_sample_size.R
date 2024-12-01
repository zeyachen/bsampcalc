

find_sample_size <- function(
    np = 0,
    ni = 2000,
    dsearch = 1,
    power_calc_fn,
    proportion_vector = 1,
    pwr,
    small_iterations = NULL,
    large_iterations = NULL,
    ...
) {

  # log error and stop the function if designated power is not between 0 and 1
  if (!all(pwr < 1, pwr > 0)) {
    stop("ERROR - power should be between 0 & 1")
  }

  # check if the power function is simulation based, if so we assign different iterations to different step
  fn_args <- names(formals(power_calc_fn))
  pass_iterations <- "iterations" %in% fn_args
  n <- ceiling((np + ni) / 2)
  p1 <- if (pass_iterations) {
    power_calc_fn(total_individuals = n, iterations = small_iterations, ...)
  } else {
    power_calc_fn(total_individuals = n, ...)
  }$power

  # get the increment of sample size from binary search
  x <- n_rec(
    np = np,
    ni = ni,
    power_calc_fn = power_calc_fn,
    dsearch = dsearch,
    pwr = pwr,
    p = p1,
    n = n,
    iterations = if (pass_iterations) small_iterations else NULL,
    ...
  )

  # while loop to search through the increment with larger increment for better precision
  p <- 0
  i <- 1
  while (p < pwr & i < length(x)) {
    p <- if (pass_iterations) {
      power_calc_fn(total_individuals = x[i], iterations = large_iterations, ...)
    } else {
      power_calc_fn(total_individuals = x[i], ...)
    }$power
    i <- i + 1
  }
  return(ceiling(x[i] * proportion_vector))
}
