
#' Find Optimal Sample Size
#'
#' This function uses a binary search algorithm to find the optimal sample size required to achieve a specified power
#' for a statistical test. The function takes the optimal sample size from the range of sample size values provided by the binary
#' search helper function.
#'
#' @param np Integer. Lower bound of the sample size search range (default: 0).
#' @param ni Integer. Upper bound of the sample size search range (default: 2000).
#' @param dsearch Numeric. Precision of the binary search. Larger values lead to finer resolution and more iterations (default: 1).
#' @param power_calc_fn Function. The power-calculating function within the environment that the user should already have.
#' The results should be in a named list with the power value listed under power "return(list(power=final_power))".
#' @param proportion_vector Numeric vector. Proportion of individuals allocated to each basket/arm in multi-arm trials (default: 1).
#' @param pwr Numeric. Nominal/expected power (e.g., 0.8 for 80\% power).
#' @param small_iterations Integer. Specifies the number of simulations to use in the initial (coarse) binary search if a simulation-based power function is provided.
#' @param large_iterations Integer. Specifies the number of simulations to use in the subsequent (precise) search if a simulation-based power function is provided.
#'
#' @return Numeric. The optimal sample size.
#'
#' @examples
#' \dontrun{
#' find_sample_size(
#'   np = 10,
#'   ni = 1000,
#'   dsearch = 5,
#'   power_calc_fn = function(total_individuals, ...) {
#'     return(list(power = 0.8))
#'   },
#'   proportion_vector = 1,
#'   pwr = 0.8
#' )}
#' @export
#' @author
#' Desmond Zeya Chen (Contact via GitHub Issues)
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
    iterations = if (pass_iterations) small_iterations else NULL, # assign smaller iterations to binary search algorithm for efficiency
    ...
  )

  # while loop to search through the increment with larger iterations for precision
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
