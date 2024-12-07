
#' Recursive Binary Search for Sample Size Range
#'
#' This is a helper function that performs a binary search using recursion to find a range of sample sizes
#' that meet a specified power criterion. This binary search function helps narrow down the range of possible sample sizes.
#'
#' @param np Integer. Lower bound of the sample size range we are searching through.
#' @param ni Integer. Upper bound of the sample size range we are searching through.
#' @param power_calc_fn Function. The power-calculating function within the environment that the user should already have.
#' The results should be in a named list with the power value listed under power "return(list(power=final_power))".
#' @param dsearch Numeric. Precision of the binary search. Larger values lead to finer resolution and more iterations.
#' @param pwr Numeric. Nominal/expected power (e.g., 0.8 for 80\% power).
#' @param p Numeric. The current iterate power value calculated at the midpoint sample size.
#' @param n Integer. The current iterate midpoint sample size.
#'
#' @return A vector of sample sizes within the range `[np, ni]` that meet the specified precision.
#'
#' @examples
#' \dontrun{
#' n_rec(
#'   np = 10,
#'   ni = 1000,
#'   power_calc_fn = function(total_individuals, ...) {
#'     return(list(power = 0.8))
#'   },
#'   dsearch = 5,
#'   pwr = 0.8,
#'   p = 0.75,
#'   n = 500
#' )}
#' @export
#' @author
#' Desmond Zeya Chen (Contact via GitHub Issues)
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
  if (ni - np <= dsearch) { # return the increment of sample size from binary search
    return(c(np:ni))
  } else if (p > pwr) { # set upper bound to current iterates and search smaller side
    ni <- n
    n <- ceiling((np + ni) / 2)
    p <- power_calc_fn(total_individuals = n, ...)$power
    n_rec(np = np, ni = ni, power_calc_fn = power_calc_fn, dsearch = dsearch, pwr = pwr, p = p, n = n, ...)
  } else if (p < pwr) { # set lower bound to current iterates and search larger side
    np <- n
    n <- ceiling((np + ni) / 2)
    p <- power_calc_fn(total_individuals = n, ...)$power
    n_rec(np = np, ni = ni, power_calc_fn = power_calc_fn, dsearch = dsearch, pwr = pwr, p = p, n = n, ...)
  }
}
