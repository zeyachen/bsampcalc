clspwrfn <- function(total_individuals,effsiz, alpha = 0.05,...) {

  t_critical <- qt(1 - alpha, df = total_individuals - 1)
  power1 <- 1 - pt(t_critical - effsiz, df = total_individuals - 1)
  return(list(power=power1))
}

simpwrfn <- function(total_individuals,iterations,effsiz, alpha = 0.05,...) {
  set.seed(999)
  t_critical <- qt(1 - alpha, df = total_individuals - 1)
  power2 <- 1 - pt(t_critical - effsiz, df = total_individuals - 1)*exp(rnorm(1)/iterations)
  return(list(power=power2))
}

test_that("Closed-form t test works", {
  expect_equal(find_sample_size(power_calc_fn=clspwrfn,effsiz=2.5,pwr=0.8004507), 161)
})

test_that("simulation t test works", {
  expect_equal(find_sample_size(power_calc_fn=simpwrfn,effsiz=2.5,pwr=0.8004507,dsearch = 30,small_iterations = 1E4,large_iterations = 1E6), 161)
})

test_that("find_sample_size detects error",{
  expect_error(find_sample_size(power_calc_fn = NULL, pwr = 80), "ERROR - power should be between 0 & 1")
})
