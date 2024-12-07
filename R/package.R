#' @title A Rapid Binary-Search Based Sample Size Finding Algorithm
#' @description
#' State-of-the-art trial designs often make it difficult to analytically solve for the Type I error rate and power. Typically, simulations are required to approximate these values, which avoids complex mathematical derivations. However, this approach involves looping over a range of sample sizes, making the process both computationally expensive and labor-intensive.
#'
#' To address these challenges, we propose a package that uses a binary search-based algorithm for sample size determination, leveraging the monotonic relationship between power and sample size. This approach reduces the time complexity from O(n) to O(log(n)), eliminating the need for exhaustive simulations over a range of sample sizes. The package replaces complex mathematical derivations with a recursive root-finding algorithm, efficiently calculating sample size while minimizing computational resource usage.
#'
#' Our framework is generalized, making it applicable to all power calculation functions, whether closed-form or simulation-based. While the binary search algorithm significantly enhances efficiency, users should be aware that when the number of iterations in simulations is too low, it may break the monotonicity assumption, potentially leading to misleading results. Therefore, it is important to prudently choose the number of iterations to ensure accurate and reliable outcomes. This approach reduces the time complexity for power calculations with closed-form solutions and improves resource use for simulations.
#' @docType package
#' @name bsampcalc
#' @author
#' Desmond Zeya Chen, Abigail McGrory, Aoqi Xie, Uththami Kukathasan, and Hamda Altaf.
NULL
