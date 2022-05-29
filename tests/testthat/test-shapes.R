context("Dimensions of shapes")

test_that("matrix manipulation", {
  
  M <- matrix(runif(n = 5 * 10), nrow = 5, ncol = 10)
  
  expect_equal(rowSums(M), rowSums(aggregate_composition_tails(M, lb = 3, ub = 7)))
  
})
