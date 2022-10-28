testthat::test_that("correct result",{
  library(MASS)
  testthat::expect_equal(ridgereg(Petal.Length~Species, data =iris,lam=3),lm.ridge(Petal.Length~Species, data =iris,lam=3))
  testthat::expect_success(ridgereg(Petal.Length~Species, data =iris,lam=3))
  testthat::expect_success(ridgereg(Petal.Length~Species, data =iris,lam=4))

})


testthat::test_that("Wrong results",{
  testthat::expect_error(ridgereg(formula,iris,a))
  testthat::expect_error(ridgereg(formula,iris,"a"))
  testthat::expect_error(ridgereg(formula,iris,TRUE))
  testthat::expect_error(ridgereg(formula,hello,3))

})

