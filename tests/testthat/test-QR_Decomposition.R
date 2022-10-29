testthat::test_that("correct result",{
  library(MASS)
  testthat::expect_equal(ridgereg_QR(Petal.Length~Species, data =iris,lam=3),lm.ridge(Petal.Length~Species, data =iris,lam=3))
  testthat::expect_success(ridgereg_QR(Petal.Length~Species, data =iris,lam=3))
  testthat::expect_success(ridgereg_QR(Petal.Length~Species, data =iris,lam=4))

})


testthat::test_that("Wrong results",{
  testthat::expect_error(ridgereg_QR(formula,iris,a))
  testthat::expect_error(ridgereg_QR(formula,iris,"a"))
  testthat::expect_error(ridgereg_QR(formula,iris,TRUE))
  testthat::expect_error(ridgereg_QR(formula,hello,3))

})
