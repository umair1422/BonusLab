testthat::test_that("Wrong results",{
  testthat::expect_error(ridgereg_QR(formula,iris,a))
  testthat::expect_error(ridgereg_QR(formula,iris,"a"))
  testthat::expect_error(ridgereg_QR(formula,iris,TRUE))
  testthat::expect_error(ridgereg_QR(formula,hello,3))

})
