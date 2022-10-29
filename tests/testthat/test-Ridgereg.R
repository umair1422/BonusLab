testthat::test_that("Wrong results",{
  testthat::expect_error(Ridgereg(formula,iris,a))
  testthat::expect_error(Ridgereg(formula,iris,"a"))
  testthat::expect_error(Ridgereg(formula,iris,TRUE))
  testthat::expect_error(Ridgereg(formula,hello,3))

})

