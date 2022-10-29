testthat::test_that("right exceptions are thrown",{
  testthat::expect_error(Ridgereg(formula,iris,a))
  testthat::expect_error(Ridgereg(formula,iris,"a"))
  testthat::expect_error(Ridgereg(formula,iris,TRUE))
  testthat::expect_error(Ridgereg(formula,hello,3))

})

