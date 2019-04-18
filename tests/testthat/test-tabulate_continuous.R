context("test-tabulate_continuous")

output <- tabulate_continuous(mtcars$mpg, 
                              variable_name = "MPG",
                              round_digits = 1)

test_that("clean continuous vector works", {
  expect_true(all(dim(output) == c(1, 3)))
  expect_true(all(
    names(output) == c("var_name", "lvl_names", "values")
  ))
  expect_equal(output$lvl_names, "")
  expect_equal(class(output), "data.frame")
  expect_equal(as.character(sapply(output, typeof)), 
               c("character", "character", "character"))
})

# incorporate NA input values
mpg_na <- mtcars$mpg
mpg_na[3] <- NA_integer_

test_that("error handling", {
  expect_error(tabulate_continuous(mpg_na, variable_name = "MPG"))  
  expect_error(tabulate_continuous(mtcars$mpg,
                                   variable_name = "MPG",
                                   round_digits = z))
}) 

test_that("NA handling", {
  expect_equal(class(tabulate_continuous(
    mpg_na,
    variable_name = "MPG",
    na.rm = TRUE
  )),
  "data.frame")
})
