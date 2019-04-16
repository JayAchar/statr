context("test-tabulate_categorical")

output <- tabulate_categorical(mtcars$vs, 
                               variable_name = "VS",
                               round_digits = 1)

test_that("clean categorical vector works", {
  expect_true(all(dim(output) == c(2, 3)))
  expect_true(all(
    names(output) == c("var_name", "lvl_names", "values")
  ))
  expect_equal(output$lvl_names, c("0", "1"))
  expect_equal(class(output), "data.frame")
  expect_equal(as.character(sapply(output, typeof)), 
               c("character", "character", "character"))
  expect_true(all(dim(tabulate_categorical(mtcars$vs,
                                           variable_name = "VS",
                                           useNA = "always"))),
              c(4, 3))
})


# incorporate NA input values
vs_na <- mtcars$vs
vs_na[3] <- NA_integer_

test_that("error handling", {
  expect_error(tabulate_categorical(vs_na,
                                   round_digits = z))
}) 

test_that("NA handling", {
  expect_equal(dim(tabulate_categorical(vs_na,
                                        variable_name = "VS",
                                        useNA = "no")),
               c(2, 3))
  expect_true(all(names(tabulate_categorical(vs_na,
                                             variable_name = "VS",
                                             useNA = "no")) == c("var_name", "lvl_names", "values")))
  expect_equal(dim(tabulate_categorical(vs_na,
                                        variable_name = "VS",
                                        useNA = "ifany")),
               c(3, 3))
})
