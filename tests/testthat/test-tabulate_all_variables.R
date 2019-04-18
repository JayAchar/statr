context("test-tabulate_all_variables")

new_var_names <- c(
  "Sepal Length",
  "Sepal Width",
  "Petal Length",
  "Petal Width",
  "Species"
)

output <- tabulate_all_variables(
  iris,
  unique_value_threshold = 5,
  variable_names = new_var_names
)

test_that("standard works", {
  expect_equal(names(output), c("var_name",
                                "lvl_names",
                                "values"))
  expect_equal(length(unique(output$var_name)), 6)
  expect_equal(as.character(sapply(output, typeof)), 
               rep("character", 3))
})


# add NA to categorical and continuous variable
iris_na <- iris
iris_na[1, 1] <- NA_integer_
iris_na[1, 5] <- NA_integer_
output_na <- tabulate_all_variables(iris_na,
                                    unique_value_threshold = 5,
                                    variable_names = new_var_names,
                                    na.rm = TRUE)

test_that("working with NAs", {
  expect_equal(length(unique(output$var_name)), 6)
  expect_equal(as.character(sapply(output, typeof)), 
               rep("character", 3))
  expect_equal(nrow(output_na), 8)
})

