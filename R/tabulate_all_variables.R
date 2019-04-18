#' Tabulate data frame summary
#'
#' @param x data frame
#' @param unique_value_threshold Integer defining the number of unique 
#' values above which a variable is analysed as continuous
#' @param variable_names Character string to define variable names within output.
#' Length must be identical to the number of variables. 
#' @param ... Additional arguments to be passed to nested functions (e.g. na.rm = TRUE).
#'
#' @author Jay Achar
#' @importFrom assertthat assert_that
#' @importFrom purrr map_int map2
#' @importFrom dplyr bind_rows %>% 
#' @seealso \code{\link{statr}}
#' @return Data frame of summary values. Counts and percentages for categorical
#' variables and median and IQR for continuous variables
#' @export
#'
#' @examples
#' tabulate_all_variables(iris,
#' unique_value_threshold = 6,
#' variable_names = c("Sepal Length", "Sepal Width", "Petal Length",
#' "Petal Width", "Species"))

tabulate_all_variables <- function(x,
                                   unique_value_threshold = 6, 
                                   variable_names = NULL,
                                   ...) {
  
  # arg checks
  assert_that(is.data.frame(x),
              is.numeric(unique_value_threshold), 
              unique_value_threshold > 0)
  
  # count how many unique values in each variable
  unique_vals <- purrr::map_int(x, .f = ~ length(unique(.x)))
  
  # convert to logical based on unique value threshold
  cat_cont <- ifelse(unique_vals >= unique_value_threshold, 
                     TRUE, 
                     FALSE)
  
  # use variable_names arg if present
  if (is.null(variable_names)) {
    cont_vars <- names(x[, cat_cont, drop = FALSE])
    cat_vars <- names(x[, !cat_cont, drop = FALSE])
  } else {
    assert_that(length(variable_names) == ncol(x))
    cont_vars <- variable_names[cat_cont]
    cat_vars <- variable_names[!cat_cont]
  }
  
  # run tabulate continuous on variables with high numbers of unique values
  # use raw variable names
  cont <- purrr::map2(x[ , cat_cont, drop = FALSE], 
                      cont_vars,
                      .f = function(data, name) {tabulate_continuous(x = data, 
                                                 variable_name = name, 
                                                 ...)})
  
  # run tabulate continuous on variables with high numbers of unique values
  # use raw variable names
  cat <- purrr::map2(x[ , !cat_cont, drop = FALSE], 
                     cat_vars,
                     .f = ~ tabulate_categorical(x = .x, variable_name = .y))
  
  # combine all tables to produce data frame of results
  full_df <- dplyr::bind_rows(cont) %>%
    dplyr::bind_rows(cat)
  
  full_df
  
}
