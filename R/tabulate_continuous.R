#' Tabulate continuous variable
#'
#' @param x Variable object for tabulation.
#' @param variable_name String variable name to include in first column of output table.
#' @param round_digits Integer to define how many digits to round the percentage.
#' @param ... Additional arguments to be passed to nested functions (e.g. na.rm = TRUE).
#'
#' @author Jay Achar
#' @return data frame with one row per unique value within the input variable
#' and columns containing the unique values and their counts with associated percentages
#' @importFrom assertthat assert_that
#' @importFrom stats median quantile
#' @seealso \code{\link{statr}}
#' @export
#'
#' @examples
#' tabulate_continuous(mtcars$mpg, variable_name = "MPG")

tabulate_continuous <- function(x,
                                variable_name,
                                round_digits = 1, 
                                ...) {
  # check args
  assert_that(is.numeric(round_digits),
              is.character(variable_name))
  
  # calculate median
  med <- round(median(x, ...), round_digits)
  
  # calculate IQR values
  p25 <- round(quantile(x, 0.25, ...), round_digits)
  p75 <- round(quantile(x, 0.75, ...), round_digits)
  
  # generate string with IQR values
  iqr_string <- paste0(" (", p25, "-", p75, ")")
  
  # generate output
  output <- paste0(med, iqr_string)
  
  # generate output data frame
  df <- data.frame(lvl_names = "",
                   values = output,
                   stringsAsFactors = FALSE)
  df
}
