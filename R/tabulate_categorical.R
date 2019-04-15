#' Tabulate categorical variable
#'
#' @param x variable object for tabulation
#' @param round_digits integer to define how many digits to round the percentage
#'
#' @author Jay Achar
#' @return data frame with one row per unique value within the input variable
#' and columns containing the unique values and their counts with associated percentages
#' @export
#' @importFrom assertthat assert_that
#' @seealso \code{\link{statr}}
#' @examples
#' tabulate_categorical(mtcars$vs)

tabulate_categorical <- function(x,
                                 round_digits = 1) {
  # arg cheks
  assert_that(is.numeric(round_digits))

  # record unique levels of categorical variable
  lvl_names <- unique(x)
  
  # tabulate variable
  tab <- table(x)
  
  # calculate proportios
  prop <- prop.table(tab)
  
  # convert proportion to percentage and round
  prop_clean <- round_percent(prop * 100, round_digits)
  
  # combine count with proportion
  combo <- paste0(tab, " (", prop_clean, ")")
  
  # generate data frame with variable levels, counts and proportions
  df <- data.frame(lvl_names = lvl_names,
                   values = combo,
                   stringsAsFactors = FALSE)
  
  df
}
