#' Tabulate categorical variable
#' 
#' @usage tabulate_categorical(x,
#' variable_name, 
#' round_digits = 1,
#' useNA = c("ifany", "always", "no"))
#'
#' @param x variable object for tabulation
#' @param variable_name String variable name to include in first column of output table.
#' @param round_digits integer to define how many digits to round the percentage
#' @param useNA whether to include NA values in the table. See ‘Details’. Can be abbreviated.
#'
#' @details \code{useNA} controls if the table includes counts of NA values: the allowed values correspond 
#' to never ("no"), only if the count is positive ("ifany") and even for zero counts ("always"). 
#' @author Jay Achar
#' @return data frame with one row per unique value within the input variable
#' and columns containing the unique values and their counts with associated percentages
#' @export 
#' @importFrom assertthat assert_that
#' @seealso \code{\link{statr}}
#' @examples
#' tabulate_categorical(mtcars$vs, variable_name = "VS")

tabulate_categorical <- function(x,
                                 variable_name,
                                 round_digits = 1,
                                 useNA = c("ifany", "always", "no")) {
  # arg cheks
  assert_that(is.numeric(round_digits),
              is.character(variable_name))
  useNA <- match.arg(useNA)

  # tabulate variable
  tab <- table(x, useNA = useNA)
  
  # record unique levels of categorical variable
  lvl_names <- names(tab)
  
  # calculate proportios
  prop <- prop.table(tab)
  
  # convert proportion to percentage and round
  prop_clean <- round_percent(prop * 100, round_digits)
  
  # combine count with proportion
  combo <- paste0(tab, " (", prop_clean, ")")
  
  # generate data frame with variable levels, counts and proportions
  df <- data.frame(
    var_name = c(variable_name, rep("", length(tab) - 1)),
    lvl_names = lvl_names,
    values = combo,
    stringsAsFactors = FALSE
  )
  
  df
}
