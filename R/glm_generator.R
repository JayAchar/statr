#' GLM generator
#'
#' Takes a string of formulas and trains GLM models to output
#' coefficients, diagnostic parameters and predicted values in a tibble. 
#' @param x string of formulas
#' @param data name of data object
#' @param family define type of GLM required
#' @author Jay Achar
#' @return nested tibble 
#' @importFrom purrr compose partial map map_dbl
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom broom glance tidy augment
#' @importFrom stats glm as.formula gaussian
#' @importFrom magrittr %>% 
#' @seealso \code{\link{statr}}
#' @export
#'
#' @examples
#' formulas <- c("mpg ~ hp", "mpg ~ cyl", "mpg ~ wt")
#' glm_generator(x = formulas, data = mtcars, family = gaussian)


glm_generator <- function(x, data, family = gaussian) {
  
  # family <- enquo(family)
  
  mod_f <- compose(partial(glm, data = data, family = family), 
                   as.formula)
  
  output <- tibble::tibble(formula = x, 
                           all = map(.data$formula, mod_f),
                           gla = .data$all %>% map(glance),
                           tid = .data$all %>% map(tidy),
                           aug = .data$all %>% map(augment),
                           bic = map_dbl(.data$gla, "BIC"), 
                           N = map_dbl(.data$aug, ~dim(.x)[1]))
  
  output
}
