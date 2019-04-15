#' Round vector of number to percentages
#'
#' @param x A numeric vector with non-negative values.
#' @param decimals An integer giving the number of decimals that are used
#' @param ties A string that is either 'random' (the default) or 'last'. 
#' Determines how to break ties. Random is random, last prefers to break ties at the last position
#'
#' @return Returns a vector of numeric values of the same length as x.
#' @author Jay Achar
#' @export
#' @seealso \code{\link{statr}}
#'
#' @examples
#' round_percent(x = c(10, 10, 20), 0L)

round_percent <- function (x,
                           decimals = 0L,
                           ties = c("random", "last")) {
  ties <- match.arg(ties)
  if (!is.numeric(x)) {
    stop("only works on numeric vectors")
  }
  if (min(x) < 0) {
    stop("only works on non-negative vectors")
  }
  if (decimals < 0) {
    stop("number of decimals should be a non-negative integer")
  }
  decimals <- as.integer(decimals)
  multiplier <- 10 ^ (2 + decimals)
  x <- x / sum(x) * multiplier
  res <- floor(x)
  rsum <- sum(res)
  if (rsum < multiplier) {
    tiebreaker <- switch(ties,
                         random = sample(length(x)),
                         last = seq(length(x)))
    o <- order(x %% 1, tiebreaker, decreasing = TRUE)
    res[o[1:(multiplier - rsum)]] <- res[o[1:(multiplier -
                                                rsum)]] + 1
  }
  res / (10 ^ decimals)
}