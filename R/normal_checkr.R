#' Normal Distribution checker
#'
#' Tool to test the distribution of a numerical variable
#' and checks it against the Normal distribution. THe input
#' must be a numerical vector and the output is a list.
#' @param x a numerical vector
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{statr}}
#' @importFrom nortest ad.test
#' @importFrom ggpubr ggdensity ggqqplot
#' @importFrom stats shapiro.test
#' @export
#' @examples
#' 
#' normal_checkr(mtcars$mpg)
#' 

normal_checkr <- function(x)
{

# check variable is numerical
	if (! is.numeric(x)) {
		stop("Vector must be numeric class")
	}

# summary statistics
summ <- summary(x)

# plot distribution density
density <- ggdensity(x)

# qqplot
qq <- ggqqplot(x)

# Anderson Darling normality test
ad_test <- ad.test(x)

# Shapiro-Wilks normality test
shap_test <- shapiro.test(x)

# output list
y <- list(
					summary = summ,
					density = density,
					qq_plot = qq,
					ad_test = ad_test,
					shap_test = shap_test)
y
}