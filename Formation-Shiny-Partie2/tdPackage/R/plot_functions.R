globalVariables(c("fit", "se.fit"))

#' Method for plotting linear models
#'
#' Simple method to plot a linear model using ggplot
#' along with 95% confidence intervals.
#'
#' @param data The linear model object from \code{lm}
#' @param mapping Regular mapping, see \code{ggplot} and \code{aes} for details.
#' @param vars A list of variable values used for prediction.
#' @param \ldots Additional arguments passed to \code{ggplot}
#' @return A ggplot class object.
#' @export
#' @importFrom stats predict qnorm
#' @import ggplot2
ggplot.lm <- function(data, mapping, vars, ...) {
  newdat <- do.call(expand.grid, vars)
  yvar <- as.character(formula(data)[[2]])
  d <- as.data.frame(predict(data, newdata = newdat, se.fit = TRUE))
  d <- within(d, {
    LL <- fit + qnorm(.025) * se.fit
    UL <- fit + qnorm(.975) * se.fit
  })
  colnames(d)[1] <- yvar
  data <- cbind(newdat, d[, c(yvar, "LL", "UL")])
  ggplot(data = data, mapping = mapping, ...)
}

