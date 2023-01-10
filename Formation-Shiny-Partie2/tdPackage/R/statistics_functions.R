#' Function to display descrptives statistics of a dataframe
#'
#' THis function takes two arguments, a vector x and the logical na.rm
#' and return numeric summary of data
#'
#' @param x A vector of numeric values
#' @param na.rm A boolean value TRUE or FALSE
#'
#' @return a dataframe contains one row and 4 columns min, median, sd, max
#' @author Josue AFOUDA
#' @export
#'
#' @importFrom stats median
#' @examples
#' # Example usage of numeric_summary
#' numeric_summary(airquality$Ozone, na.rm = TRUE)
numeric_summary <- function(x, na.rm) {

  # Include an error if x is not numeric
  if(!is.numeric(x)){
    stop("Data must be numeric")
  }

  # Create data frame
  data.frame( min = min(x, na.rm = na.rm),
              median = median(x, na.rm = na.rm),
              sd = sd(x, na.rm = na.rm),
              max = max(x, na.rm = na.rm))
}
