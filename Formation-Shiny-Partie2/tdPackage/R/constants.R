#' Physico-Chemical constants
#'
#' @importFrom units set_units
#'
#' @export
const_chem <- function() {
  L = list(
    Atomic.mass = 1.66053906660 * 10^-24,
    Avogadro.number = 6.022142 * 10^23,
    Boltzmann.constant = 1.380651 * 10^-23
  )
  return(L)
}
