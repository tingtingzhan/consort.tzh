

#' @title Find Side Box from `orders` Argument
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param orders \link[base]{character} \link[base]{vector}
#' 
#' @param sidebox_pattern \link[base]{regex}
#' 
#' @param ... additional parameters of \link[consort]{consort_plot}
#' 
#' @details
#' Function [consort_plot_sidebox] finds the argument `side_box` of 
#' function \link[consort]{consort_plot}, from the argument of `orders`.
#' 
#' @returns
#' Function [consort_plot_sidebox] returns a \CRANpkg{consort} object.
#' 
#' @export
consort_plot_sidebox <- function(data, orders, sidebox_pattern = '^sidebox_', ...) {
  
  if (!is.character(orders) || anyNA(orders)) stop('currently only supports `orders` of character vector')
  nm <- names(orders)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('illegal `orders` names')
  
  consort_plot(
    data = data, orders = orders, 
    side_box = grep(pattern = sidebox_pattern, x = nm, value = TRUE),
    ...)
  
}