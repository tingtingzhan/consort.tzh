

#' @title rmd_.consort
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param ... ...
#' 
#' @export
rmd_.consort <- function(x, xnm, ...) {
  
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    sprintf(fmt = '```{r results = \'asis\', fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = 'plot(%s)', xnm), # invokes ?consort:::plot.consort
    '```',
    '<any-text>'
  ))
  
}
