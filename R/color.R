#' Get age groups colors
#'
#' @param n Colors size. Maximum N is 7.
#' @examples
#' ag_col()
#'
#' @export
ag_col <- function (n = 7, theme = "age_cut") {
  cols <- list(
    age_cut = c("#AFb1d3", "#40b1d3", "#80b1d3", "#8dd3c7",
               "#1f78b4", "#8C564B", "#ff0000"),
    diagnosis = c("#FB8072", "#8DD3C7", "#80B1D3","#998199","#8C564B","#9467BD", "#e78ac3", "#e5c494")
  )
  col <- cols[[theme]]
  if (is.null(col)) {
    col <- do.call(theme, list())(n)
  }
  len <- length(col)
  lenplus1 <- len + 1
  return (col[(lenplus1-n):len])
}
