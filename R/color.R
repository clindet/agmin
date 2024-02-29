#' Get age groups colors
#'
#' @param n Colors size. Maximum N is 7.
#' @examples
#' ag_col()
#'
#' @export
ag_col <- function (n = 7) {
  if (n > 7) n <- 7
  cols <- c("#AFb1d3", "#40b1d3", "#80b1d3", "#8dd3c7",
               "#1f78b4", "#8C564B", "#ff0000")
  return (cols[(8-n):7])
}
