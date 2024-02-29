#' A simple method to set age groups by intervals
#'
#' @param x Any vector.
#' @param low_breaks Determine the first age group.
#' @param high_breaks Determine the last age group.
#' @param interval Determine the intervals of age groups.
#' @param to_char Set the connector of age groups (start and end) excluding the first and last.
#' @examples
#' age <- c(1:100, 1:100)
#' ag_cut(age)
#' @export
ag_cut <- function(x, low_breaks = 19, high_breaks = 70, interval = 10,
                    to_char = "-") {
  right <- TRUE
  include.lowest <- FALSE
  x <- str_remove_all(x, "| |years|y|岁|[a-zA-Z]|#|@|!|$|%|^|&|_|=")
  x <- as.numeric(x)
  breaks <- seq(low_breaks, high_breaks, interval)
  x <- round(x, digits = 0)
  x2 <- cut(round(x), breaks,
      include.lowest = include.lowest, right = right)
  if (breaks[length(breaks)] != high_breaks) {
    high_breaks <- breaks[length(breaks)] + 1
  }
  if (!include.lowest) {
    if (right) {
      x2 <- factor(x2, levels = c(paste0("<", low_breaks + 1), levels(x2), paste0(">=", high_breaks)))
      x2[is.na(x2) & x <= low_breaks] <- paste0("<", low_breaks + 1)
      x2[is.na(x2) & x >= high_breaks] <- paste0(">=", high_breaks)
    } else {
      x2 <- factor(x2, levels = c(paste0("<=", low_breaks - 1), levels(x2), paste0(">=", high_breaks - 1)))
      x2[is.na(x2) & x <= low_breaks - 1] <- paste0("<=", low_breaks - 1)
      x2[is.na(x2) & x >= high_breaks - 1] <- paste0(">=", high_breaks - 1)
    }
  } else {
    if (right) {
      x2 <- factor(x2, levels = c(paste0("<", low_breaks), levels(x2), paste0(">", high_breaks)))
      x2[is.na(x2) & x < low_breaks] <- paste0("<", low_breaks)
      x2[is.na(x2) & x > high_breaks] <- paste0(">", high_breaks)
    } else {
      x2 <- factor(x2, levels = c(paste0("<", low_breaks - 1), levels(x2), paste0(">", high_breaks - 1)))
      x2[is.na(x2) & x < low_breaks - 1] <- paste0("<", low_breaks - 1)
      x2[is.na(x2) & x > high_breaks - 1] <- paste0(">", high_breaks - 1)
    }
  }
  c_flag <- str_detect(x2, ",")
  p_flag <- str_detect(x2, "^[(]")
  m_flag <- str_detect(x2, "[)]$")
  x2 <- str_remove_all(x2, "[(]|[)]")
  x2 <- str_remove_all(x2, fixed("]"))
  x2 <- str_remove_all(x2, fixed("["))
  x2_c <- str_split(x2, ",")
  x2_c[p_flag] <- lapply(x2_c[p_flag], function(x) {
    c(as.numeric(x[1]) + 1, x[2])
  })
  x2_c[m_flag] <- lapply(x2_c[m_flag], function(x) {
    c(x[1], as.numeric(x[2]) - 1)
  })
  x3 <- sapply(x2_c, function(x) {
    if (length(x) == 1) {
      return (x)
    } else {
      paste0(x[1], to_char, x[2])
    }
  })
  x3_u <- unique(x3)
  x3_u <- sort(x3_u)
  x3_u_inter <- x3_u[!str_detect(x3_u, ">|>=|<|<=")]
  x3 <- factor(x3, levels = c(x3_u[str_detect(x3_u, "<|<=")], x3_u_inter,
                              x3_u[str_detect(x3_u, ">|>=")]))
  return(x3)
}

