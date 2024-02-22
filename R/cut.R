#' A simple method to set age groups by intervals
#'
#' @param x A numeric vector.
#' @examples
#' age <- 1:100
#' age_cut(age)
#' @export
age_cut <- function(x, low.breaks = 19, high.breaks = 70, interval = 10,
                    to_char = "-") {
  right <- TRUE
  include.lowest <- FALSE
  x <- str_remove_all(x, "| |years|y|岁|[a-zA-Z]|#|@|!|$|%|^|&|_|=")
  x <- as.numeric(x)
  breaks <- seq(low.breaks, high.breaks, interval)
  x2 <- cut(round(x), breaks,
      include.lowest = include.lowest, right = right)
  if (breaks[length(breaks)] != high.breaks) {
    high.breaks <- breaks[length(breaks)] + 1
  }
  if (!include.lowest) {
    if (right) {
      x2 <- factor(x2, levels = c(paste0("<", low.breaks + 1), levels(x2), paste0(">=", high.breaks)))
      x2[is.na(x2) & x <= low.breaks] <- paste0("<", low.breaks + 1)
      x2[is.na(x2) & x >= high.breaks] <- paste0(">=", high.breaks)
    } else {
      x2 <- factor(x2, levels = c(paste0("<=", low.breaks - 1), levels(x2), paste0(">=", high.breaks - 1)))
      x2[is.na(x2) & x <= low.breaks - 1] <- paste0("<=", low.breaks - 1)
      x2[is.na(x2) & x >= high.breaks - 1] <- paste0(">=", high.breaks - 1)
    }
  } else {
    if (right) {
      x2 <- factor(x2, levels = c(paste0("<", low.breaks), levels(x2), paste0(">", high.breaks)))
      x2[is.na(x2) & x < low.breaks] <- paste0("<", low.breaks)
      x2[is.na(x2) & x > high.breaks] <- paste0(">", high.breaks)
    } else {
      x2 <- factor(x2, levels = c(paste0("<", low.breaks - 1), levels(x2), paste0(">", high.breaks - 1)))
      x2[is.na(x2) & x < low.breaks - 1] <- paste0("<", low.breaks - 1)
      x2[is.na(x2) & x > high.breaks - 1] <- paste0(">", high.breaks - 1)
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
  return(x3)
}

