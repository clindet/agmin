#' Draw bar percent plot with logistic regression
#'
#' @examples
#' age <- data.frame(age=rgamma(10000, shape = 50), groups = "Age-50")
#' age$age_cut <- ag_cut(age$age)
#' age$os <- c(rgamma(2000, shape = 160), rgamma(2000, shape = 320),
#'   rgamma(4000, shape = 560), rgamma(2000, shape = 800))
#' age$os_status <- round(rgamma(10000, shape = 720) %% 2)
#' age$os_status[age$os_status != 0] <- 1
#' age$facet_var <- round(rgamma(10000, shape = 720) %% 2)
#' age$facet_var[age$facet_var != 0] <- 1
#' vis_bar_logic(age, "os_status")
#' @export
vis_bar_logic <- function (x, factor_var, age_cut = "age_cut", facet_var = NULL, title_prefix = "", xlab = "Age groups",
                           ylab = "Percent", digits = 3, palt = ag_col(theme = "pal_npg"), ...) {

  if (is.null (facet_var)) {
    x2 <- x
    x2[,age_cut] <- as.numeric(x2[,age_cut])
    logistic <- eval(parse(text = sprintf("glm(%s ~ %s,
                      data = x2, family = binomial())", factor_var, age_cut)))
    res <- summary(logistic)
    z_val <- round(res$coefficients[2,3], digits = digits)
    p_val <- round(res$coefficients[2,4], digits = digits)
    p <- suppressMessages(
      do.call(ggbarstats, list(data= x,
        x = factor_var,
        y = age_cut,
        title = sprintf("%s%s ~ %s: Z-value:%s; P-value: %s",
                           title_prefix, age_cut, factor_var, z_val, p_val),
        results.subtitle = FALSE,
        ggplot.component = list(
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)))
        ),
        ...
      ) +
      xlab (xlab) + ylab(ylab) +
      theme(panel.grid=element_blank()) +
      scale_fill_manual(values = palt)
    )
    return(p)
  }

  if (is.factor(x[,facet_var])) {
    p <- NULL
    for (i in levels(x[,facet_var])) {
      ptmp <- vis_bar_logic(x[x[,facet_var] == i,], factor_var, age_cut = age_cut,
                            title_prefix = paste0(facet_var, "-", i, ": "),
                            xlab = xlab,
                            ylab = ylab, digits = digits, ...)
      if (is.null(p)) {
        p <- ptmp
      } else {
        p <- p + ptmp
      }
    }
  } else {
    p <- NULL
    for (i in unique(x[,facet_var])) {
      ptmp <- vis_bar_logic(x[x[,facet_var] == i,], factor_var, age_cut = age_cut,
                            title_prefix = paste0(facet_var, "-", i, ": "),
                            xlab = xlab,
                            ylab = ylab, digits = digits, ...)
      if (is.null(p)) {
        p <- ptmp
      } else {
        p <- p + ptmp
      }
    }
  }
  return (p)
}

