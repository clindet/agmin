#' Draw histogram of age.
#'
#' @param x A data.frame. It contains age column at least.
#' @param age_col Column name of age.
#' @param facet_var Column name for facet display.
#' @param binwidth vis_histogram binwidth.
#' @param title Do not work in the facet mode.
#' @param title_median Show median in title.
#' @param title_iqr Show IQR in title.
#' @param digits Set digits.
#' @param add_line Add median or mean line in plots.
#'
#' @examples
#' age <- data.frame(age=rgamma(10000, shape = 50), groups = "Age-50")
#' vis_hist(age)
#' age2 <- data.frame(age=rgamma(10000, shape = 70), groups = "Age-70")
#' age <- rbind(age, age2)
#' vis_hist(age, facet_var = "groups")
#' @export
vis_hist <- function (x, age_col = "age", facet_var = NULL, fill = "#000000", binwidth = 1,
                      add_param = list(title = "All samples", title_median = TRUE,
                      title_iqr = TRUE, digits = 0,
                      add_line = "median", linecol = "gray", linetype = "dashed"),
                      ...) {
  opt <- options()
  options(digits = ifelse(add_param$digits == 0, 1, add_param$digits))
  x <- clean_col_df(x, col = age_col, col_rename = "age", facet_var = facet_var)
  x$fill <- fill
  p <- ggplot(data = x) +
    geom_histogram(aes(x = age),
                   color="white", binwidth = binwidth, fill = fill) + theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
  add_param$x <- x
  add_param$p <- p
  add_param$facet_var <- facet_var
  p <- do.call(add_title_vline, add_param)
  p <- set_font(p, ...) + xlab ("Age") + ylab("Count")
  options(opt)
  return(p)
}

vis_age_title <- function (x, title = "All samples", age_col = "age",
                            digits = 0, title_median = TRUE, title_iqr = TRUE,
                            return_string = FALSE) {
  idx <- which(colnames(x) == age_col)
  colnames(x)[idx] <- "age"
  title_extra <- c(
    ifelse(title_median, paste0("Median: ", round(median(x$age, na.rm = TRUE), digits = digits)), ""),
    ifelse(title_iqr,
           paste0("IQR: ",
                  round(as.numeric(summary(x$age)["1st Qu."]), digits = digits),
                  "-",
                  round(as.numeric(summary(x$age)["3rd Qu."]), digits = digits)
           ),
           ""
    )
  )
  title_extra <- title_extra[title_extra != ""]
  title_extra <- paste0(title_extra, collapse = "; ")
  if (any(c(title_median, title_iqr))) {
    title_extra <- paste0(" (", title_extra, ")")
  }
  if (return_string) {
    return (sprintf("%s%s", title, title_extra))
  }
  return(ggtitle(sprintf("%s%s", title, title_extra)))
}

vis_age_title_facet <- function (labs, x, facet_var, ...) {
  for(i in 1:length(labs)) {
    labs[i] <- vis_age_title(x[x[,facet_var] == labs[i],],
                             title = sprintf("%s-%s", facet_var, labs[i]), ...)
  }
  return(unlist(labs))
}

add_title_vline <- function (x, p = NULL, facet_var = NULL,
                             facet_mode = "hist", age_col = "age",
                             title = "All samples", title_median = TRUE,
                             title_iqr = TRUE, digits = 0, add_line = "median",
                             linecol = "gray", linetype = "dashed") {
  if (is.null(facet_var)) {
    p <- p + vis_age_title(x, title = title, age_col = age_col, digits = digits,
                           title_median = title_median, title_iqr = title_iqr)
  }

  if (is.null(facet_var) && facet_mode == "hist") {
    if (add_line %in% c("median", "mean")) {
      v <- eval(parse(text = sprintf("round(%s(x$age, na.rm = TRUE), digits = digits)", add_line)))
      p <- p + geom_vline(xintercept = v,
                          color = linecol, linetype = linetype)
    }
    return(p)
  } else if (is.null(facet_var)) {
    return(p)
  }

  labs <- levels(x[,facet_var])
  labs <- vis_age_title_facet(labs, x, facet_var = facet_var, age_col = age_col, digits = digits,
                              title_median = title_median, title_iqr = title_iqr)
  names(labs) <- levels(x[,facet_var])
  parms <- list(labs)
  if (facet_mode == "bar") {
    x2 <- as.data.frame(table(x[,facet_var], x$age_cut))
    names(parms) <- "Var1"
    p <- p + facet_wrap(~Var1,
                        labeller = do.call(labeller, parms))
    return(p)
  }

  names(parms) <- facet_var
  p <- p + facet_wrap(as.formula(paste("~", facet_var)),
                      labeller = do.call(labeller, parms))
  res_stat <- lapply(levels(x[,facet_var]), function(g) {
    x <- x$age[x[,facet_var] == g]
    return(c(median=round(median(x, na.rm = TRUE), digits = digits),
             mean=round(mean(x, na.rm = TRUE), digits = digits)))
  })

  x2 <- data.frame(group = levels(x[,facet_var]),
                   median = sapply(res_stat, function(x) {return (x["median"])}),
                   mean = sapply(res_stat, function(x) {return (x["mean"])}))
  colnames(x2)[1] <- facet_var
  if (is.numeric(add_line)) {
    p <- p + geom_vline(xintercept = add_line, color = linecol, linetype = linetype)
  } else {
    p <- p + geom_vline(data = x2, eval(parse(text = sprintf("aes(xintercept = %s)", add_line))),
                        color = linecol, linetype = linetype)
  }
  return(p)
}

set_font <- function (p, fontfamily = "Arial",
                      fontface = NULL, fontsize = NULL, fontcolor = NULL) {
  p <- p + theme(text = element_text(family = fontfamily,
                                     face = fontface,
                                     color = fontcolor,
                                     size = fontsize))
  return(p)
}

clean_col_df <- function (x, col, col_rename = "age", facet_var = NULL) {
  x <- as.data.frame(x)
  for (i in 1:length(col)) {
    idx <- which(colnames(x) == col[i])
    colnames(x)[idx] <- col_rename[i]
  }
  if (!is.factor(x[,facet_var])) {
    x[,facet_var] <- factor(x[,facet_var], levels = unique(x[,facet_var]))
  }
  return(x)
}

#' Draw bar plot by N of age groups.
#'
#' @export
#' @examples
#' age <- data.frame(age=rgamma(10000, shape = 50), groups = "Age-50")
#' age$age_cut <- ag_cut(age$age, low_breaks = 29)
#' vis_bar(age, age_cut_col = "age_cut")
#' age2 <- data.frame(age=rgamma(10000, shape = 70), groups = "Age-70")
#' age2$age_cut <- ag_cut(age2$age, low_breaks = 29)
#' age <- rbind(age, age2)
#' vis_bar(age, age_cut_col = "age_cut", facet_var = "groups")
vis_bar <- function (x, age_cut_col = "age_cut", age_col = "age",
                     facet_var = NULL, fill = NULL,
                     convert_to_percent = FALSE,
                     show_zero = FALSE,
                     palt = NULL,
                     bar_val = NULL,
                     add_param = list(title = "All samples", title_median = TRUE,
                     title_iqr = TRUE, digits = 0,
                     add_line = "median", linecol = "gray", linetype = "dashed"), ...) {
  opt <- options()
  options(digits = ifelse(add_param$digits == 0, 1, add_param$digits))
  x <- clean_col_df(x, col = c(age_cut_col, age_col),
                    col_rename = c("age_cut", "age"),
                    facet_var = facet_var)

  if (is.null(palt)) palt <- ag_col()

  if (is.null(fill) || !fill %in% colnames(x)) {
    x$Groups <- "all"
    fill <- "Groups"
  }
  if (is.null (facet_var)) {
    x2 <- as.data.frame(table(x$age_cut, x[,fill]))
    x2$Percent <- 0
    if (convert_to_percent) {
      x2 <- x2 %>% group_by(Var1) %>%
        mutate(Percent = Freq/sum(Freq)*ifelse("all" %in% bar_val, 200, 100))
      print(x2)
      if (!show_zero) x2 <- x2[x2$Var2 != "0",]
      if (!is.null(bar_val)) x2 <- x2[x2$Var2 %in% bar_val,]
      p <- ggplot(data = x2) +
        geom_bar(aes(x = Var1, y = Percent, fill = Var2), stat="identity", position = "dodge",
                 color="white") + theme_minimal()
    } else {
      if (!show_zero) x2 <- x2[x2$Var2 != "0",]
      if (!is.null(bar_val)) x2 <- x2[x2$Var2 %in% bar_val,]
      p <- ggplot(data = x2) +
        geom_bar(aes(x = Var1, y = Freq, fill = Var2), stat="identity", position = "dodge",
                 color="white") + theme_minimal()
    }
  } else {
    x2 <- as.data.frame(table(x[,facet_var], x$age_cut, x[,fill]))
    x2$Percent <- 0
    if (convert_to_percent) {
      x2 <- x2 %>% group_by(Var1, Var2) %>%
        mutate(Percent = Freq/sum(Freq)*ifelse("all" %in% bar_val, 200, 100))
      if (!show_zero) x2 <- x2[x2$Var2 != "0",]
      if (!is.null(bar_val)) x2 <- x2[x2$Var3 %in% bar_val,]
      p <- ggplot(data = x2) +
        geom_bar(aes(x = Var2, y = Percent, fill = Var3), stat="identity", position = "dodge",
                 color="white") + theme_minimal() +
        scale_fill_manual(values = palt)
    } else {
      if (!show_zero) x2 <- x2[x2$Var2 != "0",]
      if (!is.null(bar_val)) x2 <- x2[x2$Var3 %in% bar_val,]
      p <- ggplot(data = x2) +
        geom_bar(aes(x = Var2, y = Freq, fill = Var3), stat="identity", position = "dodge",
                 color="white") + theme_minimal()
    }
  }
  p <- p + theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold")) +
    scale_fill_manual(values = palt) + guides(fill = guide_legend(reverse=TRUE, title = fill))
  add_param$x <- x
  add_param$p <- p
  add_param$facet_var <- facet_var
  add_param$facet_mode <- "bar"
  add_param$age_col <- age_col
  p <- do.call(add_title_vline, add_param)
  p <- set_font(p, ...) + xlab ("Age groups") +
    ylab(ifelse(convert_to_percent, "Percentage (%)", "Counts"))
  options(opt)
  return(p)
}

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
                           ylab = "Percent", digits = 3, palt = ag_col(theme = "pal_npg"),
                           negative_flag = c("0", ".", "Negative", "negative", "control"), ...) {

  if (is.null (facet_var)) {
    x2 <- x
    x2[,age_cut] <- as.numeric(x2[,age_cut])
    x2[,paste0(factor_var, "_binary")] <- 0
    x2[x2[,factor_var] %in% negative_flag, paste0(factor_var, "_binary")] <- 1

    logistic <- eval(parse(text = sprintf("glm(%s ~ %s,
                      data = x2, family = binomial())", paste0(factor_var, "_binary"), age_cut)))
    res <- summary(logistic)
    z_val <- round(res$coefficients[2,3], digits = digits)
    p_val <- round(res$coefficients[2,4], digits = digits)
    p <- suppressMessages(
      do.call(ggbarstats, list(data= x,
                               x = factor_var,
                               y = age_cut,
                               title = sprintf("%s%s ~ %s: Z-value:%s; P-value: %s",
                                               title_prefix, factor_var, age_cut, z_val, p_val),
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
                            ylab = ylab, digits = digits, ...) +
        scale_fill_manual(values = palt)
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
                            ylab = ylab, digits = digits, ...) +
        scale_fill_manual(values = palt)
      if (is.null(p)) {
        p <- ptmp
      } else {
        p <- p + ptmp
      }
    }
  }
  return (p)
}

