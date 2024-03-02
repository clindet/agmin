#' Draw survival curve by age groups
#'
#' @export
#' @examples
#' age <- data.frame(age=rgamma(10000, shape = 50), groups = "Age-50")
#' age$age_cut <- ag_cut(age$age)
#' age$os <- c(rgamma(2000, shape = 160), rgamma(2000, shape = 320),
#'   rgamma(4000, shape = 560), rgamma(2000, shape = 800))
#' age$os_status <- round(rgamma(10000, shape = 720) %% 2)
#' age$os_status[age$os_status != 0] <- 1
#' vis_surv(age, plt = ag_col()[-c(1,2)])
vis_surv <- function(x, group = "age_cut", time = "os", status = "os_status", title = "",
                     xlab = "Time in days",
                     leg_labs = NULL, xlim = NULL, plt = NULL, brk = 180,
                     rtb = TRUE, pval = TRUE, conf_int = TRUE, conf_alpha = 0.1,
                     nc_plot = FALSE, nc_plot_h = 0.3, rtb_y_col = TRUE, rtb_y = FALSE, rtb_h = 0.25,
                     ...) {
  x$os <- as.numeric(x[,time])
  x$os_status <- as.numeric(x[,status])
  x$age_cut <- x[,group]
  if (!is.factor(x$age_cut)) {
    x$age_cut <- factor(x$age_cut)
  }
  if (is.null(leg_labs)) {
    leg_labs <- levels(x$age_cut)
  }
  fit <- survfit(Surv(os, os_status) ~ age_cut, data = x)
  p <- ggsurvplot(
    fit,
    data = x,
    risk.table = rtb,
    pval = pval,
    conf.int = conf_int,
    palette = plt,
    xlim = xlim,
    # survival estimates.
    xlab = xlab,
    brk.time.by = brk,
    risk.table.y.text.col = rtb_y_col,
    risk.table.height = rtb_h,
    risk.table.y.text = rtb_y,
    # in legend of risk table.
    ncensor.plot = nc_plot,
    ncensor.plot.height = nc_plot_h,
    conf.int.alpha = conf_alpha,
    title = title,
    legend.labs = leg_labs,
    ...
  )
  return(p)
}
