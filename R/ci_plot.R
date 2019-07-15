#' Custom Cumulative incidence plot with risk table
#'
#' Hodgepodge of surviminer, survival, and custom code to get a
#' cumulative incidence curve with risk table the way I like it
#'
#' @param dat data frame
#' @param time variable, in quotes, with the survival time information
#' @param status variable, in quotes, with crr survival status
#' @param variable variable, in quotes, of interest to compare
#' @param title title of graph
#' @param note if you need to annotate the figure
#' @param palette color palette
#' @param legend_labs vector
#' @param xlim vector
#' @param ylim vector
#' @param break.time.by vector
#' @author Margaret L. Hannum
#' @examples
#' # example add

ci_plot <- function(dat, time, status, variable,
                    title, note, pallette,
                    legend_labs, xlim, ylim, break.time.by) {
  cuminc_mod <- cmprsk::cuminc(ftime = dat[[time]],
                       fstatus = dat[[status]],
                       group = dat[[variable]])
  my_pallete <- pallette
  names(my_pallete) <- levels(dat[[variable]])
  crr_plot <- ggcomprisk_mh(cuminc_mod,
                                      multiple_panels = F) +
    labs(title = title,
         x = "Time from Date of Start RT (Month)",
         y = "Cumulative Incidence") +
    scale_linetype_manual(labels = c("LRF", "Death w/ no LRF"), values = c("solid", "blank"), guide = "none") +
    theme_survminer()  +
    scale_colour_manual(
      breaks = levels(dat[[variable]]),
      values = my_pallete) +
    theme(legend.position = "top") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    annotate("text", label = note, x = 72, y = 0.02) +
    scale_x_continuous(breaks=c(24, 48, 72, 96))

  fit <- survival::survfit(as.formula(paste("Surv(", time, ",",  status, ") ~ ", variable, sep = " ")),
                 data = dat)
  # get risk table from survminer
  crr_risk <- survminer::ggrisktable(fit,
                                     data = dat,
                                     risk.table.type = "risk.table",
                                     risk.table.title = "Number at Risk",
                                     legend.labs = legend_labs,
                                     xlim = xlim,
                                     break.time.by = break.time.by,
                                     xlab = "",
                                     ylab = "")

  b <- gridExtra::arrangeGrob(crr_plot, crr_risk,
                              nrow = 2,
                              heights = c(4, 1))
  b

}
