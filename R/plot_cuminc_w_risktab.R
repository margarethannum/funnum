#' Plot Cumulative Incidence with Survminer Risk Table
#'
#' Pastes together a modified version of ggcompetingrisks with a
#' custom ggrisktable
#'
#' @param title character title
#' @param note if you want to annotate
#' @param variable group variable
#' @param survtime survival time variable
#' @param survstatus_gray survival status variable for competing risks (0, 1, 2)
#' @param survstatus_reg survival status variable for standard kaplan meier (0,1): required for risk table
#' @param dat dataframe
#' @param legend_labs Vector of group factors. Default is levels(as.factor(dat[[variable]]))
#' @param xlim vector of x limits
#' @param ylim vector of y limits
#' @param break.time.by numeric
#' @param breaks numeric vector
#'
#' @export
#' @seealso \code{\link[survminer]{ggcompetingrisks}}, \code{\link[survminer]{ggrisktable}}, \code{\link[cmprsk]{cuminc}}
#' @author Margaret L. Hannum
#' @examples
#' ADD EXAMPLE

plot_cuminc_w_risktab <- function(title, note,
                                    # pallette,
                                    variable,
                                    survtime,
                                    survstatus_gray,
                                    survstatus_reg,
                                    dat,
                                  legend_labs = levels(as.factor(dat[[variable]])),
                                    xlim, ylim,
                                    break.time.by,
                                    breaks) {
  cuminc_mod <- cuminc(ftime = dat[[survtime]],
                       fstatus = dat[[survstatus_gray]],
                       group = dat[[variable]])
  # my_pallete <- pallette
  # names(my_pallete) <- levels(dat[[variable]])
  crr_plot <- ggcompetingrisks_margie(cuminc_mod,
                                      multiple_panels = F) +
    labs(title = title,
         x = "Time from Date of Start Tx (Month)",
         y = "Cumulative Incidence") +
    scale_linetype_manual(labels = c("LRF", "Death w/ no LRF"), values = c("solid", "blank"), guide = "none") +
    theme_survminer()  +
    # scale_colour_manual(
    #   breaks = levels(dat[[variable]]),
    #   values = my_pallete) +
    theme(legend.position = "top") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    #annotate("text", label = note, x = 72, y = 0.02) +
    scale_x_continuous(breaks= breaks
    )

  fit <- survfit(as.formula(paste("Surv(", survtime, ",",  survstatus_reg, ") ~ ", variable, sep = " ")),
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
