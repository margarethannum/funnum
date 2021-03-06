#' Plot nice lasso results from glmnet
#'
#' Input a glmnet object and output a formatted ggplot result of the coefficients.
#'
#' @param fit object of class `glmnet`
#' @param beta class dgCMatrix from extracting coefficients from model fit
#' @param intercept string indicating name of intercept in model results
#'
#' @export
#' @seealso \code{\link[survminer]{ggcompetingrisks}}
#' @author Margaret L. Hannum
#' @examples
#'
#'  \dontrun{
#'  ## Gaussian
#'  x = matrix(rnorm(100 * 20), 100, 20)
#'  y = rnorm(100)
#'  fit1 = glmnet(x, y)
#'  ggglmnet(fit1, coef(fit1), intercept = "(Intercept)")
#'
#'  ## multinomial
#'  g4 = sample(1:4, 100, replace = TRUE)
#'  fit3 = glmnet(x, g4, family = "multinomial")
#'  # plot coefficients for first level of multinomial endpoint
#'  ggglmnet(fit3, coef(fit3)$`1`, intercept = "")
#'  }

ggglmnet <- function(fit, beta, intercept){
  tmp <- as.data.frame(as.matrix(beta))
  tmp$coef <- row.names(tmp)
  tmp <- reshape::melt(tmp, id = "coef")
  tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
  tmp$lambda <- fit$lambda[tmp$variable+1] # extract the lambda values
  tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm

  #make data without intercept and only have labels for nonzero coefficients
  tmpdat <- tmp[tmp$coef != intercept,] %>%
    mutate(label = ifelse(lambda == min(lambda) & value != 0, as.character(coef), NA))

  # now make the nice ggplot
  tmpdat %>%
    ggplot(
      aes(lambda, value, color = coef#, linetype = coef
      )) +
    geom_line() +
    scale_x_log10() +
    xlab("Lambda") +
    guides(color = guide_legend(title = ""),
           linetype = guide_legend(title = "")) +
    geom_label_repel(
      aes(label = label),
      nudge_x = -1,
      na.rm = TRUE) +
    theme_bw() +
    theme(legend.position = "none")
}
