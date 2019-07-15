#Competing risks modified function (from survminer)
# Making color be group instead of event
ggcomprisk_mh <-
  function (fit, gnames = NULL, gsep = " ", multiple_panels = TRUE)
  {
    if (!is.null(fit$Tests))
      fit <- fit[names(fit) != "Tests"]
    fit2 <- lapply(fit, `[`, 1:2)
    if (is.null(gnames))
      gnames <- names(fit2)
    fit2_list <- lapply(seq_along(gnames), function(ind) {
      df <- as.data.frame(fit2[[ind]])
      df$name <- gnames[ind]
      df
    })
    time <- est <- event <- group <- NULL
    df <- do.call(rbind, fit2_list)
    df$event <- sapply(strsplit(df$name, split = gsep), `[`,
                       2)
    df$group <- sapply(strsplit(df$name, split = gsep), `[`,
                       1)
    pl <- ggplot(df, aes(time, est, color = group))
    if (multiple_panels) {
      pl <- ggplot(df, aes(time, est, color = group)) + facet_wrap(~event)
    }
    else {
      pl <- ggplot(df, aes(time, est, color = group, linetype = event))
    }
    pl + geom_line(size = 1.5)
  }
