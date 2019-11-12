#' Calculate row AND column crosstabulatd percent
#'
#' Useful in comparing cluster assignments
#'
#' @param x crosstabulated matrix (e.g. cluster assignments)
#' @param rowvar name of row variables
#' @param colvar name of column variables
#'
#' @export
#' @author Margaret L. Hannum
#' @examples
#
# x <- cbind( x1 = c(1, 0, 0, 4), x2 = c(0, 1, 5, 0), x3 = c(6, 3, 0, 0))
# xtab_rowcol(x, "a", "b")

xtab_rowcol <- function(x, rowvar, colvar) {
  x1 <- matrix(rep(rowSums(x, na.rm = TRUE), ncol(x)), ncol = ncol(x))
  x2 <- matrix(rep(colSums(x, na.rm = TRUE), nrow(x)), nrow = nrow(x), byrow = TRUE)
  x3 <- x1 + x2
  x4 <- x/x3
  colname <- dplyr::enquo(colvar)
  rowname <- dplyr::enquo(rowvar)

  df <- tibble::rownames_to_column(tibble::as_tibble(x4), rowvar)

  long <-
    df %>%
    tidyr::gather((!!colname), percent, -rowvar)

  long2 <- long %>%
    dplyr::arrange(dplyr::desc(percent)) %>%
    dplyr::mutate(new_var = dplyr::row_number())

  long3 <- long2 %>%
    dplyr::distinct_(rowvar, colvar, .keep_all = TRUE)
#
#   x_new <- x[as.numeric(long3[[rowvar]]), long3[[colvar]]]
#   return(x_new)
}

# x <- cbind( x1 = c(1, 0, 0, 4), x2 = c(0, 1, 5, 0), x3 = c(6, 3, 0, 0))
t <- xtab_rowcol(x, "nebula", "icluster")


xtab_rowcol <- function(x, rowvar, colvar) {
  x1 <- matrix(rep(rowSums(x, na.rm = TRUE), ncol(x)), ncol = ncol(x))
  x2 <- matrix(rep(colSums(x, na.rm = TRUE), nrow(x)), nrow = nrow(x), byrow = TRUE)
  x3 <- x1 + x2
  x4 <- x/x3

  df <- tibble::rownames_to_column(tibble::as_tibble(x4), !!rlang::sym(rowvar))
  #
  # long <-
  #   df %>%
  #   tidyr::gather(!!colvar, percent, -!!rowvar)
  #
  # long2 <- long %>%
  #   dplyr::arrange(dplyr::desc(percent)) %>%
  #   dplyr::mutate(new_var = dplyr::row_number())
  #
  # long3 <- long2 %>%
  #   dplyr::distinct(!!rowvar, .keep_all = TRUE)
  #
  #   x_new <- x[as.numeric(long3[[rowvar]]), long3[[colvar]]]
  #   return(x_new)
}
