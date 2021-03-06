#' Adds columns of adjusted p-values
#'
#' Adds adjusted p-values to a dataframe. Helper functions wrap `add_p.adjust`
#' for common additions.
#'
#' @family augmenters
#'
#' @param data data frame
#' @param method scalar character: which adjustment method to use. See
#'   \code{\link[stats]{p.adjust}} for more info.
#' @param p_value scalar character: name of the p-value column.
#' @param var scalar character: name of the column to be created. if `NULL` then
#'   it attempts to create its own column. To avoid accidenntally overwriting an
#'   existing column, it is safer to always supply a name here.
#'
#' @return data frame
#' @export
#'
add_p.adjust <- function(data, method = stats::p.adjust.methods, p_value = 'p.value', var = NULL) {

  method <- match.arg(method)

  if (rlang::is_null(var)) {
    var <- paste(method, 'pvalue', sep = '_')
  }

  data[[var]] <- stats::p.adjust(data[[p_value]], method = method)
  data
}

#' @rdname add_p.adjust
#' @export
#'
add_bonf_pv <- function(data, p_value = 'p.value', var = NULL) {
  add_p.adjust(data, method = 'bonferroni', p_value = p_value, var = var)
}

#' @rdname add_p.adjust
#' @export
#'
add_fdr_pv <- function(data, p_value = 'p.value', var = NULL) {
  add_p.adjust(data, method = 'fdr', p_value = p_value, var = var)
}

# TODO fancy pretty-print math this

#' Adds -1*log_10() to a data frame
#'
#' Negative log 10 pvalues are often used when comparing a large number of
#' pvalues. It ends up with a more natural scale, where larger values are "more
#' significant."
#'
#' @family augmenters
#'
#' @inheritParams add_p.adjust
#' @return data frame
#' @export
#'
add_nlog10_pv <- function(data, p_value = 'p.value', var = NULL) {

  if (rlang::is_null(var)) {
    var <- 'neg_log10_pvalue'
  }


  data[[var]] <- biodatacoreUtils::neg_log_10(data[[p_value]])
  data
}

