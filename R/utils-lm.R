#' Attempts to guess the proper modeling family of a variable
#'
#' @param x atomic vector
#'
#' @return family object
#' @export
#'
guess_glm_fam <- function(x) {
  if (biodatacoreUtils::is_binary_valued(x, na_rm = TRUE) || is.factor(x)) {
    stats::binomial(link = 'logit')
  } else {
    stats::gaussian(link = 'identity')
  }
}
