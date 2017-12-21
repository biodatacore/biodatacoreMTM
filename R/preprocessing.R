
#' Title
#'
#' @family preprocessing utilities
#'
#' @inheritParams biodatacoreUtils::fill_if
#'
#' @return atomic vector of same length as x
#' @export
#'
fill_zero <- function(x, .fill) {
  biodatacoreUtils::fill_if(x, ~. == 0, .fill)
}

#' @rdname fill_zero
#' @export
fill_na <- function(x, .fill) {
  biodatacoreUtils::fill_if(x, ~rlang::is_na(.), .fill)
}

#' A wrapper around `scale` and `as.vector` suitable for use with `dplyr::mutate`
#'
#' Removes atributes that currently break `dplyr``
#'
#' @family preprocessing utilities
#'
#' @inheritParams base::scale
#'
#' @export
#'
scale2 <- function(x, center = TRUE, scale = TRUE) {
  as.vector(scale(x, center = center, scale = scale))
}



#' Title
#'
#' @param data data frame
#'
#' @return data frame
#' @export
#'
df_remove_rows_with_na <- function(data) {
  stopifnot(is.data.frame(data))
  data[stats::complete.cases(data), ]
}
