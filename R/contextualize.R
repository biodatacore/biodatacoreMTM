#' Construct a single row summary of a model, fit, or other object
#'
#' contextualize methods always return either a one-row data frame (except on
#' NULL, which returns an empty data frame). It is different from
#' \code{broom::glance} in that it gives information about how the model was
#' run, instead of EG fit statistics.
#'
#' @inheritParams broom::glance
#'
#' @return data frame
#' @export
#'
contextualize <- function(x, ...) {
  UseMethod('contextualize')
}

#' @describeIn contextualize Returns `NULL`.
#' @export
contextualize.NULL <- function(x, ...) data.frame()

#' @describeIn contextualize Errors and prints class of object.
#' @export
contextualize.default <- function(x, ...) {
  msg <- "contextualize doesn't know how to deal with data of class {class(x)}"
  rlang::abort(glue::glue(msg))
}
