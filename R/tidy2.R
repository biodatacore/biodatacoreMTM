#' broom::tidy workarounds and additions.
#'
#' Tidy the result of a test into a summary data.frame. The output of tidy is
#' always a data.frame with disposable row names. It is therefore suited for
#' further manipulation by packages like dplyr, reshape2, ggplot2 and ggvis.
#'
#' @section Getting Help: Always look at the \code{broom::tidy} equivalent for
#'   more help on a function. Almost always a \code{tidy2} method will call the
#'   \code{broom::tidy} method, and just add things in.
#'
#' @return All tidying methods return a \code{data.frame} without rownames. The
#'   structure depends on the method chosen.
#' @inheritParams broom::tidy
#'
#' @export
tidy2 <- function(x, ...) {
  UseMethod("tidy2")
}



#' tidy on a NULL input
#'
#' tidy on a NULL input returns an empty data frame, which means it can be
#' combined with other data frames (treated as "empty")
#'
#' @inheritParams broom::tidy.NULL
#' @param x A value NULL
#' @param ... extra arguments (not used)
#'
#' @return An empty data.frame
#'
#' @export
tidy2.NULL <- function(x, ...) {
  broom::tidy(x)
}


#' Default tidying method
#'
#' By default, tidy uses \code{as.data.frame} to convert its output. This is
#' dangerous, as it may fail with an uninformative error message.
#' Generally tidy is intended to be used on structured model objects
#' such as lm or htest for which a specific S3 object exists.
#'
#' If you know that you want to use \code{as.data.frame} on your untidy
#' object, just use it directly.
#'
#' @inheritParams broom::tidy.default
#'
#' @return A data frame, from \code{as.data.frame} applied to the input x.
#'
#' @export
tidy2.default <- function(x, ...) {
  broom::tidy(x)
}
