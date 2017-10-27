#' Pipe
#'
#' Use the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @keywords internal
#' @param lhs,rhs Pipe the output of \code{lhs} to the first input of \code{rhs}
#' @examples
#' 1 %>% `+`(1)
NULL

#' Double Pipe
#'
#' Use the double pipe function, \code{\%<>\%} to turn function composition into
#' a series of imperative statements.
#'
#' @importFrom magrittr %<>%
#' @name %<>%
#' @rdname pipe
#' @export
#'
#' @param lhs,rhs Pipe the output of \code{lhs} to the first input of \code{rhs}
#'   and assign to the left hand side
#' @examples
#' a = 1
#' a %<>% `+`(1)
#' a
NULL

#' .data
#'
#' Use the .data when writing functions that use piping with dplyr to avoid lazy
#' eval issues and get informative error messages
#'
#' @importFrom rlang .data
#' @name .data
#' @rdname pipe
NULL
