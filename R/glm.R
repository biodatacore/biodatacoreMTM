#' glm generic
#'
#' Wraps glm to provide more flexible interface
#'
#' @return object of class 'glm'
#' @export
glm2 <- function(formula, ...) {
  UseMethod('glm2')
}

#' @inheritParams stats::glm
#' @describeIn glm2 Default \code{\link[stats]{glm}}
#' @export
glm2.default <- function(formula, family = stats::gaussian, data, weights, subset,
                        na.action = na.omit, start = NULL, etastart = NULL, mustart = NULL, offset = NULL,
                        control = list(...), model = TRUE, method = "glm.fit",
                        x = FALSE, y = TRUE, contrasts = NULL, ...) {


  # So, normally there are two types of arguments as far as I can tell. Ones
  # with defaults that the user can optionally specify, and ones without
  # defaults, that a user *must* specify. stats::glm uses some black magic to
  # have arguments without defaults that the user does not need to specify. This
  # makes it difficult to call stats::glm from within glm2 the normal way. EG
  # stats::glm(data = data, weights = weights, subset = subset). Instead we
  # rewrite the call and evaluate it as if we had just called stats::glm from
  # the start.
  call <- match.call()
  call[[1L]] <- quote(stats::glm)
  eval(call, parent.frame())



}
