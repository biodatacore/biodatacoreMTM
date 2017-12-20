#' Tidying methods for a linear model
#'
#' @name lm_tidiers
#'
#'
#' @inheritParams contextualize
#'
NULL


# Contextualize -----------------------------------------------------------


#' Contextualizes an lm model
#'
#' @describeIn contextualize
#'
#' @param ... Arguments passed to other methods. Currently unused.
#'
#' @return data frame
#' @export
#'
contextualize.lm <- function(x, ...) {

  data_size <- nrow(x$data)
  model_sample_size <- length(x$residuals)
  fo <- stats::formula(x)


  dplyr::data_frame(
    data_size = data_size,
    model_sample_size = model_sample_size,
    formula = biodatacoreUtils::deparse2(fo),
    response = biodatacoreUtils::deparse2(rlang::f_lhs(fo)),
    control = biodatacoreUtils::deparse2(rlang::f_rhs(fo))
  )
}

#' Contextualizes a glm model
#'
#' @describeIn contextualize
#'
#' @param ... Arguments passed to other methods. Currently unused.
#'
#' @return data frame
#' @export
#'
contextualize.glm <- function(x, ...) {

  ret <- dplyr::data_frame(
    family = x$family$family,
    link = x$family$link
  )

  contextualize.lm(x) %>%
  dplyr::bind_cols(ret)
}


