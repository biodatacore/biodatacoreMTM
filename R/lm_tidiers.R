#' Tidying methods for a linear model
#'
#' @name lm_tidiers
#'
#' @inheritParams broom::tidy.lm
#' @inheritParams contextualize
#'
NULL


# tidy --------------------------------------------------------------------


#' @rdname lm_tidiers

#' @export
#'
tidy2.lm <- function(x, conf.int = FALSE, conf.level = 0.95,
                     exponentiate = FALSE, quick = FALSE, ...) {

  out <- broom::tidy(x, conf.int, conf.level, exponentiate, quick)

  if (anyNA(coef(x))) {
    coefs <- coef(x)
    na_coefs <- names(coefs)[rlang::are_na(coefs)]

    out %<>%
      dplyr::add_row(term = na_coefs)

    # Arrange in original order
    out %<>%
      dplyr::slice(match(.data$term, names(coefs)))
  }

  out
}
# Contextualize -----------------------------------------------------------



#' @rdname lm_tidiers
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

#' @rdname lm_tidiers
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


