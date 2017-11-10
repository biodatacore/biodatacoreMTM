#' Workaround and enhancers for broom::tidy.lm models
#'
#' Handles a specific case where some coefficents are NA in the model output. It
#' preserves those coefficents as NA instead of removing them. Also adds
#' additional output. Parameters \code{conf.int:quick} are original
#' \code{\link[broom]{lm_tidiers}} parameters.
#'
#' @details If you have missing values in your model data, you may need to refit
#'   the model with \code{na.action = na.exclude}. See
#'   \code{\link[broom]{tidy.lm}} for more information.
#'
#' @inheritParams broom::tidy.lm
#' @export
tidy2.lm <- function(x, conf.int = FALSE, conf.level = .95,
                     exponentiate = FALSE, quick = FALSE, ...) {

  # These shenanigans is due to the fact that `broom::tidy` creates its
  # summaries in two ways. The first by grabbing the coefficents from the model
  # summary. The second by calling `confint` on the model. Then it cbinds it
  # together. The issue is that if there is an NA coefficent, the coefficents
  # will ignore the NA terms while `confint` will include them. This breaks the
  # cbind, as it is attempting to cbind dataframes with different numbers of
  # rows. Even if confidence intervals are not requested, NA coefs are omitted
  # from the final result, a behavior I would like to avoid.


  # As such, we call `tidy.summary.lm` which gives
  # us the tidy object without any additional processing with additional
  # arguments, add in the missing NA rows, and then call `process_lm` to
  # finish the tidying



  # Errors with NA coef only happen with conf.int = TRUE. quick automatically
  # sets conf.int to FALSE

  co <- stats::coef(x)


  # quick passes NA properly
  if (quick) {
    ret <- data.frame(term = names(co), estimate = unname(co))
    return(process_lm(ret, x, conf.int = FALSE, exponentiate = exponentiate))
  } else {


    # This line exists to make it possible to call tidy2.lm on glm models. (glm
    # models inherit from lm models) The way broom works is that a model is
    # first summarized with `summary()` and then tidied using tidy.summary.lm
    # before going on to other functions. There doesn't exist a
    # broom::tidy.summary.glm method. broom just manually calls
    # broom::tidy.summary.lm on the summary.glm object. For some reason, I can't
    # find a way to invoke specifc methods of `tidy` using `::` notation..
    # Knowing that it calls broom::tidy.summary.lm directly on the glm.summary
    # object, I spoof the class to get the same results.
    smry <- summary(x)
    class(smry) <- 'summary.lm'
    partial_tidy_model <- broom::tidy(smry)

    # Coef of the regular model always seems to return an NA
    if (any(rlang::are_na(co))) {

      # left joining on ensures that the output is ordered in the same order as
      # the output of 'confint' this makes the cbind in process_lm correct.
      # Using dplyr::data_frame to avoid stupid stringsAsFactors nonsense.
      partial_tidy_model <-
        dplyr::left_join(dplyr::data_frame(term = names(co)), partial_tidy_model)

    }

  }

  tidy_model <-
    process_lm(
      partial_tidy_model,
      x,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate
    )


  # Additional add-ons: Model-wide statistics

  # It seems that residuals(x) might add in NA for observations that were
  # removed during modeling due to NA. However, x$residuals seems to only be
  # there for observations that were modeled.
  tidy_model$sample_size <- length(x$residuals)

  fo <- stats::formula(x)
  tidy_model$formula <- biodatacoreUtils::deparse2(fo)
  tidy_model$response <- biodatacoreUtils::deparse2(rlang::f_lhs(fo))
  tidy_model$control  <- biodatacoreUtils::deparse2(rlang::f_rhs(fo))

  return(tidy_model)

}

#' Workaround and enhancers for broom::tidy.lm models
#'
#' Handles a specific case where some coefficents are NA in the model output.
#' Also adds additional output. Parameters \code{conf.int:quick} are original
#' \code{\link[broom]{lm_tidiers}} parameters.
#'
#' @details If you have missing values in your model data, you may need to refit
#'   the model with \code{na.action = na.exclude}. See
#'   \code{\link[broom]{tidy.lm}} for more information.
#'
#' @inheritParams broom::tidy.lm
#' @export
tidy2.glm <- function(x, conf.int = FALSE, conf.level = .95,
                      exponentiate = FALSE, quick = FALSE, ...) {

  tidy_model <-
    tidy2.lm(
      x,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate,
      quick = quick,
      ...)

  tidy_model$family <-
    x$family$family

  tidy_model$link <-
    x$family$link

  return(tidy_model)

}


#' helper function to process a tidied lm object. Taken from
#' \url{https://github.com/tidyverse/broom/blob/master/R/lm_tidiers.R}
#'
#' Adds a confidence interval, and possibly exponentiates, a tidied object.
#' Useful for operations shared between lm and biglm.
#'
#' @param ret data frame with a tidied version of a coefficient matrix
#' @param x an "lm", "glm", "biglm", or "bigglm" object
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if
#'   \code{conf.int=TRUE}
#' @param exponentiate whether to exponentiate the coefficient estimates and
#'   confidence intervals (typical for logistic regression)
#'
process_lm <- function(ret, x, conf.int = FALSE, conf.level = .95,
                       exponentiate = FALSE) {
  if (exponentiate) {
    # save transformation function for use on confidence interval
    if (is.null(x$family) ||
        (x$family$link != "logit" && x$family$link != "log")) {
      warning(paste("Exponentiating coefficients, but model did not use",
                    "a log or logit link function"))
    }
    trans <- exp
  } else {
    trans <- identity
  }

  if (conf.int) {
    # avoid "Waiting for profiling to be done..." message
    CI <- suppressMessages(stats::confint(x, level = conf.level))

    perc_levels <- colnames(CI)
    CI_nms <- c(paste('conf.low', perc_levels[[1]]), paste('conf.high', perc_levels[[2]]))
    colnames(CI) <- CI_nms

    # This should never happen due to the left_join but in case I want to get
    # rid of left join this check is here.

    stopifnot(all(rownames(CI) == ret$term))
    ret <- cbind(ret, trans(biodatacoreUtils::unrowname(CI)))
  }
  ret$estimate <- trans(ret$estimate)

  ret
}


