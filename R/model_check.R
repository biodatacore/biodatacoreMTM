base_checks <- function(data, fos) {
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_bare_list(fos))
  purrr::walk(fos, ~stopifnot(rlang::is_formula(.)))

  # rc check
  lhss <-
    purrr::map(fos, rlang::f_lhs)

  rhss <-
    purrr::map(fos, rlang::f_rhs)

  overlaps <-
    purrr::map2_lgl(lhss, rhss, ~any(all.vars(.x) %in% all.vars(.y)))

  if (any(overlaps)) {
    msg <-
      paste(
        'Duplicate variables detected in response and control for formulas:',
        '{fos[overlaps]}'
      )

    rlang::abort(
      glue::glue(msg)
    )
  }

  # Model vars in dataset
  model_vars <-
    fos %>%
    purrr::map_chr(all.vars)  %>%
    unique()


  if (!all(model_vars %in% names(data))) {
    msg <-
      paste(
        'Not all variables specified in formulas are present in data.',
        '{setdiff(model_vars, names(data))} missing from data.'
      )

    rlang::abort(
      glue::glue(msg)
    )
  }

  # Binary valued columns are properly formatted
  is_bvc <- apply(data, 2, biodatacoreUtils::is_binary_valued, na_rm = TRUE)
  is_factor_col <- apply(data, 2, function(x) is.factor(x) | is.character(x))
  is_zero_one_col <- apply(data, 2, function(x) all(x %in% c(0, 1, NA)))

  improper_bvc <- is_bvc & !is_factor_col & !is_zero_one_col
  if (any(improper_bvc)) {
    msg <-
      paste(
        'There are some  variables detected with only 2 non NA values that are', '
        improperly formatted.',
        'Binary valued columns should either be factor, or 0-1 to avoid errors',
        'with certain modeling functions.',
        '{names(data)[improper_bvc]} are the improperly formatted binary valued columns.'
      )

    rlang::abort(
      glue::glue(msg)
    )
  }

}

model_checks <- function(data, fos, checks = NULL) {
  base_checks(data, fos)

  is_fo <- rlang::is_function
  is_vars <- ~rlang::is_character(.) || is.numeric(.)


  stopifnot(rlang::is_bare_list(checks))
  stopifnot(purrr::vec_depth(checks) == 3)
  # walk-walk
  purrr::walk(checks, function(check_pair) {
    stopifnot(rlang::is_bare_list(check_pair))
    stopifnot(length(check_pair) == 2)
    stopifnot(purrr::some(check_pair, is_fo))
    stopifnot(purrr::some(check_pair, is_vars))
  })

  check_funs <-
    purrr::map(checks, ~purrr::keep(., is_fo)) %>%
    purrr::flatten()

  check_vars <-
    purrr::map(checks, ~purrr::keep(., is_vars)) %>%
    purrr::flatten()

  purrr::reduce2(check_vars, check_funs, .init = data, .f = function(dat, cv, cf) {
    dat[, cv] <- cf(dat[, cv])
    dat
  })


}



#' Apply a list of checks to a dataset
#'
#' @param data data_frame
#' @param checks nested list of checks
#'
#' @return list
#' @export
#'
data_check <- function(data, checks, individually = TRUE) {
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_scalar_logical(individually))

  is_fo <- ~rlang::is_function(.) || rlang::is_formula(.)
  is_vars <- ~rlang::is_character(.) || is.numeric(.)


  stopifnot(rlang::is_bare_list(checks))

  # walk-walk
  purrr::walk(checks, function(check_pair) {
    stopifnot(rlang::is_bare_list(check_pair))
    stopifnot(length(check_pair) == 2)
    stopifnot(purrr::some(check_pair, is_fo))
    stopifnot(purrr::some(check_pair, is_vars))
  })

  check_funs <-
    purrr::map(checks, ~purrr::keep(., is_fo)) %>%
    purrr::flatten()

  check_vars <-
    purrr::map(checks, ~purrr::keep(., is_vars)) %>%
    purrr::flatten()


  purrr::map2(check_vars, check_funs, function(cv, cf) {
    dat <-
      data %>%
      dplyr::select(!!cv)

    if (individually) {
      results <- purrr::map(dat, cf)
    } else {
      results <- list(cf(dat))
    }

    purrr::walk(results, function(result) {
      if (!rlang::is_scalar_logical(result)) {
        rlang::abort('Check output is not scalar logical', type = 'type_error')
      }
    })

    results

  }) %>%
    purrr::set_names(names(checks))


}
