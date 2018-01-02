
#' Apply a list of checks to a dataset at specified variables
#'
#' check_at and check_if apply lists predicates on lists of subsets of a data
#' frame. They are suitable for programming and for data analysis pipelines.
#'
#' @name check
#'
#' @details details about form of checks
#'
#' @note \code{\link[biodatacoreUtils]{zip}} is a useful function for creating
#'   lists of checks and subsets.
#'
#' @param data data frame
#' @param .fs list.  Each elemnt of .fs can be a single predicate function or a
#'   formula describing such a predicate function. Must return a scalar logical.
#' @param individually scalar logical. If TRUE, each column in the subset is
#'   passed to .f. if FALSE, the entire subset dataframe is passed.
#'
#' @return list
#' @export
#'


# It is possible to use dplyr functions like summarise instead of map. However,
# I like splitting the part where the data is selected from the part where the
# calculation is performed, for the purpose of understanding how `individually`
# works.

#' @rdname check
#'
#' @param .ats list. Each element of .ats can be a character vector of names or
#'   a numeric vector of positions. Only those elements corresponding to .at
#'   will be modified.
#'
#' @export
#'
check_at <- function(data, .ats, .fs, individually = TRUE) {

  # Assertions
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_scalar_logical(individually))


  # Map2 takes on the names of the first element
    purrr::map2(.ats, .fs, function(.at, .f) {
      dat <-
        data %>%
        dplyr::select_at(.at)

      if (individually) {
        results <- purrr::map(dat, .f)
      } else {
        results <- purrr::map(list(dat), .f)
      }

      purrr::walk(results, function(result) {
        if (!rlang::is_scalar_logical(result)) {
          rlang::abort('Check output is not scalar logical', type = 'type_error')
        }
      })

      results

    })


}

#' @rdname check
#'
#' @param .ps list. Each elemnt of .ps can be a single predicate function, a formula
#'   describing such a predicate function, or a logical vector of the same
#'   length as the number of cols of data. Only those elements where .p evaluates to TRUE will be checked
#'
#' @export
#'
check_if <- function(data, .ps, .fs, individually = TRUE) {

  # Assertions
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_scalar_logical(individually))


  # Map2 takes on the names of the first element
    purrr::map2(.ps, .fs, function(.p, .f) {
      dat <-
        data %>%
        dplyr::select_if(.p)

      if (individually) {
        results <- purrr::map(dat, .f)
      } else {
        results <- purrr::map(list(dat), .f)
      }

      purrr::walk(results, function(result) {
        if (!rlang::is_scalar_logical(result)) {
          rlang::abort('Check output is not scalar logical', type = 'type_error')
        }
      })

      results

    })




}

# Check that was deleted because summarise_at is the same thing.

#
#
# base_checks <- function(data, fos) {
#   stopifnot(is.data.frame(data))
#   stopifnot(rlang::is_bare_list(fos))
#   purrr::walk(fos, ~stopifnot(rlang::is_formula(.)))
#
#   # rc check
#   lhss <-
#     purrr::map(fos, rlang::f_lhs)
#
#   rhss <-
#     purrr::map(fos, rlang::f_rhs)
#
#   overlaps <-
#     purrr::map2_lgl(lhss, rhss, ~any(all.vars(.x) %in% all.vars(.y)))
#
#   if (any(overlaps)) {
#     msg <-
#       paste(
#         'Duplicate variables detected in response and control for formulas:',
#         '{fos[overlaps]}'
#       )
#
#     rlang::abort(
#       glue::glue(msg)
#     )
#   }
#
#   # Model vars in dataset
#   model_vars <-
#     fos %>%
#     purrr::map_chr(all.vars)  %>%
#     unique()
#
#
#   if (!all(model_vars %in% names(data))) {
#     msg <-
#       paste(
#         'Not all variables specified in formulas are present in data.',
#         '{setdiff(model_vars, names(data))} missing from data.'
#       )
#
#     rlang::abort(
#       glue::glue(msg)
#     )
#   }
#
#   # Binary valued columns are properly formatted
#   is_bvc <- apply(data, 2, biodatacoreUtils::is_binary_valued, na_rm = TRUE)
#   is_factor_col <- apply(data, 2, function(x) is.factor(x) | is.character(x))
#   is_zero_one_col <- apply(data, 2, function(x) all(x %in% c(0, 1, NA)))
#
#   improper_bvc <- is_bvc & !is_factor_col & !is_zero_one_col
#   if (any(improper_bvc)) {
#     msg <-
#       paste(
#         'There are some  variables detected with only 2 non NA values that are', '
#         improperly formatted.',
#         'Binary valued columns should either be factor, or 0-1 to avoid errors',
#         'with certain modeling functions.',
#         '{names(data)[improper_bvc]} are the improperly formatted binary valued columns.'
#       )
#
#     rlang::abort(
#       glue::glue(msg)
#     )
#   }
#
# }

