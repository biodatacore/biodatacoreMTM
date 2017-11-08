#' Substitute a vector of replacements for a single term into a list of formulas
#'
#' @param fos formula or list of formulas
#' @param term scalar character: the term in the formula to be substituted out
#' @param sub vector: the terms to be substituted in. A formula is generated for
#'   each element of sub
#' @param named scalar logical: Whether or not character substitutions
#'   should be passed as names.
#' @return list
#' @seealso f_subs
#' @export
#'
fs_sub <- function(fos, term, sub, named = TRUE) {

  # single formula case
  if (rlang::is_formula(fos)) fos <- list(fos)

  stopifnot(rlang::is_bare_list(fos))
  stopifnot(rlang::is_scalar_character(term))
  stopifnot(rlang::is_vector(sub))
  stopifnot(rlang::is_scalar_logical(named))

  new_fos <-
    purrr::map(fos, function(fo) {
      purrr::map(sub, function(sub_env) {
        sub_env %<>%
          as.list() %>%
          purrr::set_names(term)

        if (named) sub_env %<>% purrr::map(as.name)

        biodatacoreUtils::substitute_q(fo, env = sub_env)
      })
    }) %>%
    purrr::flatten()

  return(new_fos)

}


#' Substitute a list of value - replacement pairs into a single formula
#'
#' @param fo formula
#' @param subs named list: value - replacement pairs. The name of the list
#'   element is the term that will be replaced. The vector of values under that
#'   list are the terms that will be substituted in.
#' @param cross logical: should the substitutions be combined, applying each
#'   value - replacement pair serially to the same formula? Or should each
#'   substitution be applied by itself to the formula?
#' @param named scalar logical: Whether or not substitutions
#'   should be passed as names.
#' @return list of formulas
#'
#' @seealso fs_sub
#' @export
#
f_subs <- function(fo, subs, cross = TRUE, named = TRUE) {
  stopifnot(rlang::is_formula(fo))
  stopifnot(rlang::is_list(subs) && rlang::is_named(subs))
  stopifnot(purrr::every(purrr::map(subs, rlang::is_vector), base::identity))
  stopifnot(rlang::is_scalar_logical(cross))

  if (!cross) {
    purrr::imap(subs, ~fs_sub(fo, .y, .x, named)) %>%
      purrr::flatten()
  } else {
    fs_sub_nc <- purrr::partial(fs_sub,
                                named = named,
                                .lazy = FALSE,
                                .first = FALSE)
    purrr::reduce2(names(subs), subs, fs_sub_nc, .init = fo)
  }
}



#' Substitute a list of value - replacement pairs into a list of formulas
#'
#' @param fos list of formulas
#' @param subs named list: value - replacement pairs. The name of the list
#'   element is the term that will be replaced. The vector of values under that
#'   list are the terms that will be substituted in.
#' @param cross logical: should the substitutions be combined, applying each
#'   value - replacement pair serially to the same formula? Or should each
#'   substitution be applied by itself to the formula?
#' @param named scalar logical: Whether or not character substitutions
#'   should be passed as names.
#' @return list of formulas
#'
#' @seealso fs_sub f_subs
#' @export
#
f_sub <- function(fos, subs, cross = TRUE, named = TRUE) {
  stopifnot(rlang::is_named(subs))
  purrr::map(fos, ~f_subs(., subs, cross, named))
}



#' Helper function to convert a list of left and right hand side modeling vars
#' to simple formulas
#'
#' Essentially a wrapper around \code{\link{reformulate}} with checks.
#'
#' @param lst named nested list: list of length two lists.
#' @param lhs,rhs scalar character: the names of the lhs and rhs elements
#'
#' @return list of formulas
#' @export
#'
lst_to_formulas <- function(lst, lhs = 1, rhs = 2) {
  stopifnot(purrr::vec_depth(lst) == 3)

  purrr::walk(lst, ~stopifnot(length(.) == 2))

  if (rlang::is_string(lhs) | rlang::is_string(rhs)) {
    stopifnot(rlang::is_string(lhs) && rlang::is_string(rhs))

    lst  %>%
      purrr::walk(~stopifnot(rlang::is_named(.))) %>%
      purrr::walk(~stopifnot(all(c(lhs, rhs) %in% names(.))))
  } else {
    stopifnot(rlang::is_integerish(lhs) && rlang::is_integerish(rhs))
  }

  lst %>%
    purrr::walk(function(x) purrr::walk(x, ~stopifnot(rlang::is_character(.))))


  lhss <- purrr::map(lst, lhs)
  rhss <- purrr::map(lst, rhs)


  purrr::walk2(lhss, rhss, function(lhs, rhs) {

    if (anyNA(lhs)) {
      msg <- paste('NAs detected in',
                   '{lhs}')
      rlang::abort(
        glue::glue(msg)
      )
    }

    if (anyNA(rhs)) {
      msg <- paste('NAs detected in',
                   '{rhs}')
      rlang::abort(
        glue::glue(msg)
      )
    }

    if (length(lhs) != 1) {
      msg <- paste('LHS must have length 1: {lhs}')
      rlang::abort(
        glue::glue(msg)
      )
    }

    if (!(length(rhs) >= 0)) {
      msg <- paste('RHS must have length >= 0')
      rlang::abort(
        glue::glue(msg)
      )
    }

    if (anyDuplicated(lhs)) {
      msg <- paste('Duplicate variables detected within RHS: {lhs}')
      rlang::abort(
        glue::glue(msg)
      )
    }

    if (anyDuplicated(rhs)) {
      msg <- paste('Duplicate variables detected within RHS: {rhs}')
      rlang::abort(
        glue::glue(msg)
      )
    }

    if (anyDuplicated(c(lhs,rhs))) {
      msg <- paste('Duplicate variables detected between LHS, RHS: {c(lhs,rhs)}')
      rlang::abort(
        glue::glue(msg)
      )
    }

  })


  purrr::map2(lhss, rhss, function(lhs, rhs) {
    stats::reformulate(rhs, response = lhs, intercept = TRUE)
  })
}
