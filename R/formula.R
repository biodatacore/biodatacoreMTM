#' Substitute a vector of replacements for a single term into a list of formulas
#'
#' @param fos formula or list of formulas
#' @param term scalar character: the term in the formula to be substituted out
#' @param sub vector: the terms to be substituted in. A formula is generated for
#'   each element of sub
#'
#' @return list
#' @export
#'
fs_sub <- function(fos, term, sub) {

    # single formula case
    if (rlang::is_formula(fos)) fos <- list(fos)

    stopifnot(rlang::is_list(fos))
    stopifnot(rlang::is_scalar_character(term))
    stopifnot(rlang::is_vector(sub))

    new_fos <-
        purrr::map(fos, function(fo) {
            purrr::map(sub, function(sub_env) {
                sub_env %<>%
                    as.list() %>%
                    purrr::set_names(term)

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
#'
#' @return list of formulas
#' @export
#
f_subs <- function(fo, subs, cross = TRUE) {
    stopifnot(rlang::is_formula(fo))
    stopifnot(rlang::is_list(subs) && rlang::is_named(subs))
    stopifnot(purrr::every(purrr::map(subs, rlang::is_vector), base::identity))

    if (!cross) {
        purrr::imap(subs, ~fs_sub(fo, .y, .x)) %>%
        purrr::flatten()
    } else {
        purrr::reduce2(names(subs), subs, fs_sub, .init = fo)
    }
}
