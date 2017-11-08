#' #' Checks inputs before modeling
#' #'
#' #' Though it is not necessary, filtering the dataset before applying
#' #' modeling_prep *can* make it run faster
#' #'
#' #' @param data data_frame. Must contain all responses and controls.
#' #' @param model_specifications nested list of strings:
#' #'
#' #' @return list
#' #' @export
#' #' @section rc format: rc is a nested list of strings. The top layer should be
#' #'   named after model ids. The bottom list should have names "response" or
#' #'   "control". E.G. : \code{list(model_1 = list(response = "x", control "y"),
#' #'   model_2 = list(response = "x", control = "z"))}
#' #' @section mzid keyword: "mzid" is a special keyword. If it is in the response,
#' #'   it will generate a list where the response is each available mzid. If it is
#' #'   in the controls, it will generate a list where each control is each
#' #'   available mzid in the dataset. Note that this (should) also handle cases
#' #'   where an mzid variable is already specified in the response or control.
#' #' @section Survival Models: survival models require that the length of the
#' #'   response be of length 2 with the first being the time element and the
#' #'   second being the indicator element
#' #' @section Data Setup: If an mzid response is more than 20% missing, it is
#' #'   tested as both binary factor (present/not present) and continuous
#' #' @note Variables with only two unique non-NA values are converted to factor
#' #'   variables.
#' #' @note mzid variables automatically have NAs transformed to 0s.
#' #' @note currently, only the response and control fields are checked or
#' #'   transformed. Other fields are passed
#' modeling_prep <- function(data, model_specifications) {
#'
#'     assertthat::assert_that(
#'         inherits(data, 'data.frame')
#'     )
#'
#'     purrr::walk(model_specifications, ~.specification_checks(., data))
#'
#'     # Cleanup some mzid vars
#'     for (mzid in mzid_cols(data)) {
#'         # mzid variables should have NAs filled in as 0.
#'         data[[mzid]] %<>% na_to_zero()
#'
#'         # mzid variables should all be greater than 0.
#'         assertthat::assert_that(
#'             all(data[[mzid]] >= 0),
#'             msg = paste(mzid, "has values less than 0")
#'         )
#'     }
#'
#'     # If an mzid response is more than 20% missing, it is tested as both binary
#'     # factor (present/not present) and continuous
#'     for (mzid in mzid_cols(data)) {
#'         if (mean(data[[mzid]] == 0) >= 0.2) {
#'             # modify data
#'             binary_mzid_name <-
#'                 paste0(mzid, "_20binary")
#'
#'             data[[binary_mzid_name]] <-
#'                 as.factor(as.numeric(data[[mzid]] != 0))
#'
#'         }
#'     }
#'
#'     # mzid expand formulas
#'     model_specifications %<>% purrr::map(~.expand_mzid(., data))
#'
#'     # Convert to data_frame
#'     model_specifications %<>%
#'         purrr::flatten() %>%
#'         purrr::map(~dplyr::as_data_frame(list(fo = .))
#'         ) %>%
#'         dplyr::bind_rows(.id = 'model_name')
#'
#'     model_vars <- unique(unlist(purrr::map(model_specifications$fo, all.vars)))
#'
#'     # Trim the dataset
#'     # Use !! to error in case a variable is not found.
#'     data %<>%
#'         dplyr::select(!!model_vars)
#'
#'     # IF only two unique values, convert to factor
#'     for (mv in model_vars) {
#'         if (has_two_unique_values(data[[mv]]) & !is.factor(data[[mv]])) {
#'             message(paste(
#'                 mv,
#'                 "has only two unique values. It will be converted to factor"
#'             ))
#'             data[[mv]] <- as.factor(data[[mv]])
#'         }
#'     }
#'
#'     bdc_data <- list(data = data, model_specifications = model_specifications)
#'     class(bdc_data) <- 'bdc_data'
#'     return(bdc_data)
#'
#'
#' }
#'
#' .mzid_expand.list <- function(model_specification, data) {
#'     purrr::map(model_specification, ~.mzid_expand(., data))
#' }
#'
#' .mzid_expand.formula <- function(model_specification, data) {
#'     modeling_vars <- all.vars(model_specification)
#'     # response <- get_response(fo)
#'
#'     # Deparse sometimes splits the forumla into multiple strings Paste avoids
#'     # that.
#'     char_fo <- as.character.formula(model_specification)
#'
#'     # general mzid
#'     if (stringr::str_detect(char_fo, 'mzid(?!\\_)')) {
#'         mzid_already_included <-
#'             grep('^mzid\\_',
#'                  modeling_vars,
#'                  value = TRUE
#'             )
#'         mzid_to_expand <-
#'             setdiff(mzid_cols(data), mzid_already_included)
#'
#'         expanded_fos <-
#'             purrr::map(mzid_to_expand, function(mzid) {
#'                 sub_fo <-
#'                     stringr::str_replace_all(char_fo, 'mzid(?!\\_)', mzid) %>%
#'                     stats::as.formula()
#'             })
#'
#'         names(expanded_fos) <- rep('fo', length(expanded_fos))
#'
#'         return(expanded_fos)
#'
#'
#'     } else  {
#'         return(list(fo = model_specification))
#'     }
#' }
#'
#' .mzid_binarize <- function(model_specification, data) {
#'     UseMethod('.mzid_binarize', model_specification)
#' }
#'
#' .mzid_binarize.formula <- function(model_specification, data) {
#'     # formula --> list of formulas
#'     modeling_vars <- all.vars(model_specification)
#'     # response <- get_response(fo)
#'
#'     # Deparse sometimes splits the forumla into multiple strings Paste avoids
#'     # that.
#'     char_fo <- purrr::reduce(deparse(model_specification), paste0)
#'
#'     if (stringr::str_detect(char_fo, 'mzid\\_')) {
#'         # This needs to be adjusted so that if a single mzid variable is
#'         # specified that is more than 20% missing, it still gets a binary form
#'         # even if a general 'mzid' is not present. If there are multiple mzid
#'         # that fit this criteria, we need to expand it to an all pairwaise
#'         # combinations of models
#'
#'         mzid_already_included <-
#'             grep('^mzid\\_',
#'                  modeling_vars,
#'                  value = TRUE
#'             ) %>%
#'             purrr::set_names()
#'
#'         # Just grab all variables that start with the variable name iteself.
#'         # Since the binary variables are just the name plus a suffix, this
#'         # should grab any binary ones that exist.
#'         mzid_to_expand <-
#'             purrr::map(mzid_already_included, function(mzid) {
#'                 grep(mzid, names(data), value = TRUE)
#'             })
#'
#'         # Create all pairwise combinations of mzid from all expandable mzid.
#'         # This creates a list where each element is a named list, where the name
#'         # is the original mzid, and the element at that list is the replacement,
#'         # which may or may not be equal to the original
#'         mzid_to_expand %<>% purrr::cross()
#'
#'         # We map over the sets, and then loop over the names (original) and the
#'         # value (the replacement)
#'         expanded_fos <-
#'             purrr::map(mzid_to_expand, function(lst) {
#'
#'                 for (original_mzid in names(lst)) {
#'                     replacement_mzid <- lst[[original_mzid]]
#'                     char_fo %<>% stringr::str_replace_all(original_mzid, replacement_mzid)
#'                 }
#'
#'                 return(stats::as.formula(char_fo))
#'             })
#'
#'         return(expanded_fos)
#'
#'
#'     } else {
#'         return(list(model_specification))
#'     }
#' }
#'
#' .rc_to_fo <- function(lst, data) {
#'     response <- lst$response
#'     control <- lst$control
#'
#'     control %<>% paste(collapse = ' + ')
#'
#'     if (length(response) == 2) {
#'         if (has_two_unique_values(data[[response[[1]]]])) {
#'             response %<>% rev()
#'         }
#'
#'         response %<>% paste(collapse = ', ')
#'         fo <- stats::as.formula(paste('survival::Surv(', response, ')', '~', control))
#'
#'     } else {
#'         fo <- stats::as.formula(paste(response, '~', control))
#'     }
#'
#'     return(fo)
#' }
#'
#'
