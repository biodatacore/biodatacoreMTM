#' Run linear models and collect lots of output.
#'
#' @rdname modeling
#' @export
# lm_rank <- function(bdc_data, preprocess = preprocess_default,
#                     no_intercept_in_output = TRUE,
#                     scale_pvalue_by_model = TRUE) {
#
#     # todo check that no response has length 2
#     responses <-
#         bdc_data$model_specifications$fo %>%
#         purrr::map(get_response)
#
#     stopifnot(all(purrr::map_dbl(responses, length) == 1))
#
#     model_data <- get_data(bdc_data)
#     model_data %<>% preprocess()
#
#     model_specifications <- get_specifications(bdc_data)
#
#     .choose_model_type <- .lm_choose_model_type
#     .train_model <- .lm_train_model
#     .tidy_model <- .lm_tidy_model
#
#     cat("\n")
#     print("Model Setup")
#
#     # for some reason, `do` is throwing away other columns, thus `bind_cols` at
#     # end
#
#     model_specifications %<>%
#         dplyr::rowwise(.) %>%
#         dplyr::do(
#             model_type = .choose_model_type(.data, model_data)
#         ) %>%
#         dplyr::bind_cols(model_specifications)
#
#     cat("\n")
#     print("Modeling")
#
#     trained_models <-
#         model_specifications %>%
#         dplyr::rowwise(.) %>%
#         dplyr::do(model = .train_model(.data, model_data))
#
#     cat("\n")
#     print("Extracting")
#     tidy_models <-
#         trained_models %>%
#         dplyr::rowwise(.) %>%
#         dplyr::do(tidy_model = .tidy_model(.data))
#
#     output <-
#         tidy_models %>%
#         dplyr::bind_cols(model_specifications)
#
#     # Formatting output
#     output %<>%
#         dplyr::mutate(fo = as.character(.data$fo),
#                       family = .data$model_type$family,
#                       link = .data$model_type$link) %>%
#         dplyr::select(-.data$model_type) %>%
#         tidyr::unnest()
#
#     cat("\n")
#     print("Calculating Ranks")
#
#     if (no_intercept_in_output) {
#         output %<>%
#             dplyr::filter(.data$term != "(Intercept)")
#     }
#
#     if (scale_pvalue_by_model) {
#         output %<>%
#             dplyr::group_by(.data$model_name)
#     }
#
#     ranks <-
#         output %>%
#         dplyr::arrange(.data$p.value) %>%
#         dplyr::mutate(rank = seq_along(.data$p.value),
#                       bonf_pvalue = stats::p.adjust(.data$p.value, "bonferroni"),
#                       fdr_pvalue  = stats::p.adjust(.data$p.value, "fdr")
#         ) %>%
#         dplyr::ungroup() %>%
#         dplyr::select(.data$model_name, .data$response, .data$control,
#                       .data$family, .data$link, dplyr::everything()
#         )
#
#     cat("\n")
#     print("Done")
#     return(ranks)
#
# }
#
# # IDEA use .data instead of d for functions that take data frames
# .lm_choose_model_type <- function(lst, data) {
#     re <- get_response(lst$fo)
#
#     # cannot use `ifelse`` to return a list
#     if (is.factor(data[[re]]) & has_two_unique_values(data[[re]])) {
#         stats::binomial(link = 'logit')
#     } else if (is.double(data[[re]])) {
#         stats::gaussian(link = 'identity')
#     } else {
#         stop(
#             paste('Trouble determining model type for',
#                   re,
#                   '. It should continuous or a two value factor.'
#             )
#         )
#     }
#
# }
#
# # IDEA use .data instead of d for functions that take data frames
# .lm_train_model <- function(lst, data) {
#
#     fo <- lst$fo
#     fam <- lst$model_type
#     response <- get_response(fo)
#     control <- get_control(fo)
#
#     md <-
#         dplyr::select(data,
#                       dplyr::one_of(response, control))
#
#     if (fam$family == 'gaussian' & fam$link == 'identity') {
#         model <-
#             stats::lm(fo,
#                       data = md,
#                       na.action = stats::na.omit
#             )
#     } else {
#         model <-
#             stats::glm(fo,
#                        family = fam,
#                        data = md,
#                        na.action = stats::na.omit
#             )
#     }
#
#     return(model)
# }
#
# .lm_tidy_model <- function(lst) {
#
#     base_model <- lst$model
#     coefs <- base_model$coefficients
#
#     # These shenanigans is due to the fact that `broom::tidy` creates its
#     # summaries in two ways. The first by grabbing the coefficents from the
#     # model summary. The second by calling `confint` on the model. And then it
#     # cbinds it together. The issue is that if there is an NA coefficent, the
#     # coefficents will ignore the NA terms while `confint` will include them.
#     # This breaks the cbind, as it is attempting to cbind dataframes with
#     # different numbers of rows. As such, we call `tidy` without confidence
#     # intervals, add in the missing NA rows, and then call `confint` and cbind
#     # them manually
#
#     if (anyNA(coefs)) {
#         which_coefs_are_na <- names(coefs)[is.na(coefs)]
#         partial_tidy_model <- broom::tidy(base_model, conf.int = FALSE)
#
#         rows_to_add <- data.frame(term = which_coefs_are_na,
#                                   estimate = rep(NA, length(which_coefs_are_na)),
#                                   std.error = rep(NA, length(which_coefs_are_na)),
#                                   statistic = rep(NA, length(which_coefs_are_na)),
#                                   p.value = rep(NA, length(which_coefs_are_na))
#         )
#
#         partial_tidy_model %<>% rbind(rows_to_add)
#
#         # from broom::tidy()
#         CI <- suppressMessages(stats::confint(base_model, level = 0.95)) # level = conf.level
#         colnames(CI) <- c("conf.low", "conf.high")
#         rownames(CI) <- NULL
#         tidy_model <- cbind(partial_tidy_model, CI)
#
#     } else {
#         tidy_model <- broom::tidy(base_model, conf.int = TRUE)
#     }
#
#     tidy_model$neg_log10_pvalue <- -1*log10(tidy_model$p.value)
#     tidy_model$sample_size <- length(base_model$residuals) # This is okay beacuse stats::na.omit doesnt pad with NAs for NA values
#
#     fo <- stats::formula(base_model)
#     # ::lhr/rhs technically returns a 'call' object, though the formula method
#     # works just fine in this case.
#     tidy_model$response <- as.character.formula(formula.tools::lhs(fo))
#     tidy_model$control  <- as.character.formula(formula.tools::rhs(fo))
#     return(tidy_model)
# }
