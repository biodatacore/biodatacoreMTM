bdc_validate_metab <- function(x) {
  stopifnot(
    is.numeric(x) &
      anyNA(x) &
      biodatacoreUtils::is_non_negative(x)
  )
}

bdc_df_validate_metab_at <- function(data, .vars) {
  data %>%
    dplyr::mutate_at(rlang::UQ(.vars), ~bdc_validate_metab(.x))
}
