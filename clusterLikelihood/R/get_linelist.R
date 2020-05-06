#' @include load_default.R

get_linelist <- function(df, context){

  known_symptom <- colnames(df) %in% colnames(default_mode)
  if(any(!known_symptom)) {
    print('some symptoms passed to linelist unknown, excluded:')
    print(colnames(df)[!known_symptom])
  }

  df <- as_tibble(df) %>%
        select(any_of(context$symptom_names)) %>%
        mutate(case_number=1:nrow(df))

  ll <- tibble(case_number=1:nrow(df)) %>%
            full_join(df, by='case_number') %>%
    select(-'case_number')

  ll
}
