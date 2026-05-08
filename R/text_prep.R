#' Prepare text responses for analysis
#'
#' Filters to text-type responses, joins with profile assignments,
#' cleans question names, and drops uninformative responses.
#'
#' @param df_cbe Long-format CBE survey data from load_survey_data()
#' @param df_profiles Profile assignments with survey_id, respondent_id,
#'   profile, profile_label columns
#' @return A tibble with text responses joined to profile labels
prepare_text_data <- function(df_cbe, df_profiles) {
  profiles <- df_profiles |>
    dplyr::select(survey_id, respondent_id, profile, profile_label)

  df_cbe |>
    dplyr::filter(question_type == "text",
                  !is.na(response),
                  stringr::str_trim(response) != "") |>
    dplyr::select(survey_id, respondent_id, question, response) |>
    dplyr::left_join(profiles, by = c("survey_id", "respondent_id")) |>
    dplyr::filter(!is.na(profile)) |>
    dplyr::mutate(question = stringr::str_remove(question, "^text_") |>
                    stringr::str_replace_all("_", " ") |>
                    stringr::str_to_title()) |>
    dplyr::filter(!stringr::str_to_lower(response) %in%
                    c("none", "nothing", "n/a", "na", ".", "-"),
                  !stringr::str_detect(stringr::str_to_lower(response), "cbe"))
}

#' Tokenize text responses into words
#'
#' Unnests text into individual word tokens, corrects common
#' misspellings, and applies word stemming.
#'
#' @param df_text Prepared text data from prepare_text_data()
#' @return A tibble with one row per word token, including word_stem
tokenize_text <- function(df_text) {
  df_text |>
    dplyr::mutate(response_id = dplyr::row_number()) |>
    tidytext::unnest_tokens(word, response, token = "words") |>
    dplyr::mutate(
      word = stringr::str_replace(word, "flores", "fluores"),
      word = stringr::str_replace(word, "floures", "fluores"),
      word_stem = SnowballC::wordStem(word, language = "en")
    )
}
