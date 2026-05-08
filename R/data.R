#' Load and clean CBE Occupant Survey data
#'
#' Reads the raw CBE database, filters to US office buildings,
#' recodes question names, tags question types, and converts
#' string responses to numeric scales.
#'
#' @param raw_db_path Path to data/raw/db_all.rds
#' @return A tibble in long format with columns: survey_id, year,
#'   respondent_id, question, response, question_type, response_num
load_survey_data <- function(raw_db_path) {
  df_cbe <- readr::read_rds(raw_db_path) |>
    dplyr::filter(survey_type == "Office",
                  building_country == "usa") |>
    dplyr::mutate(year = as.numeric(lubridate::year(survey_endDate))) |>
    dplyr::select(survey_id, year, respondent_id, question, response) |>
    dplyr::distinct()

  # recode question names
  df_cbe <- df_cbe |>
    dplyr::mutate(question = dplyr::coalesce(dplyr::recode(
      question,
      `what is your age?` = "bg_age",
      `what is your gender?` = "bg_gender",
      `which of the following best describes your workspace?` = "bg_space_type",
      `how many years have you worked in this building?` = "bg_years_building",
      `in a typical week, how many hours do you spend in your workspace?` = "bg_hours_work",
      `how long have you been working at your present workspace?` = "bg_tenure_workspace",
      `how would you describe the work you do?` = "bg_type_work",
      `on which floor is your workspace located?` = "bg_floor",
      `in which area is your workspace located?` = "bg_area",
      `are you near a window?` = "bg_near_window",
      `are you near a wall?` = "bg_near_wall",
      `are you near an exterior wall?` = "bg_near_exterior_wall",
      `in which direction do the windows face?` = "bg_window_direction",
      `how efficiently is this building performing?` = "bg_building_efficiency",
      `how well informed do you feel about using the building features?` = "bg_building_informed",

      `how satisfied are you with the air quality?` = "sat_air_quality",
      `how satisfied are you with the amount of light?` = "sat_light_amount",
      `how satisfied are you with the amount of space?` = "sat_space",
      `how satisfied are you with the building overall?` = "sat_building_overall",
      `how satisfied are you with the cleaning service?` = "sat_cleaning_service",
      `how satisfied are you with the colors and textures?` = "sat_aesthetics",
      `how satisfied are you with the comfort of your furnishings?` = "sat_furniture_comfort",
      `how satisfied are you with the ease of interaction?` = "sat_interaction",
      `how satisfied are you with the general cleanliness?` = "sat_cleanliness",
      `how satisfied are you with the general maintenance?` = "sat_maintenance",
      `how satisfied are you with the level of visual privacy?` = "sat_visual_privacy",
      `how satisfied are you with the noise level?` = "sat_noise_level",
      `how satisfied are you with the sound privacy?` = "sat_sound_privacy",
      `how satisfied are you with the temperature?` = "sat_temperature",
      `how satisfied are you with the visual comfort?` = "sat_visual_comfort",
      `how satisfied are you with your ability to adjust your furniture?` = "sat_furniture_adjust",
      `how satisfied are you with your access to a view?` = "sat_view_access",
      `how satisfied are you with your personal workspace?` = "sat_workspace_overall",
      `how satisfied are you with the amount of daylight?` = "sat_daylight",
      `how satisfied are you with the amount of electric light?` = "sat_electric_light",
      `how satisfied are you with the view content?` = "sat_view_content",

      `do your furnishings enhance or interfere with your ability to get your job done?` = "prod_furnishings",
      `does the acoustic quality enhance or interfere with your ability to get your job done?` = "prod_acoustic",
      `does the air quality enhance or interfere with your ability to get your job done?` = "prod_air",
      `does the building enhance or interfere with your ability to get your job done?` = "prod_building",
      `does the cleanliness and maintenance of this building enhance or interfere with your ability to get your job done?` = "prod_cleanliness",
      `does the lighting quality enhance or interfere with your ability to get your job done?` = "prod_lighting",
      `does the office layout enhance or interfere with your ability to get your job done?` = "prod_office_layout",
      `does the thermal comfort enhance or interfere with your ability to get your job done?` = "prod_thermal",
      `does the workspace layout enhance or interfere with your ability to get your job done?` = "prod_workspace_layout",
      `please estimate how your productivity is increased or decreased by the environmental conditions` = "prod_estimate_pct",

      `you are dissatisfied with the acoustics. which contributes?` = "dissat_acoustics",
      `you are dissatisfied with the air quality. which contributes to the problem of odors?` = "dissat_air_odors",
      `you are dissatisfied with the amount of space. which contributes?` = "dissat_space",
      `you are dissatisfied with the cleaning service. how often?` = "dissat_cleaning_frequency",
      `you are dissatisfied with the ease of interaction. which contributes?` = "dissat_interaction",
      `you are dissatisfied with the lighting. which contributes?` = "dissat_lighting",
      `you are dissatisfied with the temperature. which contributes?` = "dissat_temperature",
      `you are dissatisfied with the view content. which contributes?` = "dissat_view",
      `you are dissatisfied with the visual privacy. which contributes?` = "dissat_visual_privacy",

      `you are dissatisfied with the air quality. please rate the level of stuffy air.` = "dissat_air_stuffy_rating",
      `you are dissatisfied with the air quality. please rate the level of unclean air.` = "dissat_air_unclean_rating",
      `you are dissatisfied with the air quality. please rate the level of odors.` = "dissat_air_odors_rating",
      `you are dissatisfied with the temperature. in cold weather the temperature is?` = "dissat_temp_cold_rating",
      `you are dissatisfied with the temperature. in warm weather the temperature is?` = "dissat_temp_warm_rating",
      `you are dissatisfied with the temperature. in cold weather?` = "dissat_temp_cold_source",
      `you are dissatisfied with the temperature. in warm weather?` = "dissat_temp_warm_source",
      `you are dissatisfied with the temperature. when is it most a problem?` = "dissat_temp_when",

      `you are dissatisfied with the acoustics. which contributes?-text` = "dissat_acoustics_text",
      `you are dissatisfied with the air quality. which contributes to the problem of odors?-text` = "dissat_air_text",
      `you are dissatisfied with the amount of space. which contributes?-text` = "dissat_space_text",
      `you are dissatisfied with the ease of interaction. which contributes?-text` = "dissat_interaction_text",
      `you are dissatisfied with the lighting. which contributes?-text` = "dissat_lighting_text",
      `you are dissatisfied with the temperature. which contributes?-text` = "dissat_temperature_text",
      `you are dissatisfied with the temperature. in cold weather?-text` = "dissat_temp_cold_text",
      `you are dissatisfied with the temperature. in warm weather?-text` = "dissat_temp_warm_text",
      `you are dissatisfied with the temperature. when is it most a problem?-text` = "dissat_temp_when_text",
      `you are dissatisfied with the visual privacy. which contributes?-text` = "dissat_visual_privacy_text",

      `please describe any other issues related to acoustics` = "text_acoustics",
      `please describe any other issues related to air quality` = "text_air",
      `please describe any other issues related to being too hot or too cold` = "text_thermal",
      `please describe any other issues related to cleaning and maintenance` = "text_cleaning",
      `please describe any other issues related to cleaning services` = "text_cleaning_services",
      `please describe any other issues related to furnishings` = "text_furnishings",
      `please describe any other issues related to lighting` = "text_lighting",
      `please describe any other issues related to the layout` = "text_layout",
      `any additional comments or recommendations about the building?` = "text_building_comments"),
      question))

  # tag question type based on prefix
  df_cbe <- df_cbe |>
    dplyr::mutate(question_type = dplyr::case_when(
      stringr::str_starts(question, "bg_") ~ "background",
      stringr::str_starts(question, "sat_") ~ "satisfaction",
      stringr::str_starts(question, "prod_") ~ "productivity",
      stringr::str_starts(question, "dissat_") & stringr::str_ends(question, "_text") ~ "dissat_text",
      stringr::str_starts(question, "dissat_") & stringr::str_ends(question, "_rating") ~ "dissat_rating",
      stringr::str_starts(question, "dissat_") ~ "dissat_source",
      stringr::str_starts(question, "text_") ~ "text",
      TRUE ~ "other"))

  # fix multiple responses to single choice questions
  df_cbe <- df_cbe |>
    dplyr::mutate(response = dplyr::case_when(
      question_type == "dissat_source" ~ response,
      TRUE ~ stringr::str_remove(response, " _; .*$")))

  # convert string responses to numeric
  df_cbe <- df_cbe |>
    dplyr::mutate(response_num = dplyr::case_when(
      question_type == "satisfaction" ~
        dplyr::recode(stringr::str_to_lower(response),
                      "very dissatisfied" = 1,
                      "dissatisfied" = 2,
                      "somewhat dissatisfied" = 3,
                      "neutral" = 4,
                      "somewhat satisfied" = 5,
                      "satisfied" = 6,
                      "very satisfied" = 7,
                      "this is not important to me" = -1,
                      "cannot rate" = -1,
                      .default = NA_real_),
      question_type == "productivity" ~
        dplyr::recode(stringr::str_to_lower(response),
                      "significantly interferes" = 1,
                      "interferes" = 2,
                      "slightly interferes" = 3,
                      "somewhat interferes" = 3,
                      "neutral" = 4,
                      "slightly enhances" = 5,
                      "somewhat enhances" = 5,
                      "enhances" = 6,
                      "significantly enhances" = 7,
                      "decreased 20%" = 1,
                      "decreased 10%" = 2,
                      "decreased 5%" = 3,
                      "increased 5%" = 5,
                      "increased 10%" = 6,
                      "increased 20%" = 7,
                      .default = NA_real_),
      question_type == "dissat_rating" ~
        dplyr::recode(stringr::str_to_lower(response),
                      "major problem" = 1,
                      "a problem" = 2,
                      "minor problem" = 3,
                      "neutral" = 4,
                      "not a problem" = 5,
                      "too hot" = -1,
                      "too cold" = -2,
                      .default = NA_real_),
      TRUE ~ NA_real_))

  df_cbe
}
