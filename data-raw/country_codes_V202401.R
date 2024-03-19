## code to prepare `country_codes_V202401` dataset goes here
country_codes_V202401 <-
  read_csv("data-raw/country_codes_V202401.csv") |>
  dplyr::select(country_code, country_iso3) |>
  dplyr::mutate(country_code = as.integer(country_code))
usethis::use_data(country_codes_V202401, overwrite = TRUE)
