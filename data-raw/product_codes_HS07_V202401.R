## code to prepare `BACI_HS07_V202401` dataset goes here
product_codes_HS07_V202401 <-
  "data-raw/product_codes_HS07_V202401.csv" |>
  readr::read_csv()
usethis::use_data(product_codes_HS07_V202401, overwrite = TRUE)
