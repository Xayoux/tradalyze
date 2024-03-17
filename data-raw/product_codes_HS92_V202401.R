## code to prepare `product_codes_HS92_V202401` dataset goes here

# Load the `product_codes_HS92_V202401` dataset
product_codes_HS92_V202401 <-
  "data-raw/product_codes_HS92_V202401.csv" |>
  readr::read_csv()

usethis::use_data(product_codes_HS92_V202401, overwrite = TRUE)
