## code to prepare `product_codes_HS22_V202401` dataset goes here
# Importe le fichier contenant tous les codes produits HS6
product_codes_HS22_V202401 <-
  "data-raw/product_codes_HS22_V202401.csv" |>
  readr::read_csv()

usethis::use_data(product_codes_HS22_V202401, overwrite = TRUE)
