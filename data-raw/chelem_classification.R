# chelem_classification dataset ---------------------------
## code to prepare `chelem_classification` dataset goes here
# Import the geographic CHELEM classification
df_chelem_geo_classification <-
  here::here("data-raw", "CHELEM_classification_geo_2022_02b.xlsx") |>
  readxl::read_xlsx(sheet = "CHELEM geo-classification") |>
  janitor::clean_names() |>
  dplyr::rename(iso = code_a)


# Create a df in which each country/sub-region is link to a region
# For each region, we have countries/sub-regions that make up the region.
list_regions <-
  df_chelem_geo_classification |>
  # Keep only rows where the level of the aggregate is 2
  # Highly aggregated, but less aggregated than the level 1 that aggregates everything
  # We have regions and details about the region that make it up.
  dplyr::filter(!is.na(composition), level %in% 2) |>
  dplyr::select(iso, name, composition) |>
  # Each component of the region is separated by a "+"
  # We want a row by country indicating its region
  tidyr::separate_rows(composition, sep = "\\+") |>
  dplyr::rename(
    iso_region = iso,
    name_region = name,
    iso_country = composition
  )


# Create a df in wich we define each sub-region
# Allow to decompose sub-region in different countries
# We want to use it to replace sub-regions in the first df
list_sub_regions <-
  df_chelem_geo_classification |>
  # Keep only sub-regions present in the first df :
  # Keep rows where the country iso code is present in the first df
  # if the column `composition` is NA it is a country
  # Otherwise it is a sub-region bc it is compose by multiple countries
  dplyr::filter(
    iso %in% unique(list_regions$iso_country),
    !is.na(composition)
  ) |>
  dplyr::select(iso, name, composition)


# Full join the first and second df
# If iso_country is a sub_region, we now have its composition
chelem_classification <-
  list_regions |>
  # By merging the 2 df we add the composition of the sub regions to the first df
  dplyr::full_join(list_sub_regions, by = c("iso_country" = "iso")) |>
  # Separate each component of the sub-region to have 1 row by component
  tidyr::separate_rows(composition, sep = "\\+") |>
  # If compositition is NOT NA : iso_country is a sub_region
  # We replace the sub-region ISOs with the country ISOs that make up the sub-region.
  dplyr::mutate(
    iso_country = dplyr::if_else(is.na(composition), iso_country, composition)
  ) |>
  # Keep only variables of interest : name and codes of countries and regions
  dplyr::select(!c(name, composition)) |>
  # It can append that certain composition ends by "+...".
  # We will remove all the lines that have any characters other than letters
  # (ISO codes are only made up of letters).
  dplyr::filter(grepl("^[[:alpha:]]+$", iso_country))

# Export the classification dataframe in .rda
usethis::use_data(chelem_classification, overwrite = TRUE)
