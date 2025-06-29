test_that("Validity of parameters works", {
  expect_error(
    dl_baci(version = 202401, dl_folder = here::here()),
    "version must be NULL or a string."
  )

  expect_error(
    dl_baci(revision = 92, dl_folder = here::here()),
    "revision is not a string. Keep in mind that it must be starting with \"HS\" followed by a two digit number such as \"HS92\"."
  )

  expect_error(
    dl_baci(revision = "92", dl_folder = here::here()),
    "revision must be a string starting with \"HS\" followed by a two digit number such as \"HS92\"."
  )

  expect_error(
    dl_baci(dl_folder = 92),
    "dl_folder must be a string specifying the path to a folder."
  )

  expect_error(
    dl_baci(dl_folder = here::here(), download = "TRUE"),
    "download must be a logical."
  )

  expect_error(
    dl_baci(dl_folder = here::here(), unzip = "TRUE"),
    "unzip must be a logical."
  )

  expect_error(
    dl_baci(dl_folder = here::here(), to_parquet = "TRUE"),
    "to_parquet must be a logical."
  )

  expect_error(
    dl_baci(dl_folder = here::here(), rm_zip = "TRUE"),
    "rm_zip must be a logical."
  )

  expect_error(
    dl_baci(dl_folder = here::here(), rm_csv = "TRUE"),
    "rm_csv must be a logical."
  )
})
