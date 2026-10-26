test_that("nba_data_url builds the shufinskiy/nba_data URL for a dataset and season", {
  expect_equal(
    nba_data_url("pbpstats", 2023),
    "https://github.com/shufinskiy/nba_data/raw/main/datasets/pbpstats_2023.tar.xz"
  )
})

test_that("download_nba_data does not download again when the CSV already exists", {
  dest <- withr::local_tempdir()
  csv <- file.path(dest, "pbpstats_2023.csv")
  writeLines("GAMEID", csv)
  # A wrong URL would error if a download were attempted.
  expect_equal(download_nba_data("pbpstats", 2023, dest_dir = dest), csv)
})
