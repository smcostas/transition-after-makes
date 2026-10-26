# Download helpers for the public NBA data dumps in github.com/shufinskiy/nba_data.
# Season keys follow that repository: 2023 means the 2023-24 season.

nba_data_url <- function(dataset, season) {
  paste0(
    "https://github.com/shufinskiy/nba_data/raw/main/datasets/",
    dataset, "_", season, ".tar.xz"
  )
}

# Returns the path to the extracted CSV. Skips the download if the CSV is already there.
download_nba_data <- function(dataset, season, dest_dir = here::here("data", "raw")) {
  csv <- file.path(dest_dir, paste0(dataset, "_", season, ".csv"))
  if (file.exists(csv)) {
    return(csv)
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(dest_dir, paste0(dataset, "_", season, ".tar.xz"))
  utils::download.file(nba_data_url(dataset, season), archive, mode = "wb")
  utils::untar(archive, exdir = dest_dir)
  unlink(archive)
  csv
}
