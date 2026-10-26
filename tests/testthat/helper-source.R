# Load every function in R/ so that each test file can use them.
for (f in list.files(here::here("R"), pattern = "[.]R$", full.names = TRUE)) {
  source(f)
}
