
auth_google_pa <- function() {

  op <- options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = TRUE,
    # specify auth tokens should be stored in a hidden directory ".secrets"
    gargle_oauth_cache = file.path(sv$app_path, paste0(sv$name, "-app"), "/.secrets/")
  )

  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

  googlesheets4::gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")

  on.exit(options(op))

}

