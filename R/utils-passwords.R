gen_seed_from_char <- function(x) {
  sum(
    as.numeric(
      charToRaw(x)
    )
  )
}

#' Generate Password from Character String
#'
#' @description Generate a password from a non-secret string `char` with a
#'   secret `key`.
#'
#' @param char A character string from which to generate a password.
#' @param key A character string representing a secret key for generating the
#'   password with the given character string.
#' @param pw_length (numeric) Length of the password
#'
#' @return A character string of length `pw_length` generated with a seed
#'   derived from the `char` provided (a public facing string, such as a name)
#'   as wekk as the `key` (a secret string known to the password generator).
#'
#' @export
#'
#' @examples
#'
#' # Create a password for the name "Jonathan" using the secret key "password".
#' gen_pw_from_char(char = "Jonathan", key = "password", pw_length = 10)
#'
gen_pw_from_char <- function(char, key, pw_length = 10) {

  # Generate a random seed unique to a character string
  char_seed <- gen_seed_from_char(char)

  # Generate a random seed based on the secret key
  key_seed <- gen_seed_from_char(key)

  withr::with_seed(
    seed = char_seed + key_seed,
    code = {
      smpl <- sample(x = c(letters, LETTERS, 0:9),
                     size = pw_length,
                     replace = TRUE)
      paste0(smpl, collapse = "")
    }
  )

}
