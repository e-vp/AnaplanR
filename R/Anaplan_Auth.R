
#' Simple Test function for package
#'
#' @return
#' @export
#'
#' @examples
#'

Anaplan_Test <- function(){
  "Test"
}

anaplan_auth <- function( login = get_user_login(), pass = get_user_password())
{
  resp <- httr2::request("https://auth.anaplan.com/token/authenticate") |>
    httr2::req_auth_basic(login, pass) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform()
  #  print(req, redact_headers = FALSE)
}

set_user_login <- function( login = NULL) {
  if (is.null(login) ){
    login <- askpass::askpass("Please enter login")
  }
  Sys.setenv("ANAPLANR_LOGIN" =  httr2::secret_encrypt(login, 'ANAPLANR_KEY'))
}

get_user_login <- function() {
  login <- Sys.getenv( "ANAPLANR_LOGIN" )
  if (identical( login, "")) {
    stop("No login name found")
  }
  httr2::secret_decrypt(login, 'ANAPLANR_KEY' )
}

set_user_password <- function( pass = NULL) {
  if (is.null(pass) ){
    pass <- askpass::askpass("Please enter password")
  }
  Sys.setenv("ANAPLANR_PASSWORD" =  httr2::secret_encrypt(pass, 'ANAPLANR_KEY' ))
}

get_user_password <- function() {
  pass <- Sys.getenv("ANAPLANR_PASSWORD")
  if (identical( pass, "")) {
    stop("No password found")
  }
  httr2::secret_decrypt(pass, 'ANAPLANR_KEY' )
}



