#' Authenticate with LOBSTER account
#'
#' Logs into your LOBSTER account and creates a session object for subsequent
#' data requests. This function handles the authentication process with
#' lobsterdata.com and validates the login was successful.
#'
#' @param login character(1) Email address associated with the LOBSTER account.
#' @param pwd character(1) Account password.
#'
#' @return A named list with components:
#' \describe{
#'   \item{valid}{logical(1) — `TRUE` when authentication succeeded.}
#'   \item{session}{rvest session object used for further navigation.}
#'   \item{submission}{rvest response returned after the sign-in form was submitted.}
#' }
#'
#' @details
#' The function submits the sign-in form using an AJAX header
#' (`x-requested-with: XMLHttpRequest`) and confirms success by checking the
#' redirect URL. Network connectivity and valid credentials are required.
#'
#' Store credentials in your `.Renviron` file
#' (`usethis::edit_r_environ()`) to avoid hardcoding them in scripts:
#'
#' ```
#' LOBSTER_USER=you@example.com
#' LOBSTER_PWD=your-password
#' ```
#'
#' @examples
#' \dontrun{
#' acct <- account_login(
#'   login = Sys.getenv("LOBSTER_USER"),
#'   pwd   = Sys.getenv("LOBSTER_PWD")
#' )
#'
#' if (acct$valid) {
#'   # Retrieve available datasets in the archive
#'   archive <- account_archive(acct)
#'
#'   # Build and submit a new data request
#'   req <- request_query("AAPL", "2023-01-03", "2023-01-05", level = 1)
#'   request_submit(acct, req)
#' }
#' }
#'
#' @seealso [account_archive()], [request_submit()]
#'
#' @export
#' @importFrom httr add_headers
#' @importFrom rvest session html_form html_form_set session_submit
#' @importFrom assertthat are_equal
account_login <- function(login, pwd) {
  session <- session(url = "https://lobsterdata.com/SignIn.php")

  form <- html_form(x = session)[[1]] |>
    html_form_set(login = login, pwd = pwd)

  submission <- session_submit(
    x = session,
    form = form,
    submit = "sign in",
    httr::add_headers('x-requested-with' = 'XMLHttpRequest')
  )

  valid <- assertthat::are_equal(
    x = submission$url,
    y = "https://data.lobsterdata.com/requestdata.php"
  )

  if (valid) {
    message("Login on lobsterdata.com successful")
  }
  list(
    valid = valid,
    session = session,
    submission = submission
  )
}

#' Retrieve LOBSTER data archive information
#'
#' Fetches information about available datasets in your LOBSTER account archive.
#' This includes details about symbols, date ranges, order book levels, file
#' sizes, and download links for each available dataset.
#'
#' @param account list Output from [account_login()] containing a valid
#'   authenticated session (`valid == TRUE`).
#'
#' @return A tibble with one row per available dataset and columns:
#' \describe{
#'   \item{id}{integer. Unique identifier for each dataset.}
#'   \item{symbol}{character. Stock/ETF ticker symbol (e.g., `"SPY"`, `"AAPL"`).}
#'   \item{start_date}{Date. First date of data coverage.}
#'   \item{end_date}{Date. Last date of data coverage.}
#'   \item{level}{integer. Order book depth (number of price levels).}
#'   \item{size}{integer. File size in bytes.}
#'   \item{download}{character. Direct download URL for the dataset.}
#' }
#' Rows are ordered by `id` descending (most recently requested first).
#' Datasets with zero file size (not yet processed by LOBSTER) are excluded.
#'
#' @details
#' The function navigates to the archive page of the authenticated session,
#' scrapes the archive table, extracts download links, and returns a structured
#' tibble. Only datasets with non-zero file sizes are included — datasets still
#' being processed by LOBSTER will not appear until processing is complete.
#'
#' @examples
#' \dontrun{
#' acct <- account_login(
#'   login = Sys.getenv("LOBSTER_USER"),
#'   pwd   = Sys.getenv("LOBSTER_PWD")
#' )
#'
#' archive <- account_archive(acct)
#' archive
#' #> # A tibble: 3 × 7
#' #>      id symbol start_date end_date   level    size download
#' #>   <int> <chr>  <date>     <date>     <int>   <int> <chr>
#' #> 1   102 AAPL   2023-01-03 2023-01-03     1  204800 https://…
#' #> 2   101 MSFT   2023-01-03 2023-01-05     2  512000 https://…
#' #> 3   100 SPY    2022-12-01 2022-12-31    10 1048576 https://…
#'
#' # Filter to a single symbol before downloading
#' data_download(
#'   requested_data = archive[archive$symbol == "AAPL", ],
#'   account_login  = acct,
#'   path           = "data-lobster"
#' )
#' }
#'
#' @seealso [account_login()], [data_download()]
#'
#' @export
#' @importFrom rvest session_jump_to html_table html_nodes html_attr
account_archive <- function(account) {
  stopifnot(account$valid)

  session <- session_jump_to(
    x = account$submission,
    url = "https://lobsterdata.com/data_archive.php"
  )

  archive <- html_table(session)[[1]]

  archive$Name <- archive$Delete <- NULL

  colnames(archive) <- c("symbol", "start_date", "end_date", "level", "size")

  archive$download <- html_nodes(session, "td:nth-child(2) a") |>
    html_attr("href") |>
    grep("download", x = _, value = TRUE) |>
    paste0("https://lobsterdata.com/", ... = _)

  archive$id <- gsub(".*id=", "", archive$download)

  class(archive$id) <- class(archive$level) <- class(archive$size) <- "integer"
  archive$start_date <- as.Date(archive$start_date)
  archive$end_date <- as.Date(archive$end_date)
  archive <- archive[archive$size != 0, ]
  archive[
    order(archive$id, decreasing = TRUE),
    c(ncol(archive), 1:(ncol(archive) - 1))
  ]
}
