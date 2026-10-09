library(testthat)

test_that("account_archive errors for invalid login", {
  expect_error(account_archive(list(valid = FALSE)), regexp = "TRUE")
})

test_that("account_archive parses mocked archive page", {
  testthat::local_mocked_bindings(
    session_jump_to = function(x, url) list(dummy = TRUE),
    html_table = function(session) {
      list(data.frame(
        Name = NA,
        Delete = NA,
        V1 = "SPY",
        V2 = "2023-01-03",
        V3 = "2023-01-03",
        V4 = 1,
        V5 = 12345,
        stringsAsFactors = FALSE
      ))
    },
    html_nodes = function(session, selector) list("node"),
    html_attr = function(x, attr) "download.php?id=42",
    .env = asNamespace("lobsteR")
  )

  res <- account_archive(list(valid = TRUE, submission = list()))

  expect_s3_class(res, "data.frame")
  expect_true("id" %in% colnames(res))
  expect_equal(res$id[1], as.integer(42))
  expect_equal(res$symbol[1], "SPY")
})
