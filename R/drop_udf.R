#' @title Drop a UDF
#'
#' @description
#' `drop_udf()` deletes a UDF from the given database
#'
#' @inheritParams load_udf
#' @param cascade Whether to automatically drop objects that depend on the
#' function (default: `FALSE`)
#'
#' @return `TRUE`, invisibly, if successful
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
drop_udf <- function(udf, conn, cascade = FALSE) {
  assertthat::assert_that(
    is_udf(udf),
    assertthat::is.flag(cascade),
    assertthat::noNA(cascade)
  )

  drop_stmt <- stringr::str_glue("DROP FUNCTION {udf$signature} {ifelse(cascade, \"CASCADE\", \"RESTRICT\")}")

  tryCatch(
    expr = {
      DBI::dbSendStatement(
        conn,
        statement = drop_stmt
      )
    },
    error = function(e) {
      cli::cli_abort("Error dropping {.file {udf$full_name}}: {e}")
    }
  )

  cli::cli_alert_success("Dropped {.file {udf$full_name}}")
  invisible(TRUE)
}
