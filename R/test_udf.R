#' @title Run a UDF's tests
#'
#' @description
#' \code{test_udf} TODO
#'
#' @param udf A \code{\link{udf}} object
#' @param conn A \link[DBI]{DBIConnection-class} object, as returned by \link[DBI]{dbConnect}
#'
#' @import cli
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
test_udf <- function(udf, conn) {
    assertthat::assert_that(
        is_udf(udf)
    )

    cli_h1("Testing {udf$full_name}")

    if (length(udf$tests) == 0) {
        cli_alert_warning("{.emph No tests defined}")
        return()
    }

    num_passed <- 0
    num_failed <- 0

    cli_div(
        theme = list(
            span.emph = list(color = "orange")
        )
    )
    cli_par()

    for (t in udf$tests) {
        query <- stringr::str_replace(
            string = t$query,
            pattern = "\\?",
            replacement = udf$full_name
        )
        result <- DBI::dbGetQuery(
            conn = conn,
            statement = query
        )[[1]]

        expect <- ifelse(is.null(t$expect), "NULL", t$expect)
        result <- ifelse(is.na(result), "NULL", result)

        if (result == expect) {
            cli_alert_success(t$description)
            num_passed <- num_passed + 1
        } else {
            cli_alert_danger("{t$description}: {.emph received {result}, but expected {expect}}")
            num_failed <- num_failed + 1
        }
    }

    cli_end()

    cli_div(
        theme = list(.pass = list(color = "green", "font-weight" = "bold"), .fail = list(color = "red", "font-weight" = "bold"))
    )
    cli_par()
    cli_text("{.pass {num_passed}} test{?s} passed, {.fail {num_failed}} failed")
    cli_end()
}
