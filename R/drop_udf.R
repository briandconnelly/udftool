#' @title Drop (i.e., delete) a UDF
#'
#' @description
#' \code{drop_udf} TODO
#'
#' @param udf A \code{\link{udf}} object
#' @param conn A \link[DBI]{DBIConnection-class} object, as returned by \link[DBI]{dbConnect}
#' @param cascade TODO
#'
#' @return TODO
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
drop_udf <- function(udf, conn, cascade = FALSE) {
    assertthat::assert_that(
        is_udf(udf),
        assertthat::is.flag(cascade)
    )

    drop_stmt <- stringr::str_glue("DROP FUNCTION {udf$signature} {ifelse(cascade, \"CASCADE\", \"RESTRICT\")}")

    tryCatch(
        expr = {
            DBI::dbSendStatement(
                con_redshift_ci,
                statement = drop_stmt
            )
        },
        error = function(e) {
            cli::cli_alert_danger("Error dropping {.file {udf$full_name}}: ")
            stop(e)
        }
    )

    cli::cli_alert_success("Dropped {.file {udf$full_name}}")
}
