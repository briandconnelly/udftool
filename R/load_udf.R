#' @title Load (i.e., create) a user-defined function
#'
#' @description
#' `load_udf()` executes a `CREATE FUNCTION` statement to load a user-defined
#' function on a given database server.
#'
#' @param udf A [`udf`] object
#' @param conn A [`DBI::DBIConnection-class`] object, as returned by
#' [`DBI::dbConnect()`]
#' @param test Whether or not to run tests after loading (default: `TRUE`)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_udf <- read_udf("my_udf.yaml")
#' my_db_connection <- DBI::dbConnect()
#' load_udf(my_udf, my_db_connection)
#' }
load_udf <- function(udf, conn, test = TRUE) {
    assertthat::assert_that(
        is_udf(udf)
    )

    tryCatch(
        expr = {
            DBI::dbExecute(
                con_redshift_ci,
                statement = udf$create_statement
            )
        },
        error = function(e) {
            cli::cli_abort("Error loading {.file {udf$full_name}}: {e}")
        }
    )

    cli::cli_alert_success("Successfully loaded {.file {udf$full_name}}")
    if (test) {
        udf$test(conn)
    }
}
