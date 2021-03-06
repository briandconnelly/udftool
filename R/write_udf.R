#' Write UDF definition file
#'
#' `write_udf()` writes a [`udf`] object to a file
#'
#' @param x A [`udf`] object
#' @inheritParams yaml::write_yaml
#' @inheritDotParams yaml::write_yaml
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_udf(my_udf, "my_udf.yml")
#' }
write_udf <- function(x, file, ...) {
  if (!is_udf(x)) {
    cli::cli_abort("{.arg x} argument must be a udf object")
  }

  x$write(file, ...)
}
