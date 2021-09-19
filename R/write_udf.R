#' Write UDF definition file
#'
#' @x A \code{\link{udf}} object
#' @inheritParams yaml::write_yaml
#' @param ... Additional arguments passed to \code{\link[yaml]{write_yaml}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_udf(my_udf, "my_udf.yml")
#' }
write_udf <- function(x, file, ...) {
    if (!is_udf(x)) {
        stop("x argument must be a udf object")
    }

    x$write(file, ...)
}
