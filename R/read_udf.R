#' Read a UDF definition file
#'
#' `read_udf()` loads a UDF definition from a file
#'
#' @inheritParams yaml::read_yaml
#' @inheritDotParams yaml::read_yaml
#'
#' @return A [`udf`] object representing the UDF
#' @export
#'
#' @examples
#' \dontrun{
#' my_udf <- read_udf("my_udf.yaml")
#' }
read_udf <- function(file, ...) {
  raw <- yaml::read_yaml(file, ...)

  udf$new(
    name = raw$name,
    version = as.character(raw$version),
    returns = raw$returns,
    volatility = tolower(raw$volatility),
    language = tolower(raw$language),
    params = lapply(
      raw$params,
      function(t) as.udfparameter.list(t)
    ),
    body = raw$body,
    description = raw$description,
    schema = raw$schema,
    replace = raw$replace,
    authors = raw$authors,
    tests = lapply(
      raw$tests,
      function(t) as.udftest.list(t)
    )
  )
}
