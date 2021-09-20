#' @title User Defined Function (UDF) parameter object
#'
#' @description
#' `udfparameter` objects store details about parameters to user-defined
#' functions. Each `udfparameter` object has the following properties:
#'
#' \describe{
#'   \item{name}{Parameter's name (e.g., \code{"Sepal.Length"})}
#'   \item{type}{Parameter's data type (e.g., \code{FLOAT})}
#'   \item{description}{Detailed description of the parameter}
#' }
#'
#' @importFrom R6 R6Class
#' @export
udfparameter <- R6Class(
  classname = "udfparameter",
  private = list(
    .name = NULL,
    .type = NULL,
    .description = NULL
  ),
  #' @description Initialize a new \code{udfparameter} object
  #' @param name The parameter's name (e.g., "Sepal.Length"")
  #' @param type The parameter's data type (e.g., "FLOAT")
  #' @param description A description of the parameter
  public = list(
    initialize = function(name, type, description) {
      assertthat::assert_that(
        assertthat::is.string(name),
        assertthat::noNA(name),
        nchar(name) > 1,
        assertthat::is.string(type),
        assertthat::noNA(type),
        nchar(type) > 1,
        assertthat::is.string(description),
        assertthat::noNA(description),
        nchar(description) > 1
      )
      self$name <- name
      self$type <- type
      self$description <- description
    }
  ),
  active = list(
    name = function(x) {
      if (missing(x)) {
        private$.name
      } else {
        private$.name <- x
      }
    },
    type = function(x) {
      if (missing(x)) {
        private$.type
      } else {
        private$.type <- x
      }
    },
    description = function(x) {
      if (missing(x)) {
        private$.description
      } else {
        private$.description <- x
      }
    }
  )
)


#' @title Test whether an object is a UDF parameter
#' @description `is_udfparameter()` returns `TRUE` if the given object
#' is a `udfparameter` object or subclass thereof
#' @param x An object
#' @export
is_udfparameter <- function(x) {
  rlang::inherits_any(x, "udfparameter")
}


# Create a list from a udfparameter object
#' @export
as.list.udfparameter <- function(x, ...) {
  list(
    name = x$name,
    type = x$type,
    description = x$description
  )
}


# Create a udfparameter from a list
as.udfparameter.list <- function(x) {
  udfparameter$new(
    name = x$name,
    type = x$type,
    description = x$description
  )
}
