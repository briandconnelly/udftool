#' @title User Defined Function (UDF) unit test object
#'
#' @description
#' `udftest` objects store details about unit tests for user-defined functions.
#' Each `udftest` object has the following properties:
#'
#' \describe{
#'   \item{query}{Query using the UDF. The UDF's name should be represented by
#'   '?' so that name changes are automatically handled. For example,
#'   `"SELECT ?(21)"`, would call the UDF with input value 21.}
#'   \item{expect}{Expected value returned by the query}
#'   \item{description}{Detailed description of the test}
#'   \item{example}{Whether or not to include this test in any generated
#'   documentation}
#' }
#'
#' @export
udftest <- R6::R6Class(
  classname = "udftest",
  private = list(
    .query = NULL,
    .expect = NULL,
    .description = NULL,
    .example = NULL
  ),
  public = list(
    #' @description Initialize a new `udftest` object
    #' @param query Query using the UDF
    #' @param expect Expected value returned by the query
    #' @param description Detailed description of the test
    #' @param example Whether or not to include this test in any generated
    #' documentation (default: `TRUE`)
    initialize = function(query, expect, description, example = TRUE) {
      assertthat::assert_that(
        assertthat::is.string(query),
        assertthat::noNA(query),
        nchar(query) > 1,
        assertthat::is.scalar(expect),
        assertthat::noNA(expect),
        assertthat::is.string(description),
        assertthat::noNA(description),
        nchar(description) > 1,
        assertthat::is.flag(example),
        assertthat::noNA(example)
      )

      self$query <- query
      self$expect <- expect
      self$description <- description
      self$example <- example
    }
  ),
  active = list(
    query = function(x) {
      if (missing(x)) {
        private$.query
      } else {
        private$.query <- x
      }
    },
    expect = function(x) {
      if (missing(x)) {
        private$.expect
      } else {
        private$.expect <- x
      }
    },
    #' @description HELLO
    description = function(x) {
      if (missing(x)) {
        private$.description
      } else {
        private$.description <- x
      }
    },
    #' @description TODO
    example = function(x) {
      if (missing(x)) {
        private$.example
      } else {
        private$.example <- x
      }
    },
    #' @description Produce a string used to show an example of a function's use
    example_string = function() {
      stringr::str_glue("{self$query}; -> {self$expect}")
    }
  )
)


#' @rdname udftest
#' @description `is_udftest()` checks if an object is a `udftest` object
#' @param x An object
#' @export
is_udftest <- function(x) {
  rlang::inherits_any(x, "udftest")
}


# Create a udftest object from a list
as.udftest.list <- function(x) {
  udftest$new(
    description = x$description,
    query = x$query,
    expect = x$expect,
    example = x$example
  )
}


# Create a list from a udftest object
#' @export
as.list.udftest <- function(x, ...) {
  list(
    query = x$query,
    expect = x$expect,
    description = x$description,
    example = x$example
  )
}
