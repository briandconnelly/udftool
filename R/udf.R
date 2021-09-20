#' User-Defined Function (UDF) object
#'
#' UDF objects store the definition of a user-defined function along with
#' metadata and unit tests.
#'
#' @export
udf <- R6::R6Class(
  classname = "udf",
  private = list(
    .name = NULL,
    .schema = NULL,
    .version = NULL,
    .description = NULL,
    .params = NULL,
    .returns = NULL,
    .volatility = NULL,
    .language = NULL,
    .body = NULL,
    .replace = NULL,
    .authors = NULL,
    .tests = NULL
  ),
  public = list(
    #' @description Initialize a `udf` object
    #' @param name The UDF's name (e.g., `"f_calculate_stuff"`)
    #' @param version A version string (e.g., `"1.0"`)
    #' @param returns Data type returned by this UDF (e.g., `"float"`)
    #' @param volatility Given the same inputs, the function will always return
    #' the same result (`"immutable"`), will return the same result during a
    #' given statement (`"stable"`), may return different results on successive
    #' calls (`"volatile"`).
    #' @param language String indicating the language used for the UDF's body
    #' (e.g., `"sql"`)
    #' @param params A list of zero or more [`udfparameter`] objects specifying
    #' input parameters
    #' @param body The body of the UDF containing SQL, Python, or other
    #' supported language
    #' @param description A detailed description of the UDF
    #' @param schema Database schema in which to place UDF (defualt: `NA`)
    #' @param replace Logical value indicating whether existing versions should
    #' be replaced (default: `FALSE`)
    #' @param authors List of authors (e.g., `list("Jane Doe")`)
    #' @param tests A list of zero or more [`udftest`] objects specifying test
    #' cases for this UDF
    #' @param ... Additional arguments (not used)
    initialize = function(name,
                          version,
                          returns,
                          volatility = c("volatile", "stable", "immutable"),
                          language = c("sql", "plpythonu"),
                          params = list(),
                          body,
                          description,
                          schema = NA,
                          replace = FALSE,
                          authors = list(),
                          tests = list(),
                          ...) {
      self$name <- name
      self$version <- version
      self$returns <- returns

      self$volatility <- rlang::arg_match(volatility)
      self$language <- rlang::arg_match(language)

      # TODO: make sure params is either a udfparameter object or a list of them
      self$params <- params

      self$body <- body
      self$description <- description
      self$schema <- schema

      if (!is.logical(replace)) {
        cli::cli_abort("{.arg replace} parameter must be either {.val TRUE} or {.val FALSE}")
      }
      self$replace <- replace

      self$authors <- authors
      self$tests <- tests
    },
    #' @description Write UDF definition file
    #' @inheritParams write_udf
    #' @inheritDotParams yaml::write_yaml
    write = function(file, ...) {
      yaml::write_yaml(as.list(self), file, ...)
    },
    #' @description Load a user-defined function
    #' @inheritParams load_udf
    create = function(conn, test = TRUE) {
      load_udf(udf = self, conn = conn, test = test)
    },
    #' @description Drop (delete) UDF
    #' @inheritParams drop_udf
    drop = function(conn, cascade = FALSE) {
      drop_udf(udf = self, conn = conn, cascade = cascade)
    },
    #' @description Run tests
    #' @inheritParams test_udf
    test = function(conn) {
      test_udf(udf = self, conn = conn)
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
    version = function(x) {
      if (missing(x)) {
        private$.version
      } else {
        private$.version <- x
      }
    },
    returns = function(x) {
      if (missing(x)) {
        private$.returns
      } else {
        private$.returns <- x
      }
    },
    volatility = function(x) {
      if (missing(x)) {
        private$.volatility
      } else {
        private$.volatility <- x
      }
    },
    language = function(x) {
      if (missing(x)) {
        private$.language
      } else {
        private$.language <- x
      }
    },
    params = function(x) {
      if (missing(x)) {
        private$.params
      } else {
        private$.params <- x
      }
    },
    body = function(x) {
      if (missing(x)) {
        private$.body
      } else {
        private$.body <- x
      }
    },
    description = function(x) {
      if (missing(x)) {
        private$.description
      } else {
        private$.description <- x
      }
    },
    schema = function(x) {
      if (missing(x)) {
        private$.schema
      } else {
        private$.schema <- x
      }
    },
    replace = function(x) {
      if (missing(x)) {
        private$.replace
      } else {
        private$.replace <- x
      }
    },
    authors = function(x) {
      if (missing(x)) {
        private$.authors
      } else {
        private$.authors <- x
      }
    },
    tests = function(x) {
      if (missing(x)) {
        private$.tests
      } else {
        private$.tests <- x
      }
    },
    #' @description Get the UDF's full name, including sch
    full_name = function() {
      if (is.null(self$schema)) {
        stringr::str_glue("\"{self$name}\"")
      } else {
        stringr::str_glue("\"{self$schema}\".\"{self$name}\"")
      }
    },
    #' @description Get the UDF's signature (i.e., the list of input parameters)
    signature = function() {
      arg_str <- lapply(
        self$params,
        function(p) {
          if (self$language == "sql") {
            toupper(p$type)
          } else if (self$language == c("plpythonu")) {
            paste(p$name, tolower(p$type))
          } else if (self$language == "js") {
            cli::cli_abort("{.val js} not yet supported")
          }
        }
      ) %>%
        paste(collapse = ", ")

      stringr::str_glue("{self$full_name}({arg_str})")
    },
    create_statement = function() {
      stringr::str_glue("
CREATE {ifelse(self$replace, \"OR REPLACE\", \"\")} FUNCTION {self$signature}
RETURNS {self$returns} {self$volatility}
AS $$
{self$body}
$$ LANGUAGE {self$language}
            ")
    }
  )
)


#' @title Test whether an object is a UDF
#' @description `is_udf` returns `TRUE` if the given object is a UDF object or
#' subclass thereof
#' @param x An object
#' @export
#' @examples
#' \dontrun{
#' u1 <- read_udf("test_udf.yml")
#' is_udf(u1)
#' }
is_udf <- function(x) {
  rlang::inherits_any(x, "udf")
}

assertthat::on_failure(is_udf) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid udf object")
}

#' @export
as.list.udf <- function(x, ...) {
  list(
    name = x$name,
    schema = x$schema,
    version = x$version,
    replace = x$replace,
    description = x$description,
    authors = x$authors,
    params = lapply(x$params, function(t) as.list(t)),
    returns = x$returns,
    volatility = x$volatility,
    language = x$language,
    body = x$body,
    tests = lapply(x$tests, function(t) as.list(t))
  )
}
