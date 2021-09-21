valid_udftest <- function(query = "SELECT ?(21)",
                          expect = 21,
                          description = "some description",
                          example = TRUE) {
  udftest$new(
    query = query,
    expect = expect,
    description = description,
    example = example
  )
}

ut1 <- valid_udftest()

test_that("constructor input validation", {
  expect_error(udftest$new())
  expect_error(valid_udftest(query = TRUE))
  expect_error(valid_udftest(query = ""))
  expect_error(valid_udftest(query = NA_character_))
  expect_error(valid_udftest(query = c("SELECT ?(21)", "SELECT ?(22)")))
  expect_error(valid_udftest(name = TRUE))
  expect_error(valid_udftest(name = ""))
  expect_error(valid_udftest(name = NA_character_))
  expect_error(valid_udftest(name = c("one", "two")))
  expect_error(valid_udftest(expect = NA_character_))
  expect_error(valid_udftest(expect = c(1, 2)))
  expect_error(valid_udftest(description = TRUE))
  expect_error(valid_udftest(description = NA_character_))
  expect_error(valid_udftest(description = c("desc1", "desc2")))
})


test_that("object created properly", {
  testthat::expect_true(R6::is.R6Class(udftest))

  expect_true(is_udftest(ut1))
  expect_named(ut1)
  expect_true(
    all(c("query", "expect", "description", "example") %in% names(ut1))
  )

  expect_true(is.character(ut1$query))
  expect_true(is.character(ut1$description))
  expect_true(is.logical(ut1$example))
  expect_true(is.character(ut1$example_string))
})


test_that("generics", {
  expect_false(is_udftest(TRUE))
  expect_false(is_udftest(21))

  expect_true(is.list(as.list.udftest(ut1)))
  expect_named(as.list.udftest(ut1))
  expect_setequal(
    names(as.list.udftest(ut1)),
    c("query", "expect", "description", "example")
  )

  ut2 <- as.udftest.list(list(
    query = "SELECT ?(21)",
    expect = 21,
    description = "some description",
    example = TRUE
  ))
  expect_true(is_udftest(ut2))
})
