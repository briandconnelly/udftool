valid_udfparameter <- function(name = "my_udf",
                               type = "FLOAT",
                               description = "some description") {
  udfparameter$new(name = name, type = type, description = description)
}

up1 <- valid_udfparameter()

test_that("constructor input validation", {
  expect_error(udfparameter$new())
  expect_error(valid_udfparameter(name = TRUE))
  expect_error(valid_udfparameter(name = ""))
  expect_error(valid_udfparameter(name = NA_character_))
  expect_error(valid_udfparameter(name = c("one", "two")))
  expect_error(valid_udfparameter(type = TRUE))
  expect_error(valid_udfparameter(type = ""))
  expect_error(valid_udfparameter(type = NA_character_))
  expect_error(valid_udfparameter(name = c("INT4", "FLOAT")))
  expect_error(valid_udfparameter(description = TRUE))
  expect_error(valid_udfparameter(description = ""))
  expect_error(valid_udfparameter(description = NA_character_))
  expect_error(valid_udfparameter(name = c("desc1", "desc2")))
})

test_that("object created properly", {
  testthat::expect_true(R6::is.R6Class(udfparameter))

  expect_true(is_udfparameter(up1))
  expect_named(up1)
  expect_true(all(c("type", "name", "description") %in% names(up1)))

  expect_true(is.character(up1$name))
  expect_true(is.character(up1$type))
  expect_true(is.character(up1$description))

  expect_equal(up1$name, "my_udf")
  expect_equal(up1$type, "FLOAT")
  expect_equal(up1$description, "some description")
})

test_that("generics", {
  expect_false(is_udfparameter(TRUE))
  expect_false(is_udfparameter(21))

  expect_true(is.list(as.list(up1)))
  expect_named(as.list(up1))
  expect_setequal(names(as.list(up1)), c("name", "type", "description"))

  up2 <- as.udfparameter.list(
    list(name = "my_other_udf", type = "INT4", description = "some description")
  )
  expect_true(is_udfparameter(up2))
})
