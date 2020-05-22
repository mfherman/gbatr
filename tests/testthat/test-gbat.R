context("GBAT output")

za <- data.frame(
  name = c("Roberta's", "L'Industrie"),
  address = c("261 Moore Street", "254 S 2nd Stret"),
  borough = c("Brooklyn", "Brooklyn")
  )


test_that("input data validation works", {
  expect_error(gbat(1), "You must supply a data frame")
  expect_error(gbat(as.matrix(iris)), "You must supply a data frame")
  expect_error(gbat(za, "a", "b"), "names not found in input data frame")
  expect_error(gbat(za, "address", "borough", "zipp", "'arg' should be one of “zip”, “boro”"))

})
