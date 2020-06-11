context("GBAT output")

za <- data.frame(
  name = c("Roberta's", "L'Industrie"),
  address = c("261 Moore Street", "254 S 2nd Street"),
  borough = c("Brooklyn", "Brooklyn"),
  stringsAsFactors = FALSE
  )

test_that("input data validation works", {
  expect_error(gbat(1), "You must supply a data frame")
  expect_error(gbat(as.matrix(iris)), "You must supply a data frame")
  expect_error(gbat(data.frame()), "Your input data frame doesn't have any rows")
  expect_error(gbat(za, "a", "b"), "names not found in input data frame")
  expect_error(gbat(za, "address", "borough", "zipp"), "should be one of \"zip\", \"boro\"")
  expect_error(gbat(za, "address", "borough", func = "F1a"), "should be one of \"F1A\", \"F1E\", \"FAP\"")
  expect_error(gbat(za, "address", "borough", geo_colnames = "block_face_x"), "Invalid column name selection.")
  })

test_that("geocoder returns correct number of columns", {
  expect_equal(ncol(gbat(za, "address", "borough", "boro")), 205)
  expect_equal(ncol(gbat(za, "address", "borough", "boro", append = FALSE)), 204)
  expect_equal(ncol(gbat(za, "address", "borough", "boro", func = "F1A")), 74)
  expect_equal(ncol(gbat(za, "address", "borough", "boro", func = "F1E")), 128)
  expect_equal(ncol(gbat(za, "address", "borough", "boro", func = "FAP")), 11)
  expect_equal(ncol(gbat(za, "address", "borough", "boro",
                         geo_colnames = c("cd", "nta"))), 7)
  expect_equal(ncol(gbat(za, "address", "borough", "boro",
                         geo_colnames = c("F1E_FireCompanyType", "F1A_Latitude"))), 6)

  })

test_that("output df identical to input df", {
  expect_identical(gbat(za, "address", "borough", "boro")[, 1:3], za)
  expect_identical(gbat(za, "address", "borough", "boro", append = FALSE)[, 1:2], za[, 2:3])
  expect_identical(gbat(tibble::as_tibble(za), "address", "borough", "boro")[, 1:3],
                   tibble::as_tibble(za)[, 1:3])
  expect_s3_class(gbat(tibble::as_tibble(za), "address", "borough", "boro"), "tbl_df")
  })


## TODO make this NAs for all cols except status
## expect_equal(gbat(za, "address", "name")$F1A_Longitude, c(NA_character_, NA_character_))
