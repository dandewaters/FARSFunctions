context("Throwing Errors")

test_that("Throws errors", {
  throws_error(fars_read("ThisFileNameDoesn'tExist.csv"))
  throws_error(fars_read_years(3000))
  throws_error(fars_summarize_years(3000:3005))
  throws_error(fars_map_state(100, 3000))
})

context("Reading files are correct")

test_that("Reads files correctly", {
  df <- fars_read_years(2013:2015)
  expect_that(df, is_a("list"))
  #expect_that(df[[1]], is_a('tbl_df'))
  expect_equal(length(df), 3)
})

context("Summarizing FARS Data is Correct")

test_that("FARS Summarize Years is correct", {
  years <- 2013:2015
  df <- fars_summarize_years(years)
  expect_that(nrow(df), equals(12))
  expect_that(ncol(df), equals(length(years) + 1))
  expect_that(names(df)[1], matches('MONTH'))
})

context("Mapping is Correct")

test_that('Mapping is Correct', {
  library(mapdata)
  map <- fars_map_state(25, 2014)
  expect_that(map, is_null())
})
