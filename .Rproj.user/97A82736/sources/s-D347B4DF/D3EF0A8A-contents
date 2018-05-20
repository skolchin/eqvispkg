context('Errors')

test_that('Test error conditions', {
  expect_error(fars_read_year(make_filename(2018)))
  expect_warning(fars_read_years(2018))
  expect_error(fars_summarize_years(2000))
  expect_error(fars_map_state(66, 2014))
})

context('Test file loading')

test_that('File loading', {
  df <- fars_read(make_filename(2015))
  expect_that(df, is_a('tbl_df'))
  expect_that(nrow(df), is_more_than(0))
})

test_that('Multiple years loading', {
  dfs <- fars_read_years(2013:2015)
  expect_that(dfs, is_a('list'))
  expect_that(dfs[[1]], is_a('tbl_df'))
  expect_equal(length(dfs), 3)
})

test_that('Test summarizing', {
  years <- 2013:2015
  df <- fars_summarize_years(years)
  expect_that(ncol(df), equals(length(years) + 1))
})

test_that('Test map plotting', {
  map <- fars_map_state(1, 2014)
  expect_null(map)
})
