context('Errors')

test_that('Test file loading and cleaning', {
  expect_error(eq_load_data("test"))
  fn <- system.file('extdata', "signif.txt", package = 'eqvispkg')
  expect_true(file.exists(fn))
  df <- eq_load_data(fn)
  expect_that(df, is_a("data.frame") )
  expect_error(df %>% select(-DAY) %>% eq_clean_data() )
  expect_error(df %>% mutate(LONGITUDE = "Test" %>% eq_clean_data() ) )
})

test_that('Test plot visualizations', {
  t <- geom_timeline(data = df, aes(x=DATE))
  expect_that(t, is_a("ggproto") )
  t <- geom_timeline_label(data = df, aes(x=DATE, label = LOCATION_NAME))
  expect_that(t, is_a("ggproto") )
  t <- theme_timeline()
  expect_that(t, is_a("theme") )
})

test_that('Test leaflet visualizations', {
  fn <- system.file('extdata', "signif.txt", package = 'eqvispkg')
  df <- eq_load_data(fn)
  df <- df[df$I_D == 5595,]
  l <- eq_create_label( data = df )
  expect_equal (nchar(l), 105 )
  m <- eq_map(data = df, annot_col = "DATE")
  expect_that(m, is_a("leaflet") )
})

