test_that("configuration file can be loaded", {
  expect_equal(extract_config(config::get(), "shape"), "square")
  expect_equal(extract_config(config::get(), "color"), "red")
  expect_null(extract_config(config::get(), "notthere"))

})

test_that("nested configuration file can be loaded", {
  conf <- config::get(file = "config-nested.yml")
  expect_equal(extract_config(conf, "shape"), "square")
  expect_equal(extract_config(conf, "color"), "red")
  expect_null(extract_config(conf, "notthere"))
  expect_equal(extract_config(conf, "textures"), conf$textures)
  expect_equal(extract_config(conf, "variability"), conf$textures$variability)
  expect_equal(extract_config(conf, "skewness"), conf$textures$variability$skewness)
  expect_equal(extract_config(conf, "smoothness"), conf$textures$smoothness)
})
