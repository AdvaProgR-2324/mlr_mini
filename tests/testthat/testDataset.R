test_that(desc = "Correct dataset", code = {
  cars_ds <- Dataset(cars, target = "dist")

  expect_equal(cars_ds$target, "dist")
  expect_equal(cars_ds$data, cars)


})
