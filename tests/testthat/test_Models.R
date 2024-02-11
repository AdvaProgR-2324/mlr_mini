test_that("test output of inducer() function", {

  cars_ds <- Dataset(datasets::cars, target = "dist")
  model1 <- InducerXGBoost(.data = cars_ds)
  indxgb <- InducerXGBoost()
  model2 <- fit(indxgb, .data = cars_ds)


 expect_equal(model1$data.name, model2$data.name)
 # model1 und model2 gleich? -> eigentlich beide Models gleich

})


# daraus evtl test basteln? Siehe Angabe von mb
# predict(model1, newdata = cars_ds[c(1, 2, 3, 4), ])
# predict(model1, newdata = data.frame(speed = 10))
