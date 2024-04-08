test_that("Test the regular linear DRC curve",{
  expect_identical(reg_linear_prediction(10,params=list("intercept_value"=1,
                                                        "slope_value"=5)),51)
  expect_identical(reg_linear_prediction(-5,params=list("intercept_value"=5,
                                                        "slope_value"=1)),0)
})


test_that("Test non linear DRC curve",{
  expect_identical(non_linear_prediction(-5,params=list("base_value"=5,
                                                        "slope_value"=1)),0)
  expect_identical(non_linear_prediction(5,params=list("base_value"=1,
                                                       "slope_value"=0.2)),0.8)
  expect_identical(non_linear_prediction(0,params=list("base_value"=1,
                                                       "slope_value"=0.2)),0)

})

test_that("Test asymptotic DRC curve",{
  expect_equal(round(asymptotic_prediction(5,params=list("Asym_value"=0.5,
                                                         "lrc_value"=0.2,
                                                         "c0_value"=4)),3),
               0.353)
  expect_equal(round(asymptotic_prediction(10,params=list("Asym_value"=0.1,
                                                    "lrc_value"=1,
                                                    "c0_value"=5)),3),
               0.1)
  expect_identical(asymptotic_prediction(-10,
                                         params=list("Asym_value"=0.1,
                                                     "lrc_value"=1,
                                                     "c0_value"=5)),0)
})

test_that("Test WangEngels DRC curve",{
  expect_equal(round(WangEngels_prediction(10,params =
                                             list("xmin_value"=0,
                                                  "xopt_value"=25,
                                                  "xmax_value"=35,
                                                  "r_value"=0.5)),3),0.14)
  expect_equal(round(WangEngels_prediction(30,params = list("xmin_value"=0,
                                                            "xopt_value"=25,
                                                            "xmax_value"=35,
                                                            "r_value"=1)),
                     3),0.792)
  expect_identical(WangEngels_prediction(40,params = list("xmin_value"=0,
                                                          "xopt_value"=25,
                                                          "xmax_value"=35,
                                                          "r_value"=1)),0)
  expect_identical(WangEngels_prediction(-1,params = list("xmin_value"=0,
                                                          "xopt_value"=25,
                                                          "xmax_value"=35,
                                                          "r_value"=1)),0)
})
