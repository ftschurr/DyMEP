test_that("test available_crops_and_phases",{
  expect_type(available_crops_and_phases(),"list")
})



test_that("test available_environmental_covariates",{
  expect_type(available_environmental_covariates(),"list")
})




test_that("test best_DyMEP_model",{
  expect_error(best_DyMEP_model(c("banana","tadumtadaa"),
                                       c("no_phase"),"no_crop"))
})


test_that("test predhelp.check_pheno_phase_order",{
  expect_type(predhelp.check_pheno_phase_order(list("sowing-emergence" =
                                                      c("tasmin","VPD","SPI"),
                                                  "emergence-jointing"=
                                                  c("tas","tasmin","VPD","SPI"),
                                              "jointing-heading" =
                                    c("global_radiation","tas","SPI")),
                                    "WW"),"list")
})


