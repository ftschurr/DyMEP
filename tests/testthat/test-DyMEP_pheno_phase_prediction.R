test_that("test pheno_phase_prediction",{
  expect_type(pheno_phase_prediction(list("sowing-emergence" =
                                            c("tasmin","VPD","SPI"),
                "emergence-jointing"= c("tas","tasmin","VPD","SPI"),
                "jointing-heading" = c("global_radiation","tas","SPI")),
           data.frame("DATE"=seq.Date(from = as.Date("2021-01-01"),
                                      to = as.Date("2022-12-31"),by=1),
                      "tas"=runif(730,min=-10,max=40),
                      "tasmin"=runif(730,min=-10,max=40),
                      "VPD" = runif(730,min=-10,max=40),
                      "SPI"= runif(730,min=-1,max=4),
                      "global_radiation"= runif(730,min=0,max=10000)),
           phase_starting_date =as.Date("2022-01-01"),
           crop_abbrev = "WR"),
              "list")
  expect_error(pheno_phase_prediction(list("sowing-emergence" =
                           c("tasmin","VPD","SPI"),
                         "emergence-jointing"= c("tas","tasmin","VPD","SPI"),
                         "jointing-heading" = c("global_radiation","tas","SPI")),
                    data.frame("DATE"=seq.Date(from = as.Date("2021-01-01"),
                                               to = as.Date("2022-12-31"),by=1),
                               "tas"=runif(730,min=-10,max=40),
                               "tasmin"=runif(730,min=-10,max=40),
                               "VPD" = runif(730,min=-10,max=40),
                               "SPI"= runif(730,min=-1,max=4),
                               "global_radiation"= runif(730,min=0,max=10000)),
                    phase_starting_date =as.Date("2022-01-01"),
                    crop_abbrev = "randomnotexistingcrop"))

})




