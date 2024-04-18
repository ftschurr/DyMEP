test_that("test pheno_phase_prediction",{
  expect_type(pheno_phase_prediction( phase_covariate_list = list(
    "sowing-emergence" = c("tasmin","VPD","SPI","tasmax","tas","RH",
                           "global_radiation"),
    "emergence-jointing"= c("tasmin","VPD","SPI","tasmax","tas","RH",
                            "global_radiation")),
    environmental_data <- data.frame("DATE" = seq.Date(
      from = as.Date("2021-01-01"), to = as.Date("2023-12-31"),by=1),
      "tas"=runif(1095,min=-10,max=40),
      "RH"=runif(1095,min=0,max=100),
      "tasmin"=runif(1095,min=-10,max=40),
      "tasmax"=runif(1095,min=-5,max=40),
      "VPD" = runif(1095,min=0,max=40),
      "SPI"= runif(1095,min=-1,max=4),
      "global_radiation"= runif(1095,min=0,max=10000)),
    phase_starting_date =as.Date("2021-01-01"),
    crop_abbrev = "WW"),
              "list")
})




