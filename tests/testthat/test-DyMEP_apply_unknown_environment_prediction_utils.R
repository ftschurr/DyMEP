test_that("test envpredutils.period_date_creator for input lentgh",{
          expect_gt(length(envpredutils.period_date_creator(Sys.Date(),100)),
                    100)

})


test_that("test envpredutils.env_period_cutter",{
  expect_type(envpredutils.env_period_cutter(Sys.Date(),
                                             "tas",
                                             list("tas"=data.frame("DATE"=
                                                    seq(as.Date(Sys.Date()),
                                                    (Sys.Date()+100),by="day"),
                                                  "VALUE"= c(1:101))),
                                             50),"list")
  expect_error(envpredutils.env_period_cutter(Sys.Date(),
                                              "tas",
                                              list("tas"=data.frame("DATE"=
                                              seq(as.Date(Sys.Date()),
                                                  (Sys.Date()+100),by="day"),
                                              "DUMMY"= c(1:101))),
                                              50))
  expect_error(envpredutils.env_period_cutter(Sys.Date(),
                                              "tas",
                                              list("tas"=data.frame("DATE"=
                                              c(1:101),
                                              "DUMMY"= c(1:101))),
                                              50))
})


test_that("test envpredutils.data_frame_to_list",{
  expect_error(envpredutils.data_frame_to_list(list("tas"=c(1:100))))
  expect_type(envpredutils.data_frame_to_list(data.frame("tas"=c(1:100),
                                                         "VPD"=c(1:100),
                                                         "DATE"=c(1:100))),
              "list")

})

test_that("test envpredutils.cumulative_dose_response_pred_helper",{
  expect_error(envpredutils.cumulative_dose_response_pred_helper(c(1:100),
                                                          mean,list("hello"=0)))
  expect_equal(envpredutils.cumulative_dose_response_pred_helper(c(1:3),
                          function(x,parameters){return(x)},list("dummy_list")),
               c(1,3,6))

})

test_that("test envpredutils.GLM_prediction_df_creator",{
  expect_type(envpredutils.GLM_prediction_df_creator(list("tas"=list(
    "growth_cumulative"=c(1))),
    timestamp_vect = Sys.Date()),
              "list")
})

