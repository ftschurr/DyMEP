test_that("test predhelputils.get_all_permutations",{
  expect_type(predhelputils.get_all_permutations(c("tas","tasmin",
                                                   "tasmax","VPD")),"character")
  expect_equal(length(predhelputils.get_all_permutations(c("tas",
                                                           "tasmin","tasmax",
                                                           "VPD"))),64)
})


test_that("test permutations_fun",{
  expect_equal(dim(permutations_fun(4,2,c("tas","tasmin","tasmax","VPD"))),
               c(12,2))
  expect_error(permutations_fun(5,2,c("tas","tasmin","tasmax","VPD")) )
})


