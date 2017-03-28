library(testthat)

test_that("Test disp_curv: cost stats columns", {
            disp_meta = list(
              "name" = "Doris the Dispatch",
              "run_id" = "plot",
              "ctrl_id" = "plot"
            )
            disp <- disp_curv$new(meta = disp_meta,
                                  terr = "nyiso")
  
            expected <- c("plprmfl", "MC_mean", "MC_sd")
            actual <- names(disp$get_mc_stats())
            expect_identical(actual, expected)
})