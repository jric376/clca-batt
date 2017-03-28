library(testthat)

setwd("..")
source("scripts/dispatch_curve.R")

test_that("Test disp_curv: cost stats columns", {
            disp <- get_test_curve("nyiso")  
            
            expected <- c("plprmfl", "MC_mean", "MC_sd")
            actual <- disp$get_mc_stats()
            expect_named(actual, expected)
})

test_that("Test disp_curv: ISO plants length", {
            disp <- get_test_curve("nyiso")
  
            expected <- 327
            actual <- nrow(disp$get_iso_plants())
            expect_equal(actual, expected)
})

test_that("Test disp_curv: cost join w/o NAs", {
            disp <- get_test_curve("nyiso")
            
            costs <- anyNA(select(disp$get_iso_plants(),
                                  starts_with("MC")))
            expect_false(costs)
})

test_that("Test disp_curv: emissions calculated w/o NAs", {
            disp <- get_test_curve("nyiso")
            
            emish <- anyNA(select(disp$get_dispatch(),
                                  ends_with("plc2erta")))
            expect_false(emish)
})

test_that("Test disp_curv: color labels", {
            disp <- get_test_curve("nyiso")
            
            expected <- 0
            empty_colors <- nrow(filter(disp$get_dispatch(),
                                          fuel_type == "black")
            )
            expect_equal(empty_colors, expected)
            
})