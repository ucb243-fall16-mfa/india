context("check RV and RV_table functions")

source("../testData.R")

test_that("RV and RV_table work", {
    ## create test tables to operate on
    t1 <- testData[,2:7]
    t2 <- testData[,8:13]

    ## The desired answer for RV_table (rounded...)
    matResult <- structure(c(1, 0.97018, 0.97018, 1), .Dim = c(2L, 2L))

    ## run tests:
    ## 0.9701797 is the desired answer
    expect_true(identical(0.97018, round(RV(t1, t2), 6)))
    expect_true(identical(matResult,
                          round(RV_table(testData[,2:13],
                                         sets = list(1:6,7:12)), 6))
                          )
})
