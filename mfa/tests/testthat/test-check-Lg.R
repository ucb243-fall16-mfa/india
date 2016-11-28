context("Check Lg and Lg_table functions")

source("../testData.R")
load("../testMFAobj.rda")

test_that("Lg and Lg_table work", {
    ## create test tables to operate on
    t1 <- testData[,2:7]
    t2 <- testData[,8:13]

    ## the desired result for Lg_table
    matResult <- structure(c(1.05858, 0.91804, 0.91804, 1.057327),
                           .Dim = c(2L, 2L))
    
    expect_true(identical(0.977769, round(Lg(t1, t2), 6)))
    expect_true(identical(matResult,
                          round(Lg_table(MFA,
                                         sets = list(1:6,7:12)), 6))
                )
})
