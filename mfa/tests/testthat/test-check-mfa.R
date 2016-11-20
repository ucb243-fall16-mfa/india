context("mfa arguments")

source("../testData.R")

test_that("check_data passes tests", {
    expect_true(check_data(testData[,2:13],
                           list(1:6, 7:12),
                           2, NULL, testData[,1]))
    errorData <- testData
    errorData[,2] <- "character"
    ## character vector in data
    expect_error(check_data(errorData[,2:13],
                           list(1:6, 7:12),
                            2, NULL, testData[,1]))
    ## mixed values in sets
    expect_error(check_data(testData[,2:13],
                            list(1:6, c("V1.1","V2.1","V3.1",
                                        "V4.1","V7","V8")),
                            2, NULL, testData[,1]))
    ## ncomps too large
    expect_error(check_data(testData[,2:13],
                           list(1:6, 7:12),
                            13, NULL, testData[,1]))
    ## vectors in sets overlap
    expect_error(check_data(testData[,2:13],
                           list(1:7, 7:12),
                            2, NULL, testData[,1]))
    ## weights don't sum to 1
    expect_error(check_data(testData[,2:13],
                           list(1:6, 7:12),
                            2, rep(1, 12), testData[,1]))
    ## ids too long
    expect_error(check_data(testData[,2:13],
                            list(1:6, 7:12),
                            2, rep(1, 12), letters[1:13]))
    ## ids not unique
    expect_error(check_data(testData[,2:13],
                            list(1:6, 7:12),
                            2, rep(1, 12), rep("A", 12)))
})

test_that("check_scale and check_center work", {
    expect_true(check_center(TRUE) & check_scale(TRUE))

    ## scale and center aren't logical values
    expect_error(check_center("Hi There"))
    expect_error(check_scale("Hi There"))
})
