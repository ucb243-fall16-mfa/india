context("check contribution_table_dim function")

load("../testMFAobj.rda")

test_that("contribution tables operate properly", {
    ## check that params work properly
    expect_true(check_contribution_params(MFA, 2))
    expect_error(check_contribution_params(MFA, 13))
    expect_error(check_contribution_params("MFA", 2))

    ## check the result is correct
    testStruct <- structure(c(0.101133, 0.100058, 0.101051, 0.096417, 0.097523, 
                              0.10077, 0.102194, 0.095559, 0.10014, 0.105155,
                              0.095402, 0.068494, 0.15169, 0.048581, 0.063498,
                              0.104449, 0.224096, 0.134101, 0.052818, 0.056869),
                            .Dim = c(10L, 2L))
    expect_true(identical(testStruct, round(contribution_table_dim(MFA), 6)))
})
