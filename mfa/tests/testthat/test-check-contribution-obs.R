context("check contribution_obs_dim function")

load("../testMFAobj.rda")

test_that("contribution of obs compute properly", {
    ## check that params work properly
    expect_true(check_contribution_params(MFA, 2))
    expect_error(check_contribution_params(MFA, 13))
    expect_error(check_contribution_params("MFA", 2))

    ## check the result is correct
    testStruct <- structure(c(0.089643, 0.061042, 0.054033, 0.115989, 0.175819, 
                              0.149068, 0.060955, 0.079889, 0.041751, 0.000499,
                              0.021149, 0.012537, 0.002487, 9.9e-05, 0.019246,
                              0.002567, 0.001538, 0.001091, 0.003908, 0.015512,
                              0.012671, 0.053434, 0.024531, 0.000542),
                            .Dim = c(12L, 2L))
    
    expect_true(identical(testStruct, round(contribution_obs_dim(MFA), 6)))
})
