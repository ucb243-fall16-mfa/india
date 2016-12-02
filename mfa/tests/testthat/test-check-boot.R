context("Checking the bootstrap function")

load("../testMFAobj.rda")
load("../testBoot.rda")

test_that("parameters pass tests", {
    expect_true(check_B(400))
    expect_error(check_B(-1))
    expect_true(check_seed(400))
    expect_error(check_seed(-1))
    expect_true(check_mfa(MFA))
    expect_error(check_mfa("Hi!"))
})

test_that("results are defensible", {
    MFA <- bootstrap(MFA, B = 5, seed = 0)
    expect_true(identical(testBootData,
                          round((attributes(MFA)$boot)[,-3], 6)))
})
