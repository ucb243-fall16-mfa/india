context("check contribution functions")

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

test_that("contribution of variables compute properly", {
    ## check that params work properly
    expect_true(check_contribution_params(MFA, 2))
    expect_error(check_contribution_params(MFA, 13))
    expect_error(check_contribution_params("MFA", 2))

    ## check the result is correct
    testStruct <- structure(c(20.88509, 17.122101, 16.287021, 14.010101,
                              19.75609, 13.072286, 21.079519, 20.939521,
                              17.040115, 15.655787, 13.560667, 11.782231,
                              25.524942, 5.068117, 18.365896, 11.364582,
                              21.123891, 19.603689, 26.790671, 18.53221,
                              25.091414, 14.490456, 11.512654, 26.854877,
                              13.853603, 22.096088, 4.676222, 20.491117,
                              9.550818, 27.565988, 23.281793, 21.198041,
                              16.067858, 12.655924, 31.493009, 25.284619,
                              31.880875, 13.535705, 20.732071, 16.639111,
                              15.092809, 5.22009, 22.299225, 15.575938,
                              24.268879, 14.499905, 21.709664, 16.545543,
                              23.11594, 32.113559, 23.179258, 25.270059,
                              24.592358, 24.412753, 14.842367, 37.770575,
                              8.126163, 6.248678, 4.001629, 8.01341, 7.553727,
                              9.551586, 13.786362, 3.062213, 26.526928,
                              15.017933, 14.26602, 39.475229, 36.647567,
                              24.28958, 21.994053, 1.838718, 33.996906,
                              7.806824, 1.173437, 3.765496, 12.33969, 18.970191,
                              20.403913, 5.302653, 6.368864, 0.113078,
                              13.975401, 22.769917, 32.451188, 0.281476,
                              34.971412, 60.088897, 69.97274, 35.106981,
                              58.927455, 26.052639, 38.54862, 14.585589,
                              13.013354, 18.507129, 23.393555, 15.333973,
                              12.886873, 13.431202, 1.832587, 9.333439, 1.98883,
                              21.192356, 10.786208, 22.90164),
                            .Dim = c(53L, 2L),
                            .Dimnames = list(c("V1", "V2", "V3", "V4", "V5",
                                "V6", "V1.1", "V2.1", "V3.1", "V4.1", "V7",
                                "V8", "V1.2", "V2.2", "V3.2", "V4.2", "V9",
                                "V10", "V1.3", "V2.3", "V3.3", "V4.3", "V8.1",
                                "V1.4", "V2.4", "V3.4", "V4.4", "V11", "V12",
                                "V1.5", "V2.5", "V3.5", "V4.5", "V13", "V1.6",
                                "V2.6", "V3.6", "V4.6", "V1.7", "V2.7", "V3.7",
                                "V4.7", "V14", "V5.1", "V1.8", "V2.8", "V3.8",
                                "V4.8", "V15", "V1.9", "V2.9", "V3.9", "V4.9"),
                                NULL))
    expect_true(identical(testStruct, round(contribution_var_dim(MFA), 6)))
})
