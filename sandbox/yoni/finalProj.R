wines <- read.csv(paste0("https://raw.githubusercontent.com/ucb-stat243/",
                         "stat243-fall-2016/master/problem-sets/final-project",
                         "/data/wines.csv"),
                  stringsAsFactors = FALSE)

i <- grep("V", colnames(wines))
data <- wines[,i]
sets <- list(1:6, 7:12, 13:18, 19:23, 24:29,
             30:34, 35:38, 39:44, 45:49, 50:53)
    
for(file in list.files(paste0("~/Documents/Stat243/finalProject",
                              "/stat243FinalProject/mfa/R"),
                       pattern = "R$", full.names = TRUE)){
    source(file)
}

load(paste0("~/Documents/Stat243/finalProject/",
            "stat243FinalProject/mfa/tests/testMFAobj.rda"))

MFA <- mfa(data, sets, ncomps = 2, ids = wines$ID)

testP <- round(testMFA$P,6)
testQ <- round(testMFA$Q,6)
testL <- round(testMFA$lambda, 6)

save(data, sets, file = "testRawData.rda")

save(testP, testQ, testL, file = "testMats.rda")
