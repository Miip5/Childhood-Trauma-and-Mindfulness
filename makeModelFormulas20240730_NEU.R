outcomeCols <- c("ffmqSumScore", "observeSum", "describeSum", "actAwareSum", "nonjudgeSum", "nonreactiveSum")
predCols <- c("etisSumScore", "GTSumScore", "EASumScore", "PPSumScore", "SESumScore")
#outcomeCols <- c("etisSumScore", "GTSumScore", "EASumScore", "PPSumScore", "SESumScore")
#predCols <- c("ffmqSumScore", "observeSum", "describeSum", "actAwareSum", "nonjudgeSum", "nonreactiveSum")
adjCols <- c("Sex", "Age")
# dat <- readRDS(file="Data_ChildhoodTrauma_Mindfulness_NEW.rds")[,c(outcomeCols, predCols, adjCols)]
dim(dat)
colnames(dat)

# produce 5*6 = 30 formulas (in the same order as in WorkingScript_MA_29.7.R (script lines 253-926):
regressionFormulas <- list()
countOutcome <- countPred <- 1
for(p in predCols) {
    for(o in outcomeCols) {
        regressionFormulas[[paste0(o, "_", countOutcome, "_", countPred)]] <- formula(paste0(o, " ~ ", paste(c(p, adjCols), collapse="+")))
        countOutcome <- countOutcome + 1
    }
    countPred <- countPred + 1
}

# produce 5*6 = 30 formulas for the rank regression
rankRegressionFormulas <- list()
countOutcome <- countPred <- 1
for(p in predCols) {
    for(o in outcomeCols) {
        rankRegressionFormulas[[paste0(o, "_", countOutcome, "_", countPred)]] <- formula(paste0(paste0("r(", o, ")"), " ~ ", paste(c(paste0("r(", p, ")"), adjCols), collapse="+")))
        countOutcome <- countOutcome + 1
    }
    countPred <- countPred + 1
}

# Produce a list with the 4 variables that are involved in the current of the 30 rank regressions.
variablesInvolvedInRangRegression <- list()
countOutcome <- countPred <- 1
for(p in predCols) {
    for(o in outcomeCols) {
        variablesInvolvedInRangRegression[[paste0(o, "_", countOutcome, "_", countPred)]] <- c(o, p, adjCols)
        countOutcome <- countOutcome + 1
    }
    countPred <- countPred + 1
}

