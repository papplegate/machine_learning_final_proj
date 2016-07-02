# install some packages
library(parallel)
library(doParallel)
library(caret)

# clear the workspace
rm(list = ls())

# set up parallel processing
# from https://github.com/lgreski/datasciencectacontent/blob/master/markdown/pml-randomForestPerformance.md
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# set a seed for random sampling to ensure consistent results
set.seed(1)

# uncomment the following lines to get the data
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv", method = "wget")
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv", method = "wget")

# read in the data
pml.train <- read.csv("pml-training.csv")
pml.test <- read.csv("pml-testing.csv")

# get rid of columns that don't contribute anything(??)
zero.var <- nearZeroVar(pml.train)
many.nas <- which(apply(pml.train, 2, function(x) {sum(is.na(x))}) > 0.1* length(pml.train[, 1]))
delete.cols <- unique(c(1:7, zero.var, many.nas))
pml.train <- pml.train[, -1* delete.cols]
pml.test <- pml.test[, -1* delete.cols]

# set up cross-validation method
fitControl <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

# perform the model fitting
fit <- train(classe ~ ., method = "rf", data = pml.train, preProcess = "pca", 
             trControl = fitControl)

# print the results to the screen
print(fit$results)
preds <- predict(fit, newdata = pml.test)
print(data.frame(item = pml.test[, 53], pred = preds))

# save off the results
save.image("rf_result2.Rdata")