library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(hms)
library(purrr)
library(stringr)
library(ggplot2)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(corrplot)
library(caTools)
library(klaR)
library(tidymodels)
library(discrim)


rm(list = ls())
source('pre-process.R')
data <- read_csv("data/rcs.csv")
ds <- preprocess(data)

# ----
# Machine Learning tasks
set.seed(42)
## Classification
k <- length(levels(ds$accident_severity))
ds <- as.data.frame(ds)
(cl <- kmodes(ds, k, iter.max = 10))

# We add a column cluster to the dataset
ds$cluster <- as.factor(cl$cluster)

ds <- ds %>%
  mutate(
    hour = as.integer(hour),
    day = as.integer(day)
  )

# Now we can use the cluster as the target variables
sample <- sample.split(ds$cluster, SplitRatio = 0.8)
train <- subset(ds, sample == TRUE)
test <- subset(ds, sample == FALSE)


metrics_class <- metric_set(accuracy, f_meas, sens, spec, ppv, npv, mcc, bal_accuracy)

dt <- decision_tree() %>%
  set_engine('rpart') %>%
  set_mode('classification')

acc_fit <- dt %>%
  fit(accident_severity ~ ., data = train, verbose=TRUE)

#plot tree

acc_pred <- acc_fit %>%
  predict(test) %>%
  pull(.pred_class)



# compute the confusion matrix
xtab <- table(acc_pred, test$accident_severity)
xtab_freq <- xtab
for (i in 1:ncol(xtab)) {
  xtab_freq[, i] <- xtab[, i] / sum(xtab[, i])
}
xtab
print(xtab_freq, digits = 2)

confusionMatrix(xtab, mode = 'prec_recall')

acc_perf <- test %>%
  mutate(predictions = acc_pred) %>%
  metrics_class(truth = accident_severity, estimate = predictions)

acc_perf

# Let's try with clusters

clust_fit <- dt %>%
  fit(cluster ~ ., data = train, verbose=TRUE)

clust_pred <- clust_fit %>%
  predict(test) %>%
  pull(.pred_class)

# compute the confusion matrix
xtab <- table(clust_pred, test$cluster)
xtab_freq <- xtab
for (i in 1:ncol(xtab)) {
  xtab_freq[, i] <- xtab[, i] / sum(xtab[, i])
}
xtab
print(xtab_freq, digits = 2)
confusionMatrix(xtab, mode = 'prec_recall')

# logistic regression

lr <- multinom_reg(
  mode = 'classification',
  engine = 'nnet')

lr_fit <- lr %>%
  fit(accident_severity ~ ., data = train)


lr_prediction <- lr_fit %>%
  predict(test) %>%
  pull(.pred_class)



xtab <- table(lr_prediction, test$accident_severity)
xtab_freq <- xtab
for (i in 1:ncol(xtab)) {
  xtab_freq[, i] <- xtab[, i] / sum(xtab[, i])
}

xtab
print(xtab_freq, digits = 2)
confusionMatrix(xtab, mode = 'prec_recall', positive = '3')

lr_performance <- test %>%
  mutate(predictions = lr_prediction) %>%
  metrics_class(truth = accident_severity, estimate = predictions)

lr_performance  

# With clusters

lr_fit <- lr %>%
  fit(cluster ~ ., data = train)

lr_prediction <- lr_fit %>%
  predict(test) %>%
  pull(.pred_class)

xtab <- table(lr_prediction, test$cluster)
xtab_freq <- xtab
for (i in 1:ncol(xtab)) {
  xtab_freq[, i] <- xtab[, i] / sum(xtab[, i])
}

xtab
print(xtab_freq, digits = 2)
confusionMatrix(xtab, mode = 'prec_recall')

lr_performance <- test %>%
  mutate(predictions = lr_prediction) %>%
  metrics_class(truth = cluster, estimate = predictions)

lr_performance





