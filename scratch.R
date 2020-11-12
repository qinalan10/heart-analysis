# Load Packages
library("tidyverse")
library('caret')

# Read in the data
hd = read_csv("data/hd.csv")

# test train split the data
set.seed(42)
trn_idx = createDataPartition(hd$num, p = .8, list = TRUE)
hd_trn = hd[trn_idx$Resample1, ]
hd_tst = hd[-trn_idx$Resample1, ]

# coerce char variables to be factors
hd_trn$num = factor(hd_trn$num)
hd_tst$num = factor(hd_tst$num)

hd_trn$location = factor(hd_trn$location)
hd_tst$location = factor(hd_tst$location)

## Change cp into factors
hd_trn$cp[hd_trn$cp == 1] = 'typical angina'
hd_trn$cp[hd_trn$cp == 2] = 'atypical angina'
hd_trn$cp[hd_trn$cp == 3] = 'non-anginal pain'
hd_trn$cp[hd_trn$cp == 4] = 'asymptomatic'
hd_trn$cp = factor(hd_trn$cp)

hd_tst$cp[hd_tst$cp == 1] = 'typical angina'
hd_tst$cp[hd_tst$cp == 2] = 'atypical angina'
hd_tst$cp[hd_tst$cp == 3] = 'non-anginal pain'
hd_tst$cp[hd_tst$cp == 4] = 'asymptomatic'
hd_tst$cp = factor(hd_tst$cp)

## Change Sex into factors
hd_tst$sex[hd_tst$sex == 1] = 'male'
hd_tst$sex[hd_tst$sex == 0] = 'female'
hd_tst$sex = factor(hd_tst$sex)

hd_trn$sex[hd_trn$sex == 1] = 'male'
hd_trn$sex[hd_trn$sex == 0] = 'female'
hd_trn$sex = factor(hd_trn$sex)

## Change FBS into factors
hd_trn$fbs[hd_trn$fbs == 1] = '> 120'
hd_trn$fbs[hd_trn$fbs == 0] = '< 120'
hd_trn$fbs = factor(hd_trn$fbs)

hd_tst$fbs[hd_tst$fbs == 1] = '> 120'
hd_tst$fbs[hd_tst$fbs == 0] = '< 120'
hd_tst$fbs = factor(hd_tst$fbs)

## Change restecg into factors
hd_trn$restecg[hd_trn$restecg == 0] = 'normal'
hd_trn$restecg[hd_trn$restecg == 1] = 'ST-T Wave'
hd_trn$restecg[hd_trn$restecg == 2] = 'left hypertrophy'
hd_trn$restecg = factor(hd_trn$restecg)

hd_tst$restecg[hd_tst$restecg == 0] = 'normal'
hd_tst$restecg[hd_tst$restecg == 1] = 'ST-T Wave'
hd_tst$restecg[hd_tst$restecg == 2] = 'left hypertrophy'
hd_tst$restecg = factor(hd_tst$restecg)


## Change exang into factors
hd_trn$exang[hd_trn$exang == 0] = 'yes'
hd_trn$exang[hd_trn$exang == 1] = 'no'
hd_trn$exang = factor(hd_trn$exang)

hd_tst$exang[hd_tst$exang == 0] = 'yes'
hd_tst$exang[hd_tst$exang == 1] = 'no'
hd_tst$exang = factor(hd_tst$exang)

# Additional Feature Engineering
hd_trn[which(hd_trn$chol == 0), ]$chol = NA

# function to determine proportion of NAs in a vector
na_prop = function(x) {
  mean(is.na(x))
}


sapply(hd_trn, na_prop)

# create dataset without columns containing more than 30% NAs
hd_trn = hd_trn[, !sapply(hd_trn, na_prop) > 0.3]
hd_tst = hd_tst[, !sapply(hd_tst, na_prop) > 0.3]

# Look at descriptive statistics
skimr::skim(hd_trn)
## Accuracy Function
accuracy = function(actual, predicted) {
  mean(actual == predicted)
}
## Test out a model

#model = knn3(num ~ ., data = hd_trn, k = 5)
#mean(hd_trn$num == predict(model, hd_trn, type = 'class'))
#accuracy(actual = hd_tst$num, predicted = predict(model, hd_tst, type = 'class'))

#########################################################################################################

# Establish Baseline Accuracy


# First model
hd_trn_clean = na.omit(hd_trn)
levels(hd_trn_clean$num)[levels(hd_trn_clean$num) == 'v2'] = 'v1'
levels(hd_trn_clean$num)[levels(hd_trn_clean$num) == 'v3'] = 'v1'
levels(hd_trn_clean$num)[levels(hd_trn_clean$num) == 'v4'] = 'v1'


hd_tst_clean = na.omit(hd_tst)
levels(hd_tst_clean$num)[levels(hd_tst_clean$num) == 'v2'] = 'v1'
levels(hd_tst_clean$num)[levels(hd_tst_clean$num) == 'v3'] = 'v1'
levels(hd_tst_clean$num)[levels(hd_tst_clean$num) == 'v4'] = 'v1'


table(
  actual = hd_tst_clean$num,
  predicted = rep("v0", length(hd_tst_clean$num))
)

## Estimation-validation split
set.seed(432)
index_fold = caret::createFolds(hd_trn_clean$num, k = 5)
k_vals = 1:50

knn_rmse = function(val_idx, k = k) {
  est = hd_trn_clean[-val_idx, ]
  val = hd_trn_clean[val_idx, ]
  model = knn3(num ~ ., data = est, k = k)
  pred = predict(model, val, type = 'class')
  mean(val$num == pred)
}

knn_cv = function(k) {
  folds = mean(sapply(index_fold, knn_rmse, k = k))
}

sapply(k_vals, knn_cv)
max(sapply(k_vals, knn_cv))
knn_final = knn3(num ~ ., data = hd_trn_clean, k = k_vals[which.max(sapply(k_vals, knn_cv))])

pred = predict(knn_final, hd_tst_clean, type = 'class')
accuracy(actual = hd_tst_clean$num, predicted = pred)


## LOGISTIC

set.seed(432)
index_fold = caret::createFolds(hd_trn_clean$num, k = 5)
calc_rmse_single_log = function(val_idx) {
  est = hd_trn_clean[-val_idx, ]
  val = hd_trn_clean[val_idx, ]
  mod_log = glm(num ~ ., data = est, family = binomial())
  pred = ifelse(predict(mod_log, val, type = 'response') > .5, "v1", "v0")
  mean(pred == val$num)
}
mean(sapply(index_fold, calc_rmse_single_log))




## Other Classification

library(randomForest)

set.seed(420)
rf = randomForest(num ~ ., data = hd_trn_clean, mtry = 5, ntree = 500)

set.seed(420)
accuracy(actual = hd_tst_clean$num, predicted = predict(rf, hd_tst_clean, type = 'class'))

cv_5 = trainControl(method = "cv", number = 5)
knn_all = train(form = num ~ .,
                data = hd_trn_clean,
                method = 'rf',
                trControl = cv_5,
                tuneLength = 15
)
knn_all
accuracy(hd_tst_clean$num, predict(knn_all, hd_tst_clean))
