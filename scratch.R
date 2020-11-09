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


# function to determine proportion of NAs in a vector
na_prop = function(x) {
  mean(is.na(x))
}

sapply(hd_trn, na_prop)

# create dataset without columns containing more than 30% NAs
hd_trn = na.omit(hd_trn[, !sapply(hd_trn, na_prop) > 0.3])


# coerce char variables to be factors
hd_trn$num = factor(hd_trn$num)
hd_trn$location = factor(hd_trn$location)

## Change cp into factors
hd_trn$cp[hd_trn$cp == 1] = 'typical angina'
hd_trn$cp[hd_trn$cp == 2] = 'atypical angina'
hd_trn$cp[hd_trn$cp == 3] = 'non-anginal pain'
hd_trn$cp[hd_trn$cp == 4] = 'asymptomatic'
hd_trn$cp = factor(hd_trn$cp)

## Change Sex into factors
hd_trn$sex[hd_trn$sex == 1] = 'male'
hd_trn$sex[hd_trn$sex == 0] = 'female'
hd_trn$sex = factor(hd_trn$sex)

## Change FBS into factors
hd_trn$fbs[hd_trn$fbs == 1] = '> 120'
hd_trn$fbs[hd_trn$fbs == 0] = '< 120'
hd_trn$fbs = factor(hd_trn$fbs)

## Change restecg into factors
hd_trn$restecg[hd_trn$restecg == 0] = 'normal'
hd_trn$restecg[hd_trn$restecg == 1] = 'ST-T Wave'
hd_trn$restecg[hd_trn$restecg == 2] = 'left hypertrophy'
hd_trn$restecg = factor(hd_trn$restecg)

## Change exang into factors
hd_trn$exang[hd_trn$exang == 0] = 'yes'
hd_trn$exang[hd_trn$exang == 1] = 'no'
hd_trn$exang = factor(hd_trn$exang)
hd_trn
