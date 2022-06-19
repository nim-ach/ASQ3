# Load the necessary packages
library(ASQ3)
library(data.table)

# For tables
library(gtsummary)
library(gt)

# For GAMS
library(mgcv)

# Load the data
data("dataset", package = "ASQ3")

# Work only with a subset of data whom:
# 1. Includes only observations from assessment 1
# 2. Includes subjects with diagnosis of congenital hypotonia
# 3. Subjects whose age is not greater than 60 months old (chronological age)
dataset <- dataset[i = n_evaluacion == 1 & diagnostico %like% "hipotonia" & edad_cronologica_meses <= 60]

# Drop levels of unused factors
fv <- dataset[, names(.SD), .SDcols = is.factor]
dataset[, (fv) := lapply(.SD, droplevels), .SDcols = fv]
rm(fv)
