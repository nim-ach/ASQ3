# Cargamos paquetes

## Paquete de datos
library(ASQ3)

## Modelado con GAMs
library(mgcv)

## Manipulación general
library(data.table)

## Tablas
library(kableExtra)
library(gt)
library(gtsummary)

## Graficos
library(ggplot2)
library(ggpubr)

# Estilo por defecto para gráficos
theme_set(theme_classic())

source(here::here("manuscript/CICYPE/functions.R"))

# Preparamos los datos
dataset <- dataset[i = n_evaluacion == 1 & diagnostico %like% "hipotonia" & edad_cronologica_meses <= 60]

# Drop levels of unused factors
fv <- dataset[, names(.SD), .SDcols = is.factor]
dataset[, (fv) := lapply(.SD, droplevels), .SDcols = fv]
rm(fv)
