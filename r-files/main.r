## Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

## Dependencies.
library(dplyr)
library(tidyr)
source("r-files/global.r")

## Define the name which differentiate the datasets to load.
diff_name <- seq(2010, 2019)

## Load data.
list_ds <- load_adjusted_data(diff_name)

## Set data to panel format.
ds <- do.call(rbind, list_ds)
ds <- arrange(ds, ativo, ano)

## Define variables to be adjusted.
a_vars <- c(
    "at", "k", "ll", "d_cp", "d_lp", "v", "depreciacao",
    "caixa", "pl", "ebit", "desp_fin", "div_onerosa",
    "fcl", "cap_giro", "qtd_acoes", "v_merc"
)

b_vars <- c("dividendos")

## Clean financial data.
ds <- clean_fin_data(ds, a_vars, b_vars)

## Remove negative values from non negative variables.
ds <- ds[ds$dividendos >= 0, ]
ds <- ds[ds$pl >= 0, ]
ds <- ds[ds$v >= 0, ]

## Checking if there is any NA in the dataset.
sapply(ds, function(x) any(is.na(x)))

## Removing firm-data with less than five sequential years of data.
ds <- filter_seq(ds, ds$ano, 6)

## Checking sequential years from firms.
seqlength(ds$ano)
