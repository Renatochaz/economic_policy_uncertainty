# Load dependencies.
library("plm")
library("miscTools")
library("stringr")
library("dplyr")

# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

# Load data.
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

# Compute squared investment.
ds$sq_inv <- ds$inv * ds$inv

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))