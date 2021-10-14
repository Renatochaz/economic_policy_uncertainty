# Configure conditions.
subset <- "no"
dummy <- "dum_kz"
constraint <- "1"
model_lags <- c(
    3, 4,
    2, 9,
    2, 2,
    1, 9,
    1, 9,
    1, 9
)

# Load dependencies.
library("plm")

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


if (subset == "yes") {
    ds <- subset(ds, get(dummy) == constraint)
}

model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, model_lags[1]:model_lags[2]) +
        stats:::lag(sq_inv, model_lags[3]:model_lags[4]) +
        stats:::lag(fcl_normalizado, model_lags[5]:model_lags[6]) +
        stats:::lag(divida, model_lags[7]:model_lags[8]) +
        stats:::lag(cv, model_lags[9]:model_lags[10]) +
        stats:::lag(tamanho, model_lags[11]:model_lags[12]),
data = ds,
effect = "individual",
model = "twosteps",
collapse = FALSE,
transformation = "ld",
fsm = "full",
)

print(summary(model, robust = TRUE))