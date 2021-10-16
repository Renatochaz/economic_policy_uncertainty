# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

# Load dependencies.
library("plm")
library("dplyr")

# Clean environment
rm(list = ls())


########### RUN #############
no_cols_lags_full <- c(3, 4, 0, 0)
no_cols_lags_kz_cons <- c(0, 1, 3, 3)
no_cols_lags_kz_uncons <- c(2, 2, 2, 2) # don't work
no_cols_lags_ww_cons <- c(0, 0, 2, 2)
no_cols_lags_ww_uncons <- c(2, 2, 2, 2)
no_cols_lags_sa_cons <- c(3, 3, 2, 2)
no_cols_lags_sa_uncons <- c(1, 1, 2, 2)
no_cols_list <- c(
    "no_cols_lags_full", "no_cols_lags_kz_cons",
    "no_cols_lags_kz_uncons", "no_cols_lags_ww_cons",
    "no_cols_lags_ww_uncons", "no_cols_lags_sa_cons", "no_cols_lags_sa_uncons"
)
# Get models
source("gmm/test_lab/find_models.r")
source("global.r")

# Configure conditions.
variables <- c(
    "inv_lag", "invsq_lag", "epu",
    "fcl", "divida", "cv", "tamanho",
    "sargan", "m2"
)

if (collapse == "collapse") {
    full <- "TRUE"
    kz_cons <- c("TRUE", "dum_kz", 1)
    kz_uncons <- c("TRUE", "dum_kz", 0)
    ww_cons <- c("TRUE", "dum_ww", 1)
    ww_uncons <- c("TRUE", "dum_ww", 0)
    sa_cons <- c("TRUE", "dum_sa", 1)
    sa_uncons <- c("TRUE", "dum_sa", 0)
    list_inf <- c(
        "full", "kz_cons", "kz_uncons",
        "ww_cons", "ww_uncons",
        "sa_cons", "sa_uncons"
    )
} else {
    full <- "FALSE"
    kz_cons <- c("FALSE", "dum_kz", 1)
    kz_uncons <- c("FALSE", "dum_kz", 0)
    ww_cons <- c("FALSE", "dum_ww", 1)
    ww_uncons <- c("FALSE", "dum_ww", 0)
    sa_cons <- c("FALSE", "dum_sa", 1)
    sa_uncons <- c("FALSE", "dum_sa", 0)
    list_inf <- c(
        "full", "kz_cons", "kz_uncons",
        "ww_cons", "ww_uncons",
        "sa_cons", "sa_uncons"
    )
}

for (i in seq_len(length(list_names))) {
    if (is.na(get(list_names[i])[1])) {
        list_names <- list_names[-c(i)]
    }
}

for (i in seq_len(length(list_names))) {
    if (is.na(get(list_names[i])[1])) {
        list_names <- list_names[-c(i)]
    }
}


for (i in list_names) {
    assign(i, c(get(i), get(list_inf[1])),
        envir = .GlobalEnv
    )
}

model_names <- list_names

n <- 10
results <- data.frame(
    index = character(n),
    coef = double(n),
    pvalue = double(n)
)

if (collapse == "collapse") {
    for (models in seq_len(length(model_names))) {
        model_name <- model_names[models]
        model_config <- get(model_names[models])
        # Load data.
        ds <- read.csv("global.csv",
            stringsAsFactors = FALSE, fileEncoding = "UTF-8"
        )

        # Compute squared investment.
        ds$sq_inv <- ds$inv * ds$inv

        # Set dataframe with definied panel indexes.
        ds <- pdata.frame(ds, index = c("codigo", "ano"))

        # Filter dataframe
        if (model_name != "model_full_no_macro") {
            ds <- subset(
                ds,
                get(model_config[14]) == as.numeric(model_config[15])
            )
            ## Removing firm-data with less than four sequential years of data.
            ds <- filter_seq(ds, ds$ano, 4)
        }

        model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
            ln_epu + fcl_normalizado + divida + cv + tamanho |
            stats:::lag(inv, model_config[1]:model_config[2]) +
                stats:::lag(sq_inv, model_config[3]:model_config[4]) +
                stats:::lag(fcl_normalizado, model_config[5]:model_config[6]) +
                stats:::lag(divida, model_config[7]:model_config[8]) +
                stats:::lag(cv, model_config[9]:model_config[10]) +
                stats:::lag(tamanho, model_config[11]:model_config[12]),
        data = ds,
        effect = "individual",
        model = "twosteps",
        collapse = as.logical(model_config[13]),
        transformation = "ld",
        fsm = "full",
        )

        mysum <- summary(model, robust = TRUE)
        coefs <- unlist(mysum["coefficients"])[1:7]
        pvalues <- unlist(mysum["coefficients"])[22:28]
        sargan <- as.numeric(unname(unlist(mysum[11])[2]))
        m2 <- as.numeric(unname(unlist(mysum[13])[2]))

        index <- c(model_name, variables)
        coef <- c(1, unname(coefs), sargan, m2)
        pvalue <- c(1, unname(pvalues), sargan, m2)
        temp_results <- data.frame(index, coef, pvalue)

        results <- bind_cols(results, temp_results)
    }
} else {
    for (models in seq_len(length(model_names))) {
        model_name <- model_names[models]
        model_config <- get(model_names[models])
        model_lags <- get(no_cols_list[models])
        # Load data.
        ds <- read.csv("global.csv",
            stringsAsFactors = FALSE, fileEncoding = "UTF-8"
        )

        # Compute squared investment.
        ds$sq_inv <- ds$inv * ds$inv

        # Set dataframe with definied panel indexes.
        ds <- pdata.frame(ds, index = c("codigo", "ano"))

        # Filter dataframe
        if (model_name != "model_full_no_macro") {
            ds <- subset(
                ds,
                get(model_config[10]) == as.numeric(model_config[11])
            )
            ## Removing firm-data with less than four sequential years of data.
            ds <- filter_seq(ds, ds$ano, 4)
        }

        model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
            ln_epu + fcl_normalizado + divida + cv + tamanho |
            stats:::lag(inv, model_lags[1]:model_lags[2]) +
                stats:::lag(sq_inv, model_lags[3]:model_lags[4]) +
                stats:::lag(fcl_normalizado, model_config[1]:model_config[2]) +
                stats:::lag(divida, model_config[3]:model_config[4]) +
                stats:::lag(cv, model_config[5]:model_config[6]) +
                stats:::lag(tamanho, model_config[7]:model_config[8]),
        data = ds,
        effect = "individual",
        model = "twosteps",
        collapse = as.logical(model_config[9]),
        transformation = "ld",
        fsm = "full",
        )

        mysum <- summary(model, robust = TRUE)
        coefs <- unlist(mysum["coefficients"])[1:7]
        pvalues <- unlist(mysum["coefficients"])[22:28]
        sargan <- as.numeric(unname(unlist(mysum[11])[2]))
        m2 <- as.numeric(unname(unlist(mysum[13])[2]))

        index <- c(model_name, variables)
        coef <- c(1, unname(coefs), sargan, m2)
        pvalue <- c(1, unname(pvalues), sargan, m2)
        temp_results <- data.frame(index, coef, pvalue)

        results <- bind_cols(results, temp_results)
    }
}

results <- results[, -c(1:3)]
View(results)