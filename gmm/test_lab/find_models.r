# Load dependencies.
library(dplyr)

# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

# Pass configs
rm(list = ls())
source("global.r")
collapse <- "collapse"
obs <- 1

filename <- c(
    # "model_full_no_macro"
    "kz_constrained_no_macro", "kz_unconstrained_no_macro"
    # "ww_constrained_no_macro", "ww_unconstrained_no_macro"
    # "sa_constrained_no_macro", "sa_unconstrained_no_macro"
)

select_filename <- c()

for (files in seq_len(length(filename))) {
    # Load results.
    results <- read.csv(
        paste0("gmm/test_lab/results/", collapse, "/", filename[files], ".csv"),
        stringsAsFactors = FALSE, fileEncoding = "UTF-8"
    )

    # Load combs.
    if (collapse == "collapse") {
        combs <- read.csv(
            "gmm/test_lab/combs/brute_combs.csv",
            stringsAsFactors = FALSE, fileEncoding = "UTF-8"
        )
    } else {
        combs <- read.csv(
            "gmm/test_lab/combs/model_combs.csv",
            stringsAsFactors = FALSE, fileEncoding = "UTF-8"
        )
    }

    # Filter conditionally
    results <- results %>%
        filter(inv < 0.1 & invSQ < 0.1 &
            fc < 0.1 & epu < 0.1 & sargan > 0.1 & sargan < 0.8)

    # Filter combs
    model_number <- results[, "model"]
    combs <- combs[model_number, ]
    write.csv(combs,
        paste0(
            "gmm/test_lab/sampled_combs/",
            collapse, "/", filename[files], ".csv"
        ),
        row.names = TRUE
    )

    assign(paste0(collapse, "_", filename[files]), results, envir = .GlobalEnv)
    select_filename <- c(
        select_filename,
        (paste0(collapse, "_", filename[files]))
    )
}

## Select observation number to return models

index <- c()
for (files in seq_len(length(select_filename))) {
    print(paste0(
        select_filename[files], ": ",
        get(paste0(select_filename[files]))[obs, "model"]
    ))
    index <- c(index, get(paste0(select_filename[files]))[obs, "model"])
}

list_names <- c()
# return model
for (files in seq_len(length(select_filename))) {
    dfcombs <- read.csv(
        paste0(
            "gmm/test_lab/sampled_combs/",
            collapse, "/", filename[files], ".csv"
        ),
        stringsAsFactors = FALSE, fileEncoding = "UTF-8"
    )

    b <- ncol(dfcombs)
    a <- as.numeric(dfcombs[dfcombs$X == index[files], 2:b])
    assign(filename[files], a, envir = .GlobalEnv)
    list_names <- c(list_names, filename[files])
}