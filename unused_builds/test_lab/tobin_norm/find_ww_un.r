# Load dependencies.
library(dplyr)
library(stringr)
library(plm)

# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")
rm(list = ls())

# Pass configs
collapse <- "collapse"
collap <- TRUE
savename <- "ww_unconstrained"
dummy <- "dum_ww"
constraint <- "0"
subs <- "no"

# Load data.
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

# Compute squared investment.
ds$sq_inv <- ds$inv * ds$inv

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))

# Filter dataframe
if (savename != "full_model") {
    source("global.r")
    ds <- subset(ds, get(dummy) == constraint)
}

if (subs == "yes") {
    ## Removing firm-data with less than four sequential years of data.
    ds <- filter_seq(ds, ds$ano, 4)
}

for (files in seq_len(length(savename))) {
    # Load results.
    results <- read.csv(
        paste0(
            "test_lab/tobin_norm/results/",
             collapse, "/", savename[files], ".csv"
        ),
        stringsAsFactors = FALSE, fileEncoding = "UTF-8"
    )

    # Filter conditionally
    results <- results %>%
        filter(
            inv < 0.2 & invSQ < 0.2 &
                epu < 0.1 & fc < 0.1 &
                q_tobin < 0.2 &
                sargan > 0.1 & sargan < 0.9 & m2 > 0.1
        )
    print(paste0("Combinations: ", nrow(results)))
}

# iterate models
qvector <- vector()
count <- 1
comb <- vector()
for (i in seq_len(nrow(results))) {
    model_config <- c(
        as.integer(
            unlist(
                str_split(
                    results[i, 2], ","
                )
            )
        )
    )


    model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
        ln_epu + fcl_normalizado + divida + cv + tamanho + q_tobin |
        stats:::lag(inv, model_config[1]:model_config[2]) +
            stats:::lag(sq_inv, model_config[3]:model_config[4]) +
            stats:::lag(fcl_normalizado, model_config[5]:model_config[6]) +
            stats:::lag(divida, model_config[7]:model_config[8]) +
            stats:::lag(cv, model_config[9]:model_config[10]) +
            stats:::lag(tamanho, model_config[11]:model_config[12]) |
            stats:::lag(q_tobin, model_config[13]:model_config[14]),
    data = ds,
    effect = "individual",
    model = "twosteps",
    collapse = collap,
    transformation = "ld",
    fsm = "full",
    )

    comb <- c(comb, i)

    qvector <- c(
        qvector,
        unname(
            unlist(
                summary(model, robust = TRUE)[1]
            )
        )[3],
        unname(
            unlist(
                summary(model, robust = TRUE)[1]
            )
        )[4]
    )

    print(paste0(
        "Finished model: ", count
    ))
    count <- count + 1
}
a <- qvector[c(TRUE, FALSE)]
b <- qvector[c(FALSE, TRUE)]
mods <- data.frame(comb, a, b)

head(
    mods[order(
        mods$b,
        decreasing = FALSE
    ), ],
    n = 2000
)

model_config <- c(
    as.integer(
        unlist(
            str_split(
                results[4, 2], ","
            )
        )
    )
)

print(model_config)

model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho + q_tobin |
    stats:::lag(inv, model_config[1]:model_config[2]) +
        stats:::lag(sq_inv, model_config[3]:model_config[4]) +
        stats:::lag(fcl_normalizado, model_config[5]:model_config[6]) +
        stats:::lag(divida, model_config[7]:model_config[8]) +
        stats:::lag(cv, model_config[9]:model_config[10]) +
        stats:::lag(tamanho, model_config[11]:model_config[12]) |
        stats:::lag(q_tobin, model_config[13]:model_config[14]),
data = ds,
effect = "individual",
model = "twosteps",
collapse = collap,
transformation = "ld",
fsm = "full",
)

summary(model, robust=TRUE)
