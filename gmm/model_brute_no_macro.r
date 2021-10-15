# Load dependencies.
library("plm")
library("miscTools")
library("stringr")
library("dplyr")


# Set working directory.
# setwd("/home/renatochaz/git/economic_policy_uncertainty")
setwd("/home/renato_ch/economic_policy_uncertainty")

source("global.r")

# Load data.
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

# Compute squared investment.
ds$sq_inv <- ds$inv * ds$inv

# Filter dataframe
if (full == "no") {
    ds <- subset(ds, get(dummy) == constraint)
    ## Removing firm-data with less than four sequential years of data.
    ds <- filter_seq(ds, ds$ano, 4)
}

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))

# Generate list of combinations of the model instruments
combs <- c(
    "0,9", "1,9", "2,9", "3,9", "1,1", "2,2"
)

n <- 4
l <- rep(list(combs), n)
combinations <- expand.grid(l)

# Initialize empty matrix
mat_combs <- matrix(, nrow = nrow(combinations), ncol = 0)

# Convert char combinations to numeric dataframe
for (i in seq_len(ncol(combinations))) {
    seq_combs <- as.character(combinations[, i])

    first_seq <- as.numeric(str_extract(seq_combs, "^\\d")) # Matchs first char
    second_seq <- as.numeric(str_extract(seq_combs, "\\d$")) # Matchs last char

    mat_combs <- cbind(mat_combs, first_seq, second_seq)
}

# Convert matrix to data frame
mat_combs <- data.frame(mat_combs)
colnames(mat_combs) <- c(seq_len(ncol(mat_combs)))

# Generate list of combinations of the model instruments
lags <- c(
    "0,0", "1,1", "2,2"
)

n <- 2
l <- rep(list(lags), n)
lag_combs <- expand.grid(l)

# Initialize empty matrix
mat_lags <- matrix(, nrow = nrow(lag_combs), ncol = 0)

# Convert char combinations to numeric dataframe
for (i in seq_len(ncol(lag_combs))) {
    seq_combs <- as.character(lag_combs[, i])

    first_seq <- as.numeric(str_extract(seq_combs, "^\\d")) # Matchs first char
    second_seq <- as.numeric(str_extract(seq_combs, "\\d$")) # Matchs last char

    mat_lags <- cbind(mat_lags, first_seq, second_seq)
}

# Convert matrix to data frame
mat_lags <- data.frame(mat_lags)
colnames(mat_lags) <- c(seq_len(ncol(mat_lags)))

# Dynamically repeat mat_lags
n <- nrow(mat_lags)
mat_lags <- mat_lags %>% slice(rep(1:n(), each = nrow(mat_combs)))

# Dinamically expand mat_combs
mat_combs <- mat_combs[rep(seq_len(nrow(mat_combs)), n), ]
rownames(mat_combs) <- NULL

# Bind dataframes together
mat_combs <- cbind(mat_lags, mat_combs)
colnames(mat_combs) <- seq_len(ncol(mat_combs))

# Set a df for computed results.
results_df <- data.frame(
    model = double(), inv = double(), invSQ = double(), epu = double(),
    fc = double(), divida = double(), cv = double(), tamanho = double(),
    sargan = double(), m2 = double()
)

# Set a vector with names for the df
results_names <- c(
    "model", "inv", "invSQ", "epu", "fc", "divida", "cv",
    "tamanho", "sargan", "m2"
)

# Set timer and start the dynamic automatized gmm estimation for best inv lags
ptm <- proc.time()
for (i in seq_len(nrow(mat_combs))) {
    model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
        ln_epu + fcl_normalizado + divida + cv + tamanho |
        stats:::lag(inv, mat_combs[i, 1]:mat_combs[i, 2]) +
            stats:::lag(sq_inv, mat_combs[i, 3]:mat_combs[i, 4]) +
            stats:::lag(fcl_normalizado, mat_combs[i, 5]:mat_combs[i, 6]) +
            stats:::lag(divida, mat_combs[i, 7]:mat_combs[i, 8]) +
            stats:::lag(cv, mat_combs[i, 9]:mat_combs[i, 10]) +
            stats:::lag(tamanho, mat_combs[i, 11]:mat_combs[i, 12]),
    data = ds,
    effect = "individual",
    model = "twosteps",
    collapse = TRUE,
    transformation = "ld",
    fsm = "full",
    )

    my_summary <- summary(model, robust = TRUE)
    pvalues <- my_summary[1]
    sargan <- my_summary[11]
    m2 <- my_summary[13]

    results_temp <- data.frame(
        i,
        unname(unlist(pvalues)[22]), unname(unlist(pvalues)[23]),
        unname(unlist(pvalues)[24]), unname(unlist(pvalues)[25]),
        unname(unlist(pvalues)[26]), unname(unlist(pvalues)[27]),
        unname(unlist(pvalues)[28]), as.numeric(unname(unlist(sargan)[2])),
        as.numeric(unname(unlist(m2)[2]))
    )
    colnames(results_temp) <- results_names

    results_df <- rbind(results_df, results_temp)
    print(paste0("Finished model: ", i, " of: ", nrow(mat_combs), " models."))
    print(paste0("Elapsed time: ", (proc.time() - ptm)[3]))
}

## Create dataset to analyze in graphical environment.
write.csv(results_df,
    paste0("gmm/csv_results/brute_no_macro", savename, ".csv"),
    row.names = FALSE
)