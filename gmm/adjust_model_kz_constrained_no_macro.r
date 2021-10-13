# Configure conditions.
savename <- "inv_kz_constrainend_no_macro"
dummy <- "dum_kz"
constraint <- "1"

# Load dependencies.
library("plm")
library("miscTools")
library("stringr")
library("dplyr")

get(dummy)
# Set working directory.
# setwd("/home/renatochaz/git/economic_policy_uncertainty")
setwd("/home/renato_ch/economic_policy_uncertainty")

# Load data.
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

# Compute squared investment.
ds$sq_inv <- ds$inv * ds$inv

# Filter dataframe
ds <- subset(ds, get(dummy) == constraint)

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))

# Generate list of combinations of the model instruments
# Only to find best investment lags
combs <- c(
    "0,0", "0,1", "0,2", "0,9",
    "1,1", "1,2", "1,9",
    "2,2", "2,9"
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
        ln_epu + fc + divida + cv + tamanho + setor_economatica |
        stats:::lag(inv, 3:6) +
            stats:::lag(sq_inv, 2:4) +
            stats:::lag(fc, mat_combs[i, 1]:mat_combs[i, 2]) +
            stats:::lag(divida, mat_combs[i, 3]:mat_combs[i, 4]) +
            stats:::lag(cv, mat_combs[i, 5]:mat_combs[i, 6]) +
            stats:::lag(tamanho, mat_combs[i, 7]:mat_combs[i, 8]),
    data = ds,
    effect = "individual",
    model = "twosteps",
    collapse = FALSE,
    transformation = "ld",
    fsm = "full",
    )

    my_summary <- summary(model, robust = TRUE)
    pvalues <- my_summary[1]
    sargan <- my_summary[11]
    m2 <- my_summary[13]

    results_temp <- data.frame(
        i,
        unname(unlist(pvalues)[79]), unname(unlist(pvalues)[80]),
        unname(unlist(pvalues)[81]), unname(unlist(pvalues)[82]),
        unname(unlist(pvalues)[83]), unname(unlist(pvalues)[84]),
        unname(unlist(pvalues)[85]), as.numeric(unname(unlist(sargan)[2])),
        as.numeric(unname(unlist(m2)[2]))
    )
    colnames(results_temp) <- results_names

    results_df <- rbind(results_df, results_temp)
    print(paste0("Finished model: ", i, " of: ", nrow(mat_combs), " models."))
    print(paste0("Elapsed time: ", (proc.time() - ptm)[3]))
}

## Create dataset to analyze in graphical environment.
write.csv(results_df,
    paste0("gmm/csv_results/", savename, ".csv"),
    row.names = FALSE
)