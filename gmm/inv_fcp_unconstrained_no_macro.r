# Configure conditions.
savename <- "inv_fcp_unconstrainend_no_macro"
dummy <- "dum_fcp"
constraint <- "0"

# Load dependencies.
library("plm")
library("miscTools")
library("stringr")
library("dplyr")

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
    "0,0", "0,1", "0,2", "0,3", "0,4", "0,5", "0,6", "0,7", "0,8", "0,9",
    "1,1", "1,2", "1,3", "1,4", "1,5", "1,6", "1,7", "1,8", "1,9",
    "2,2", "2,3", "2,4", "2,5", "2,6", "2,7", "2,8", "2,9",
    "3,3", "3,4", "3,5", "3,6", "3,7", "3,8", "3,9"
)

n <- 2
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
        ln_epu + fc + divida + cv + tamanho |
        stats:::lag(inv, mat_combs[i, 1]:mat_combs[i, 2]) +
            stats:::lag(sq_inv, 2:2) +
            stats:::lag(fc, 2:2) +
            stats:::lag(divida, 2:2) +
            stats:::lag(cv, 2:2) +
            stats:::lag(tamanho, 2:2),
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