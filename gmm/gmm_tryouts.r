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

# Generate list of combinations of the model instruments
# Only to find best investment lags
combs <- c(
    "0,0", "0,1", "0,2", "0,3", "0,4", "0,5", "0,6", "0,7", "0,8", "0,9",
    "1,1", "1,2", "1,3", "1,4", "1,5", "1,6", "1,7", "1,8", "1,9",
    "2,2", "2,3", "2,4", "2,5", "2,6", "2,7", "2,8", "2,9",
    "3,3", "3,4", "3,5", "3,6", "3,7", "3,8", "3,9"
)
combs_extra <- c(
    "0,0", "0,1", "0,2", "0,9",
    "1,1", "1,2", "1,9",
    "2,2", "2,9"
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
for (i in seq_len(20)) {
    model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
        ln_epu + fc + divida + cv + tamanho + setor_economatica |
        stats:::lag(inv, 2:7) +
            stats:::lag(sq_inv, 2:8) +
            stats:::lag(fc, mat_combs[i, 1]:mat_combs[i, 2]) +
            stats:::lag(divida, mat_combs[i, 3]:mat_combs[i, 4]) +
            stats:::lag(cv, mat_combs[i, 5]:mat_combs[i, 6]) +
            stats:::lag(tamanho, 2:9),
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
write.csv(results_df, "results/results_inv.csv", row.names = FALSE)

# Find index of desires outputs
# results_df$inv <= 0.05 & results_df$invSQ <= 0.05 &
# results_df$epu <= 0.05

# Use N model
n <- 770
model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fc + divida + cv + tamanho + setor_economatica |
    stats:::lag(inv, mat_combs[n, 1]:mat_combs[n, 2]) +
        stats:::lag(sq_inv, mat_combs[n, 3]:mat_combs[n, 4]) +
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

summary(model, robust = TRUE)

# print model
mat_combs[770, ]


# SAMPLES

# Initialize helpers to clean useless combinations
# vec_dels <- vector()
# count_cols <- 2
# Loop to clean combinations
# for (columns in seq_len(n)) {
#    if (columns == 2 | columns == 4 | columns == 8 | columns == 8) {
#        next
#    } else {
#        for (rows in seq_len(nrow(combinations))) {
#            if (combinations[rows, columns] == 1 &
#                combinations[rows, count_cols] == 0) {
#                vec_dels <- c(vec_dels, rows)
#            } else {
#                next
#            }
#        }
#        count_cols <- count_cols + 2
#        if (count_cols == 10) {
#            vec_dels <- as.numeric(vec_dels)
#            vec_dels <- unique(vec_dels)
#            combinations <- combinations[-c(vec_dels), ]
#            row.names(combinations) <- NULL
#            break
#        }
#    }
# }

model <- pgmm(inv ~ lag(inv) + lag(sq_inv) +
    ln_epu + fc + divida + cv + tamanho + setor_economatica |
    lag(inv, 2:2) + lag(sq_inv, 2:2) +
        fc, 0:0 + divida, 0:0 + lag(cv, 0:0) + tamanho, 2:9,
data = ds,
effect = "individual",
model = "twosteps",
collapse = FALSE,
transformation = "ld",
fsm = "full",
lost.ts = 2
)

model <- pgmm(inv ~ lag(inv) + lag(sq_inv) +
    ln_epu + fc + divida + cv + tamanho + setor_economatica |
    lag(inv, 0:8) + lag(sq_inv, 0:8) +
        fc, 1:9 + divida, 1:2 + lag(cv, 0:2) + tamanho, 2:2,
data = ds,
effect = "individual",
model = "twosteps",
collapse = FALSE,
transformation = "ld",
fsm = "full",
lost.ts = 2
)

summary(model, robust = TRUE)
unique(results_df)
mat_combs[6, 1:8]
mat_combs[600, 1:8]