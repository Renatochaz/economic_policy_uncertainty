# Load dependencies.
library("stringr")

# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

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

## Create dataset to analyze in graphical environment.
write.csv(mat_combs,
    paste0("gmm/csv_results/inv_combs.csv"),
    row.names = FALSE
)

# model combs
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

## Create dataset to analyze in graphical environment.
write.csv(mat_combs,
    paste0("gmm/csv_results/model_combs.csv"),
    row.names = FALSE
)