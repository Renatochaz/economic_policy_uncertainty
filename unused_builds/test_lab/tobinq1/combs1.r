library("dplyr")
library("miscTools")
library("stringr")

# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

# Generate list of combinations of the model instruments
lags <- c(
    "0, 9",
    "1, 9",
    "2, 9"
)

combs <- c(
    "0,0", "0, 9",
    "1, 1", "1, 9",
    "2, 2", "2, 9",
    "3, 9", "3, 3"
)

n <- 5
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

## Create dataset to analyze in graphical environment.
write.csv(mat_combs,
    paste0("test_lab/tobinq1/combs/combs1.csv"),
    row.names = FALSE
)