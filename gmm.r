# Load dependencies.
library("plm")
library("miscTools")

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
n <- 8
l <- rep(list(0:2), n)
combinations <- expand.grid(l)

# Initialize helpers to clean useless combinations
vec_dels <- vector()
count_cols <- 2
# Loop to clean combinations
for (columns in seq_len(n)) {
    if (columns == 2 | columns == 4 | columns == 8 | columns == 8) {
        next
    } else {
        for (rows in seq_len(nrow(combinations))) {
            if (combinations[rows, columns] == 1 &
                combinations[rows, count_cols] == 0) {
                vec_dels <- c(vec_dels, rows)
            } else {
                next
            }
        }
        count_cols <- count_cols + 2
        if (count_cols == 10) {
            vec_dels <- as.numeric(vec_dels)
            vec_dels <- unique(vec_dels)
            combinations <- combinations[-c(vec_dels), ]
            row.names(combinations) <- NULL
            break
        }
    }
}

# Set a df for computed results.
results_df <- data.frame(
    inv = double(), invSQ = double(), epu = double(), fc = double(),
    divida = double(), cv = double(), tamanho = double(), sargan = double(),
    m2 = double()
)

# Set a vector with names for the df
results_names <- c(
    "inv", "invSQ", "epu", "fc", "divida", "cv",
    "tamanho", "sargan", "m2"
)

model <- pgmm(inv ~ lag(inv) + lag(sq_inv) +
    ln_epu + fc + divida + cv + tamanho + setor_economatica |
    lag(inv, combinations[1, 1]:combinations[1, 2]) +
        lag(sq_inv, combinations[1, 1]:combinations[1, 2]) +
        fc, combinations[1, 3]:combinations[1, 4] +
    divida, combinations[1, 5]:combinations[1, 6] +
    lag(cv, combinations[1, 7]:combinations[1, 8]) + tamanho, 2:9,
data = ds,
effect = "individual",
model = "twosteps",
collapse = FALSE,
transformation = "ld",
fsm = "full",
lost.ts = 2
)

my_summary <- summary(model, robust = TRUE)
pvalues <- my_summary[1]
sargan <- my_summary[11]
m2 <- my_summary[13]

results_temp <- data.frame(
    unname(unlist(pvalues)[79]), unname(unlist(pvalues)[80]),
    unname(unlist(pvalues)[81]), unname(unlist(pvalues)[82]),
    unname(unlist(pvalues)[83]), unname(unlist(pvalues)[84]),
    unname(unlist(pvalues)[85]), as.numeric(unname(unlist(sargan)[2])),
    as.numeric(unname(unlist(m2)[2]))
)
colnames(results_temp) <- results_names

results_df <- rbind(results_df, results_temp)