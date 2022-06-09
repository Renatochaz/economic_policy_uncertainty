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

ds <- subset(ds, get(dummy) == constraint)

# Filter dataframe
if (sub == "yes") {
    ds <- filter_seq(ds, ds$ano, 4)
}

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))

# Generate list of combinations of the model instruments
combs <- c(
    "0,0", "1, 1", "2, 2",
    "0, 9", "1, 9", "2, 9"
)

n <- 6
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
    model = double(), lags = character(), inv = double(), invSQ = double(),
    epu = double(), fc = double(), divida = double(), cv = double(),
    tamanho = double(), q_tobin = double(), iee = double(),
     sargan = double(), m2 = double()
)

# Set a vector with names for the df
results_names <- c(
    "model", "lags", "inv", "invSQ", "epu", "fc", "divida", "cv",
    "tamanho", "q_tobin", "iee", "sargan", "m2"
)

# Compute
skip_to_next <- FALSE
for (i in seq_len(nrow(mat_combs))) {
    model <- tryCatch(
        pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
            ln_epu + fcl_normalizado + divida + cv + tamanho + q_tobin + iee |
            stats:::lag(inv, mod[1]:mod[2]) +
                stats:::lag(sq_inv, mod[3]:mod[4]) +
                stats:::lag(fcl_normalizado, mat_combs[i, 1]:mat_combs[i, 2]) +
                stats:::lag(divida, mat_combs[i, 3]:mat_combs[i, 4]) +
                stats:::lag(cv, mat_combs[i, 5]:mat_combs[i, 6]) +
                stats:::lag(tamanho, mat_combs[i, 7]:mat_combs[i, 8]) |
                stats:::lag(q_tobin, mat_combs[i, 9]:mat_combs[i, 10]) +
                stats:::lag(iee, mat_combs[i, 11]:mat_combs[i, 12]),
        data = ds,
        effect = "individual",
        model = "twosteps",
        collapse = collap,
        transformation = "ld",
        fsm = "full",
        ),
        error = function(e) {
            print("error", e)
            skip_to_next <<- TRUE
        }
    )

    if (skip_to_next == TRUE) {
        skip_to_next <<- FALSE
        next
    } else {
        my_summary <- summary(model, robust = TRUE)
        pvalues <- my_summary[1]
        sargan <- my_summary[11]
        m2 <- my_summary[13]
        a <- paste0(c(
            mat_combs[i, 1], mat_combs[i, 2], mat_combs[i, 3], mat_combs[i, 4],
            mat_combs[i, 5], mat_combs[i, 6], mat_combs[i, 7], mat_combs[i, 8],
            mat_combs[i, 9], mat_combs[i, 10], mat_combs[i, 11],
            mat_combs[i, 12]
        ), collapse = ",")

        results_temp <- data.frame(
            i, a,
            unname(unlist(pvalues)[28]), unname(unlist(pvalues)[29]),
            unname(unlist(pvalues)[30]), unname(unlist(pvalues)[31]),
            unname(unlist(pvalues)[32]), unname(unlist(pvalues)[33]),
            unname(unlist(pvalues)[34]), unname(unlist(pvalues)[35]),
            unname(unlist(pvalues)[36]),
            as.numeric(unname(unlist(sargan)[2])),
            as.numeric(unname(unlist(m2)[2]))
        )
        colnames(results_temp) <- results_names

        results_df <- rbind(results_df, results_temp)
        print(paste0
        ("Finished model: ", i, " of: ", nrow(mat_combs), " models."))
    }
}

## Create dataset to analyze in graphical environment.
write.csv(results_df,
    paste0("test_lab/iee/results/", collapsave, "/", savename, ".csv"),
    row.names = FALSE
)
