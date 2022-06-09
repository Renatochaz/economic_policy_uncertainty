# Load dependencies.
library("plm")
library("miscTools")
library("stringr")
library("dplyr")


# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

rm(list=ls())

collap <- FALSE

mat_combs <- data.frame(t(c(1,1,0,0,2,9,1,9,2,9,0,9,1,1)))

# Load data.
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

# Compute squared investment.
ds$sq_inv <- ds$inv * ds$inv

# Compute IIE.
ds$ln_iie <- log(ds$iee)

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))

# Generate list of combinations of the model instruments
combs <- c(
    "0,0", "0,1", "0,2", "0,3", "0,4", "0,5", "0,6", "0,7", "0,8", "0,9",
    "1,1", "1,2", "1,3", "1,4", "1,5", "1,6", "1,7", "1,8", "1,9",
    "2,2", "2,3", "2,4", "2,5", "2,6", "2,7", "2,8", "2,9",
    "3,3", "3,4", "3,5,", "3,6", "3,7", "3,8", "3,9",
    "4,4,", "4,5", "4,6", "4,7", "4,8", "4,9",
    "5,5", "5,6", "5,7", "5,8", "5,9",
    "6,6", "6,7", "6,8", "6,9",
    "7,7", "7,8", "7,9",
    "8,8", "8,9",
    "9,9"
)

n <- 1
l <- rep(list(combs), n)
combinations <- expand.grid(l)

# Initialize empty matrix
combs <- matrix(, nrow = nrow(combinations), ncol = 0)

# Convert char combinations to numeric dataframe
for (i in seq_len(ncol(combinations))) {
    seq_combs <- as.character(combinations[, i])

    first_seq <- as.numeric(str_extract(seq_combs, "^\\d")) # Matchs first char
    second_seq <- as.numeric(str_extract(seq_combs, "\\d$")) # Matchs last char

    combs <- cbind(combs, first_seq, second_seq)
}

# Convert matrix to data frame
combs <- data.frame(combs)
colnames(combs) <- c(seq_len(ncol(combs)))


# Set a df for computed results.
results_df <- data.frame(
    model = double(), lags = character(), inv = double(), invSQ = double(),
    epu = double(), fc = double(), divida = double(), cv = double(),
    tamanho = double(), q_tobin = double(), fbcf = double(), iie = double(),
    sargan = double(), m2 = double()
)

# Set a vector with names for the df
results_names <- c(
    "model", "lags", "inv", "invSQ", "epu", "fc", "divida", "cv",
    "tamanho", "q_tobin","fbcf", "iie", "sargan", "m2"
)

# Compute
skip_to_next <- FALSE
for (i in seq_len(nrow(combs))) {
    model <- tryCatch(
        pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
            ln_epu + fcl_normalizado + divida + cv + tamanho + q_tobin + fbcf + iee|
            stats:::lag(inv, mat_combs[1, 1]:mat_combs[1, 2]) +
                stats:::lag(sq_inv, mat_combs[1, 3]:mat_combs[1, 4]) +
                stats:::lag(fcl_normalizado, mat_combs[1, 5]:mat_combs[1, 6]) +
                stats:::lag(divida, mat_combs[1, 7]:mat_combs[1, 8]) +
                stats:::lag(cv, mat_combs[1, 9]:mat_combs[1, 10]) +
                stats:::lag(tamanho, mat_combs[1, 11]:mat_combs[1, 12]) |
                stats:::lag(q_tobin, mat_combs[1, 13]:mat_combs[1, 14]) +
                stats:::lag(iee, combs[i, 1]:combs[i, 2]),
        data = ds,
        effect = "individual",
        model = "twosteps",
        collapse = collap,
        transformation = "ld",
        fsm = "full",
        ),
        error = function(e) {
            print("error")
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
            mat_combs[1, 1], mat_combs[1, 2], mat_combs[1, 3], mat_combs[1, 4],
            mat_combs[1, 5], mat_combs[1, 6], mat_combs[1, 7], mat_combs[1, 8],
            mat_combs[1, 9], mat_combs[1, 10], mat_combs[1, 11],
            mat_combs[1, 12], mat_combs[1, 13], mat_combs[1, 14],
            combs[i, 1], combs[i, 2]
        ), collapse = ",")

        results_temp <- data.frame(
            i, a,
            unname(unlist(pvalues)[31]), unname(unlist(pvalues)[32]),
            unname(unlist(pvalues)[33]), unname(unlist(pvalues)[34]),
            unname(unlist(pvalues)[35]), unname(unlist(pvalues)[36]),
            unname(unlist(pvalues)[37]), unname(unlist(pvalues)[38]),
            unname(unlist(pvalues)[39]), unname(unlist(pvalues)[40]),
            as.numeric(unname(unlist(sargan)[2])),
            as.numeric(unname(unlist(m2)[2]))
        )
        colnames(results_temp) <- results_names

        results_df <- rbind(results_df, results_temp)
        print(paste0
        ("Finished model: ", i, " of: ", nrow(combs), " models."))
    }
}

 # Filter conditionally
results <- results_df %>%
     filter(
         iie < 0.1 & epu < 0.1 & fc < 0.05
         & inv < 0.05 & invSQ < 0.05 & q_tobin < 0.05 &
         sargan > 0.1 & sargan < 0.9 & m2 > 0.1
     )
print(paste0("Combinations: ", nrow(results)))

model <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
            ln_epu + fcl_normalizado + divida + cv + tamanho + q_tobin + fbcf + iee|
            stats:::lag(inv, mat_combs[1, 1]:mat_combs[1, 2]) +
                stats:::lag(sq_inv, mat_combs[1, 3]:mat_combs[1, 4]) +
                stats:::lag(fcl_normalizado, mat_combs[1, 5]:mat_combs[1, 6]) +
                stats:::lag(divida, mat_combs[1, 7]:mat_combs[1, 8]) +
                stats:::lag(cv, mat_combs[1, 9]:mat_combs[1, 10]) +
                stats:::lag(tamanho, mat_combs[1, 11]:mat_combs[1, 12]) |
                stats:::lag(q_tobin, mat_combs[1, 13]:mat_combs[1, 14]) +
                stats:::lag(iee, 4:5),
        data = ds,
        effect = "individual",
        model = "twosteps",
        collapse = collap,
        transformation = "ld",
        fsm = "full",
        )

summary(model, robust = TRUE)