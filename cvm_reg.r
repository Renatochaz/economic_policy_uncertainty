library(GetDFPData2)

## Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

df_info <- get_info_companies(tempdir())
df_info <- df_info[, c(
    "CD_CVM", "DENOM_SOCIAL", "DENOM_COMERC", "DT_INI_CATEG")]
df_info$CD_CVM <- substr(df_info$CD_CVM, 1, 4)


# Dataset
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

info <- df_info
info <- info[(info$CD_CVM %in% ds$id_cvm), ]

## Create adjusted dataset.
write.csv(df_info, "raw-data/full_cvm.csv", row.names = FALSE)
write.csv(info, "raw-data/cvm_age_2.csv", row.names = FALSE)
