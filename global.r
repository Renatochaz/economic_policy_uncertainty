### Global functions ###

## Reload source.
reload <- function() {
  source("global.r")
}

## Returns a list with financial information from  /raw-data,
## with adjusted types, column names, rounded numbers and year index.
## basename: The name part of datasets that are equal to all files.
## diffname: A vector containing the name part of datasets.
load_adjusted_data <- function(diffname, basename = "eco") {

  ## Define columns to exclude.
  exclude_columns <- c(
    "Nome", "Pais.Sede", "Ativo...Cancelado",
    "Data.do.Bal..Mais.recente..consolid.sim.",
    "Bolsa...Fonte"
  )

  ## Define factor columns.
  factor_columns <- c(
    "Classe", "Setor.Economatica",
    "Setor.Econ.mico.Bovespa",
    "Setor.NAICS.ult.disponiv", "Subsetor.Bovespa",
    "Segmento.Bovespa", "Segmento.listagem.Bovespa"
  )

  ## Define numeric columns. (columns 17 to 33)
  numeric_columns <- seq(17, 33)

  ## Define name for all columns.
  col_names <- c(
    "ativo", "classe", "setor_economatica", "tipo_de_ativo",
    "setor_bovespa", "id_cvm", "codigo", "setor_naics", "subsetor_bovespa",
    "segmento_bovespa", "listagem_bovespa", "at", "k", "ll", "d_cp", "d_lp",
    "v", "depreciacao", "caixa", "dividendos", "qtd_acoes", "pl", "v_merc",
    "ebit", "desp_fin", "div_onerosa", "fcl", "cap_giro"
  )

  ## Define a list to store datasets
  ds_list <- list()

  for (i in seq_len(length(diffname))) {
    ### Read datasets
    ds <- read.csv(paste0("raw-data/", basename, diffname[i], ".csv"),
      stringsAsFactors = FALSE, fileEncoding = "UTF-8"
    )

    for (j in seq_len(length(numeric_columns))) {
      ### Clean "-" from data.
      ds[, numeric_columns[j]] <- gsub("-", "", ds[, numeric_columns[j]])
    }

    ### Adjust columns type.
    ds <- mutate_at(ds, c(numeric_columns), as.numeric)
    ds <- ds[, !names(ds) %in% exclude_columns]
    ds <- mutate_at(ds, all_of(factor_columns), as.factor)

    ## Set 0 as NA.
    ds[ds == 0] <- NA

    ## Adjust columns names.
    colnames(ds) <- col_names

    ## Round numeric values to two decimals.
    ds %>%
      dplyr::mutate(across(
        where(~ is.numeric(.)),
        list(~ round(., digits = 2))
      ))

    ## Define year index.
    ds$ano <- diffname[i]
    ds_list[[i]] <- ds
  }
  return(ds_list)
}

## Returns a dataset without NA and replaced NA to 0
## from selected columns.
## dataset = A dataframe object
## remove_na = vector of strings containing column names
## replace_na = vector of strings containing column names
clean_fin_data <- function(dataset, remove_na, replace_na) {
  dataset <- drop_na(dataset, all_of(remove_na))

  ## Replace NA to 0 from selected columns in replace_zero
  dataset <- dplyr::mutate(dataset, across(
    all_of(replace_na), ~ ifelse(is.na(.x), 0, .x)
  ))

  ## Index rows as ascending from 1
  rownames(dataset) <- 1:nrow(dataset)

  return(dataset)
}

## Function to get length of sequences of a given numeric vector.
## x = A numeric vector.
seqlength <- function(x) {
  if (!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + 1L
  i <- c(which(y | is.na(y)), n)

  list(
    lengths = diff(c(0L, i)),
    values = x[head(c(0L, i) + 1L, -1L)]
  )
}

## Returns a dataframe with the positions of sequences,
## given a minimum sequence value
## rle: A dataframe return from seqlength
## n: Minimum sequence length
seqpositions <- function(rle, n) {
  indices <- data.frame(rle$lengths, rle$values)
  indices$position <- 0
  indices$position[1] <- 1
  indices <- indices[, c(2, 1, 3)]

  for (i in 2:nrow(indices)) {
    indices$position[i] <- indices$position[i - 1] +
      indices$rle.lengths[i - 1]
  }

  ## Setting positions of small sequences ( < n)
  indices_small <- subset(indices, rle.lengths < n)

  return(indices_small)
}

## Return the index of rows to be dropped.
## x: Dataframe returned by seqpositions,
## containing rle. values, rle.lengths and positions
def_drop_index <- function(x) {
  vec_positions <- 0
  for (i in seq_len(nrow(x))) {
    vec_positions <- c(vec_positions, seq(
      from = x$position[i],
      to = x$position[i] + x$rle.lengths[i] - 1
    ))
  }
  vec_positions <- vec_positions[-c(1)]
  return(vec_positions)
}

## Filter rows with numeric sequences below a user-defined breakpoint.
## dataset: Dataset to be filtered
## seq_vector: A vector containing numeric sequences
## n: Minimum sequence value desired
filter_seq <- function(dataset, seq_vector, n) {
  rle <- seqlength(seq_vector)
  a <- seqpositions(rle, n)
  b <- def_drop_index(a)
  dataset <- dataset[-c(b), ]
  return(dataset)
}

## Create year-median economic policy uncertainty index, given a dataset.
create_pu <- function(x) {
  ## Read.
  pu <- read_excel("raw-data/Brazil_Policy_Uncertainty_Data.xlsx")

  ## Subset 2010 > data
  pu <- pu[229:348, ]

  ## Subset to a vector.
  pu <- pu$`Brazil News-Based EPU`

  ## Turn in to a list with data for a given year per index.
  t <- split(pu, ceiling(seq_along(pu) / 12))

  ## Compute median for the year.
  pu_medio_year <- 0

  for (i in 1:10) {
    pu_medio_year <- c(pu_medio_year, sum(unlist(t[i])) / 12)
  }
  pu_medio_year <- pu_medio_year[-c(1)]
  return(pu_medio_year)
}

## Return winsorized variables at 1%.
## ds: A dataset
## vars: Vector with names of variables to winsorize
winsorize_vars <- function(ds, vars) {
  temp <- ds[, vars]

  temp <- lapply(temp, Winsorize, probs = c(0.01, 0.99))

  return(temp)
}

## Return constructed variables
## with single operation (normal and defased)
## and normalizing dividend (defased).
## Set a global vector with positions to skip in the dataset
## due to the use of defased operators
## df: A dataset.
## var1, var2, var3: Variables to do the following operation:
## [var1 - var2 (i - 1) / var3 (i - 1)]
## operator: An mathematical operator.
cons_normdef <- function(df, var1, var2, var3, operator) {
  ## Set temporary vectors.
  vec_tmp <- 0
  vec_skips <- 0
  ## Iterate when year(i-1) == year and codigo(i-1) == codigo
  for (i in seq_len(nrow(df))) {
    if (length(which(((
      df[i, "ano"] - df[i - 1, "ano"]) == 1) &
      (df[i, "codigo"] == df[i - 1, "codigo"]))) != 0) {
      ## Add to vec_temp the result of the operation
      ## [var1 - var2 (i-1) / var3 (i -1)] # nolint
      vec_tmp <- c(
        vec_tmp,
        (getFunction(operator)((df[i, var1]),
          (df[i - 1, var2]))) / df[i - 1, var3]
      )
    } else {
      ## Add this observation to the skip list.
      vec_skips <- c(vec_skips, i)
    }
  }
  ## Remove first observation from vectors (0)
  vec_tmp <- vec_tmp[-c(1)]
  vec_skips <- vec_skips[-c(1)]
  ## Add vec_skips as a global var.
  vec_skips <<- vec_skips

  return(vec_tmp)
}

## Return constructed variables
## with single operation (normal and normal)
## and normalizing dividend (defased).
## Set a global vector with positions to skip in the dataset
## due to the use of defased operators
## df: A dataset.
## var1, var2, var3: Variables to do the following operation:
## [var1 - var2/ var3 (i - 1)]
## operator: An mathematical operator.
cons_norm <- function(df, var1, var2, var3, operator) {
  ## Set temporary vectors.
  vec_tmp <- 0
  vec_skips <- 0
  ## Iterate when year(i-1) == year and codigo(i-1) == codigo
  for (i in seq_len(nrow(df))) {
    if (length(which(((
      df[i, "ano"] - df[i - 1, "ano"]) == 1) &
      (df[i, "codigo"] == df[i - 1, "codigo"]))) != 0) {
      ## Add to vec_temp the result of the operation
      ## [var1 - var2 (i-1) / var3 (i -1)] # nolint
      vec_tmp <- c(
        vec_tmp,
        (getFunction(operator)((df[i, var1]),
          (df[i, var2]))) / df[i - 1, var3]
      )
    } else {
      ## Add this observation to the skip list.
      vec_skips <- c(vec_skips, i)
    }
  }
  ## Remove first observation from vectors (0)
  vec_tmp <- vec_tmp[-c(1)]
  vec_skips <- vec_skips[-c(1)]
  ## Add vec_skips as a global var.
  vec_skips <<- vec_skips

  return(vec_tmp)
}

## Return constructed variables
## with single variable
## and normalizing dividend (defased).
## Set a global vector with positions to skip in the dataset
## due to the use of defased operators
## df: A dataset.
## var1, var2: Variables to do the following operation:
## [var1/ var3 (i - 1)]
## operator: An mathematical operator.
cons_singlenorm <- function(df, var1, var2) {
  ## Set temporary vectors.
  vec_tmp <- 0
  vec_skips <- 0
  ## Iterate when year(i-1) == year and codigo(i-1) == codigo
  for (i in seq_len(nrow(df))) {
    if (length(which(((
      df[i, "ano"] - df[i - 1, "ano"]) == 1) &
      (df[i, "codigo"] == df[i - 1, "codigo"]))) != 0) {
      ## Add to vec_temp the result of the operation
      ## [var1/ var3 (i - 1)] # nolint
      vec_tmp <- c(vec_tmp, (df[i, var1] / df[i - 1, var2]))
    } else {
      ## Add this observation to the skip list.
      vec_skips <- c(vec_skips, i)
    }
  }
  ## Remove first observation from vectors (0)
  vec_tmp <- vec_tmp[-c(1)]
  vec_skips <- vec_skips[-c(1)]
  ## Add vec_skips as a global var.
  vec_skips <<- vec_skips
  return(vec_tmp)
}

## Returns: inv, fc, divida, cv, tamanho, alavanc
## caixa_normalizado, dividendos_normalizado, q_tobin,
## cob_juros, div_pl, roa, roe, rok
## df: A dataset.
cons_finvars <- function(df) {
  vec_inv <- cons_normdef(df, "at", "at", "at", "-")
  vec_cv <- cons_normdef(df, "v", "v", "v", "-")
  vec_fc <- cons_norm(df, "ll", "depreciacao", "at", "+")
  vec_divida <- cons_norm(df, "d_cp", "d_lp", "at", "+")
  vec_caixanorm <- cons_singlenorm(df, "caixa", "at")
  vec_divpagos_at <- cons_singlenorm(df, "dividendos", "at")
  vec_rok <- cons_singlenorm(df, "ll", "k")
  vec_fcl <- cons_singlenorm(df, "fcl", "at")
  vec_onerosa <- cons_singlenorm(df, "div_onerosa", "at")

  ## Subsetting skipped observations.
  df <- df[-c(vec_skips), ]
  rownames(df) <- 1:nrow(df)

  ## Adding constructed variables.
  {
    df$inv <- vec_inv
    df$fc <- vec_fc
    df$divida <- vec_divida
    df$cv <- vec_cv
    df$tamanho <- log(df$at)
    df$alavanc <- ((df$d_cp + df$d_lp) / (df$d_cp + df$d_lp + df$pl))
    df$caixa_normalizado <- vec_caixanorm
    df$dividendos_normalizado <- vec_divpagos_at
    df$q_tobin <- ((df$v_merc + df$d_cp + df$d_lp) / df$at)
    df$cob_juros <- (df$ebit / df$desp_fin)
    df$div_pl <- (df$dividendos / df$pl)
    df$roa <- (df$ll / df$at)
    df$roe <- (df$ll / df$pl)
    df$rok <- vec_rok
    df$fcl_normalizado <- vec_fcl
    df$divonerosa_normalizado <- vec_onerosa
  }

  return(df)
}

## Return a dataset with the ID CVM and the cvm register date.
## n: A int with the number of digits to cut from the ID CVM
get_datacvm <- function(n) {
  df_info <- get_info_companies(tempdir())
  df_info <- df_info[, c("CD_CVM", "DT_REG")]
  df_info$CD_CVM <- substr(df_info$CD_CVM, 1, n)
  return(df_info)
}

## Return cvm info that matches the dataset
## by their id_cvm.
## col_idcvm: The column with cvm ID's, in 4-digit format
match_idcvm <- function(col_idcvm) {
  ## Get list of cvm companies and their register dates
  info <- get_datacvm(4)
  info <- info[(info$CD_CVM %in% col_idcvm), ]
  colnames(info) <- c("id_cvm", "dt_reg")
  return(info)
}

## Return a vector with the KZ Index.
cons_kz <- function(df) {
  df$kz <- -(1.001909 * df$fc) +
    (0.2826389 * df$q_tobin) +
    (3.139193 * df$alavanc) -
    (39.3678 * df$dividendos_normalizado) -
    (1.314759 * df$caixa_normalizado)
  return(df$kz)
}

## Return a vector with the WW Index.
cons_ww <- function(df) {
  df$ww <- -(0.091 * df$fc) -
    (0.062 * df$dum_dividendos) +
    (0.021 * df$divlp_at) -
    (0.044 * df$tamanho) +
    (0.102 * df$cv_industria) -
    (0.035 * df$cv)
  return(df$ww)
}

## Return a vector with the SA Index.
cons_sa <- function(df) {
  df$sa <- (-0.737 * df$tamanho) +
    (0.043 * (df$tamanho * df$tamanho)) -
    (0.040 * df$idade_firma)
  return(df$sa)
}

## Return a vector with the FCP Index.
cons_fcp <- function(df) {
  df$fcp <- (-0.123 * df$tamanho) -
    (0.024 * df$cob_juros) -
    (4.404 * df$roa) -
    (1.716 * df$caixa_normalizado)
  return(df$fcp)
}

## Return a dummy vector, with 0 if var < 40% and 1 if var > 60%.
## df: A dataframe
## var: Name of the var to use as base for dummy generation.
cons_dummy <- function(df, var) {
  temp_var <- df[, var]
  newvar <- 0
  ## 40% quintile.
  a <- quantile(temp_var, probs = seq(0.2, 1, 0.2))[[2]]
  ## 60% quintile.
  b <- quantile(temp_var, probs = seq(0.2, 1, 0.2))[[3]]

  for (i in seq_len(length(temp_var))) {
    if (temp_var[i] < a) {
      newvar[i] <- 0
    } else if (temp_var[i] > b) {
      newvar[i] <- 1
    } else {
      next
    }
  }
  return(newvar)
}

## Generate descriptive statistics with stargazer.
## df = A dataset
## varlist = A list of column names to generate the statistics.
cons_descriptive <- function(df, varlist) {
  df_temp <- subset(df, select = c(varlist))
  stargazer(df_temp,
    median = TRUE, digits = 4,
    omit.summary.stat = c("p25", "p75", "min", "max")
  )
}

## Frequency table constructor.
## df: A dataframe.
## var: The variable to create the frequency table.
cons_freqtable <- function(df, var) {
  freq_table <- dplyr::count(df, !!sym(var))
  freq_table$perc <- plyr::round_any((
    freq_table$n / sum(freq_table$n)) * 100, 0.0001, f = ceiling)

  freq_table$accumulated_freq[1] <- freq_table$perc[1]

  for (i in 2:nrow(freq_table)) {
    freq_table$accumulated_freq[i] <- freq_table$accumulated_freq[i - 1] +
      freq_table$perc[i]
  }

  freq_table$perc <- round(freq_table$perc, digits = 2)
  freq_table$accumulated_freq <- round(
    freq_table$accumulated_freq,
    digits = 2
  )

  print(xtable(freq_table), type = "latex")
}

## Correlation table constructor.
## df: A dataframe.
## varlist: Vector of variable to compute correlations.
cons_corrtable <- function(df, varlist) {
  # Slice variables
  slice <- df[, varlist]
  fcorr <- round(cor(slice), 2)

  # logical matrix for correlation triangulation
  upper.tri(fcorr)

  # hide upper triangle and importe to latex
  upper <- fcorr
  upper[upper.tri(fcorr)] <- ""
  upper <- as.data.frame(upper)
  print(xtable(upper), type = "latex")
}