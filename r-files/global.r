### Global functions ###

## Returns a list with financial information from  /raw-data,
## with adjusted types, column names, rounded numbers and year index.
## basename: The name part of datasets that are equal to all files.
## diffname: A vector containing the name part of datasets.
load_adjusted_data <- function(diffname, basename = "eco") {

    ## Define columns to exclude.
    exclude_columns <- c(
        "Nome", "Pais.Sede", "Ativo...Cancelado",
        "Data.do.Bal..Mais.recente..consolid.sim.",
        "Bolsa...Fonte", "ID.da.empresa"
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
        "setor_bovespa", "codigo", "setor_naics", "subsetor_bovespa",
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
            mutate(across(
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
    dataset <- mutate(dataset, across(
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