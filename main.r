## Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")

## Dependencies.
library(dplyr)
library(tidyr)
library(readxl)
library(DescTools)
library(GetDFPData2)
library(stargazer)
library(plyr)
library(xtable)
source("global.r")

## Define the name which differentiate the datasets to load.
diff_name <- seq(2010, 2019)

## Load data.
list_ds <- load_adjusted_data(diff_name)

## Set data to panel format.
ds <- do.call(rbind, list_ds)
ds <- arrange(ds, ativo, ano)

## Define variables to be adjusted.
a_vars <- c(
    "at", "k", "ll", "d_cp", "d_lp", "v", "depreciacao",
    "caixa", "pl", "ebit", "desp_fin", "div_onerosa",
    "fcl", "cap_giro", "qtd_acoes", "v_merc"
)

b_vars <- c("dividendos")

## Clean financial data.
ds <- clean_fin_data(ds, a_vars, b_vars)

## Remove negative values from non negative variables.
ds <- ds[ds$dividendos >= 0, ]
ds <- ds[ds$pl >= 0, ]
ds <- ds[ds$v >= 0, ]

## Checking if there is any NA in the dataset.
sapply(ds, function(x) any(is.na(x)))

## Removing firm-data with less than five sequential years of data.
ds <- filter_seq(ds, ds$ano, 5)

## Checking sequential years from firms.
seqlength(ds$ano)


## Remove redundant columns.
remove_cols <- c(
    "ativo", "classe", "tipo_de_ativo",
    "setor_naics", "subsetor_bovespa", "segmento_bovespa"
)
ds <- ds[, !(colnames(ds) %in% remove_cols)]
rownames(ds) <- 1:nrow(ds)

## Rename levels from: setor_economatica, setor_bovespa, listagem_bovespa.
ds$setor_economatica <- recode_factor(
    ds$setor_economatica,
    `Alimentos e Beb` = "Alimentos e Bebidas",
    `Com�rcio` = "Comercio",
    `Constru��o` = "Construcao",
    `Eletroeletr�nicos` = "Eletroeletronicos",
    `Energia El�trica` = "Energia Eletrica",
    `Minerais n�o Met` = "Minerais nao Metalicos",
    `Minera��o` = "Mineracao",
    `M�quinas Indust` = "Maquinas Industriais",
    `Petr�leo e Gas` = "Petroleo e Gas",
    `Qu�mica` = "Quimica",
    `Siderur & Metalur` = "Siderurgica e Metalurgica",
    `Telecomunica��es` = "Telecomunicacoes",
    `Transporte Servi�` = "Transporte e Servicoes",
    `Veiculos e pe�as` = "Veiculos e Pecas"
)

ds$setor_bovespa <- recode_factor(
    ds$setor_bovespa,
    `Comunica��es` = "Comunicacoes",
    `Consumo c�clico` = "Consumo Ciclico",
    `Consumo n�o c�clico` = "Consumo Nao Ciclico",
    `Materiais b�sicos` = "Materiais Basicos",
    `Petr�leo g�s e biocombust�veis` = "Petroleo, Gas e Biocombustiveis",
    `Sa�de` = "Saude",
    `Tecnologia da informa��o` = "Tecnologia da Informacao",
    `Utilidade p�blica` = "Utilidade Publica"
)

ds$listagem_bovespa <- recode_factor(
    ds$listagem_bovespa,
    `Balc�o Organizado` = "Balcao Organizado",
    `BDR n�vel 3` = "BDR Nivel 3",
    `Bovespa Mais N�vel 2` = "Bovespa Mais Nivel 2",
    `N�vel 1` = "Nivel 1",
    `N�vel 2` = "Nivel 2"
)

## Remove observations from Financial sector.
ds <- droplevels(ds[!ds$setor_bovespa == "Financeiro", ])
rownames(ds) <- 1:nrow(ds)

## Winsorize financial variables.
fin_vars <- c(
    "at", "k", "ll", "d_cp", "d_lp", "v",
    "depreciacao", "caixa", "dividendos",
    "qtd_acoes", "pl", "v_merc", "ebit",
    "desp_fin", "div_onerosa", "fcl",
    "cap_giro"
)
ds[, fin_vars] <- winsorize_vars(ds, fin_vars)

## Create dataset to python sales operation.
write.csv(ds, "raw-data/py_sales.csv", row.names = FALSE)

## Construct financial variables.
ds <- cons_finvars(ds)

## Removing firm-data with less than five sequential years of data.
ds <- filter_seq(ds, ds$ano, 4)

## Checking sequential years from firms.
seqlength(ds$ano)

## Remove first digit from id_cvm to match cvm data.
ds$id_cvm <- sub(".", "", ds$id_cvm)

## Get info on matching firms.
info <- match_idcvm(ds$id_cvm)

## Create EPU year-medians.
epu <- create_pu()

## save data to do a python operation.
write.csv(info, "raw-data/dt_reg_info.csv", row.names = FALSE)
write.csv(epu, "raw-data/yearly_epu.csv", row.names = FALSE)
write.csv(ds, "raw-data/py_create_age_epu.csv", row.names = FALSE)

## Save overall firm companies.
df_info <- get_info_companies(tempdir())
write.csv(df_info, "raw-data/info_companies.csv", row.names = FALSE)

## Run py-age-epu.ipynb and py-sales.ipynb
## Open returned datasets from Python.
ds <- read.csv("raw-data/ds_to_r.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)
sales <- read.csv("raw-data/sales_to_r.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

## Compute d_lp / at.
ds$divlp_at <- ds$d_lp / ds$at

## Compute dividendos dummy.
ds$dum_dividendos <- ifelse(ds$dividendos > 0, 1, 0)

## Compute industry sales growth.
vec_tmp <- 0
vec_skips <- 0
## Iterate when year(i-1) == year and codigo(i-1) == codigo.
for (i in seq_len(nrow(sales))) {
    if (length(which(((
        sales[i, "ano"] - sales[i - 1, "ano"]) == 1) &
        (sales[i, "setor_economatica"] ==
            sales[i - 1, "setor_economatica"]))) != 0) {
        ## Add to vec_temp the result of the operation.
        ## [var1 - var2 (i-1) / var3 (i -1)] # nolint
        vec_tmp <- c(
            vec_tmp,
            ((sales[i, "vsum"] - sales[i - 1, "vsum"]) / sales[i - 1, "vsum"])
        )
    } else {
        ## Add this observation to the skip list.
        vec_skips <- c(vec_skips, i)
    }
}
## Remove first observation from vectors (0).
vec_tmp <- vec_tmp[-c(1)]
vec_skips <- vec_skips[-c(1)]

## Subset skips and reset index.
sales <- sales[-c(vec_skips), ]
rownames(sales) <- 1:nrow(sales)

## Adding construct cv for industry.
sales$cv_industria <- vec_tmp


## Create dataset to python growth sales operation.
write.csv(sales, "raw-data/growth_sales.csv", row.names = FALSE)
write.csv(ds, "raw-data/py_growth_sales.csv", row.names = FALSE)

## Run py-growth-sales.ipynb
## Reload dataset after python operations.
ds <- read.csv("raw-data/ds_sales_to_r.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

## Winsorize constructed variables.
const_vars <- c(
    "inv", "fc", "divida", "cv", "tamanho", "alavanc",
    "caixa_normalizado", "dividendos_normalizado",
    "q_tobin", "cob_juros", "div_pl", "roa", "roe", "rok",
    "divlp_at", "cv_industria", "fcl_normalizado", "divonerosa_normalizado",
    "tx_vendas"
)

temp <- ds[, const_vars]
temp <- lapply(temp, Winsorize, probs = c(0.05, 0.95))
ds[, const_vars] <- temp

temp <- lapply(temp, Winsorize, probs = c(0.1, 0.9))
ds$divida <- temp$divida

## Compute ln economic policy uncertainty index.
ds$ln_epu <- log(ds$epu)

## Compute KZ, WW, SA and FCP index.
ds$kz <- cons_kz(ds)
ds$ww <- cons_ww(ds)
ds$sa <- cons_sa(ds)
ds$fcp <- cons_fcp(ds)

## Compute dummies with 3º quintile as cut point.
ds$dum_kz <- cons_dummy(ds, "kz")
ds$dum_ww <- cons_dummy(ds, "ww")
ds$dum_sa <- cons_dummy(ds, "sa")
ds$dum_fcp <- cons_dummy(ds, "fcp")

## Create dataset to macro-vars operation.
write.csv(ds, "raw-data/py_macro.csv", row.names = FALSE)


## Reload dataset after python macro-vars and pib operations.
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

## Generate var list to descriptive statistics.
finvar_list <- c(
    "epu", "inv", "fc", "fcl_normalizado", "divida", "cv", "caixa_normalizado",
    "dividendos_normalizado", "tamanho", "q_tobin",
    "cob_juros", "roa", "roe", "rok"
)


## Genera descriptive tables
cons_descriptive(ds, finvar_list)
cons_descriptive_small(subset(ds, dum_kz == 1), finvar_list)
cons_descriptive_small(subset(ds, dum_kz == 0), finvar_list)

cons_descriptive_small(subset(ds, dum_ww == 1), finvar_list)
cons_descriptive_small(subset(ds, dum_ww == 0), finvar_list)

cons_descriptive_small(subset(ds, dum_sa == 1), finvar_list)
cons_descriptive_small(subset(ds, dum_sa == 0), finvar_list)

## Generate industrial distribution
cons_freqtable(ds, "setor_economatica")

## Generate correlation table
cons_corrtable(ds, finvar_list)
