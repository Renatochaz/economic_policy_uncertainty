# Load dependencies.
library("plm")
library("miscTools")
library("stringr")
library("dplyr")
library("texreg")
rm(list = ls())

# Set working directory.
setwd("/home/renatochaz/git/economic_policy_uncertainty")
source("global.r")

# Load data.
ds <- read.csv("global.csv",
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
)

# Compute squared investment.
ds$sq_inv <- ds$inv * ds$inv

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))

# Set goef vectors
obs <- vector()
arbond <- vector()
sargan <- vector()

# Full model.
m1 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, 3:4) +
        stats:::lag(sq_inv, 0:0) +
        stats:::lag(fcl_normalizado, 0:9) +
        stats:::lag(divida, 2:9) +
        stats:::lag(cv, 0:0) +
        stats:::lag(tamanho, 0:0),
data = ds,
effect = "individual",
model = "twosteps",
collapse = FALSE,
transformation = "ld",
fsm = "full",
)

tex_m1 <- extract(m1,
    robust = TRUE,
    include.wald = FALSE,
    include.nobs = FALSE,
    include.sargan = FALSE
)

mysum <- summary(m1, robust = TRUE)

obs <- c(obs, nrow(ds))

sargan <- c(
    sargan,
    round(
        as.numeric(unlist(mysum[11])[2]), 3
    )
)

arbond <- c(
    arbond,
    round(
        as.numeric(unlist(mysum[13])[2]), 3
    )
)


# KZ constrained.
kz_con <- subset(ds, dum_kz == 1)
kz_con <- filter_seq(kz_con, kz_con$ano, 4)
m2 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, 2:9) +
        stats:::lag(sq_inv, 0:0) +
        stats:::lag(fcl_normalizado, 1:1) +
        stats:::lag(divida, 1:1) +
        stats:::lag(cv, 0:9) +
        stats:::lag(tamanho, 2:9),
data = kz_con,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)

tex_m2 <- extract(m2,
    robust = TRUE,
    include.wald = FALSE,
    include.nobs = FALSE,
    include.sargan = FALSE
)

mysum <- summary(m2, robust = TRUE)

obs <- c(obs, nrow(kz_con))

sargan <- c(
    sargan,
    round(
        as.numeric(unlist(mysum[11])[2]), 3
    )
)

arbond <- c(
    arbond,
    round(
        as.numeric(unlist(mysum[13])[2]), 3
    )
)

# KZ unconstrained.
kz_un <- subset(ds, dum_kz == 0)
kz_un <- filter_seq(kz_un, kz_un$ano, 4)
m3 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, 0:9) +
        stats:::lag(sq_inv, 1:1) +
        stats:::lag(fcl_normalizado, 3:9) +
        stats:::lag(divida, 3:9) +
        stats:::lag(cv, 0:0) +
        stats:::lag(tamanho, 0:9),
data = kz_un,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)

tex_m3 <- extract(m3,
    robust = TRUE,
    include.wald = FALSE,
    include.nobs = FALSE,
    include.sargan = FALSE
)

mysum <- summary(m3, robust = TRUE)

obs <- c(obs, nrow(kz_un))

sargan <- c(
    sargan,
    round(
        as.numeric(unlist(mysum[11])[2]), 3
    )
)

arbond <- c(
    arbond,
    round(
        as.numeric(unlist(mysum[13])[2]), 3
    )
)

# WW constrained.
ww_con <- subset(ds, dum_ww == 1)
ww_con <- filter_seq(ww_con, ww_con$ano, 4)
m4 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, 2:9) +
        stats:::lag(sq_inv, 0:9) +
        stats:::lag(fcl_normalizado, 2:9) +
        stats:::lag(divida, 0:9) +
        stats:::lag(cv, 1:9) +
        stats:::lag(tamanho, 0:9),
data = ww_con,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)

tex_m4 <- extract(m4,
    robust = TRUE,
    include.wald = FALSE,
    include.nobs = FALSE,
    include.sargan = FALSE
)

mysum <- summary(m4, robust = TRUE)

obs <- c(obs, nrow(ww_con))

sargan <- c(
    sargan,
    round(
        as.numeric(unlist(mysum[11])[2]), 3
    )
)

arbond <- c(
    arbond,
    round(
        as.numeric(unlist(mysum[13])[2]), 3
    )
)

## WW unconstrained.
ww_un <- subset(ds, dum_ww == 0)
m5 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, 1:9) +
        stats:::lag(sq_inv, 1:1) +
        stats:::lag(fcl_normalizado, 2:2) +
        stats:::lag(divida, 1:1) +
        stats:::lag(cv, 3:9) +
        stats:::lag(tamanho, 1:9),
data = ww_un,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)

tex_m5 <- extract(m5,
    robust = TRUE,
    include.wald = FALSE,
    include.nobs = FALSE,
    include.sargan = FALSE
)

mysum <- summary(m5, robust = TRUE)

obs <- c(obs, nrow(ww_un))

sargan <- c(
    sargan,
    round(
        as.numeric(unlist(mysum[11])[2]), 3
    )
)

arbond <- c(
    arbond,
    round(
        as.numeric(unlist(mysum[13])[2]), 3
    )
)

# SA constrained
sa_con <- subset(ds, dum_sa == 1)
sa_con <- filter_seq(sa_con, sa_con$ano, 4)
m6 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, 0:0) +
        stats:::lag(sq_inv, 0:0) +
        stats:::lag(fcl_normalizado, 1:1) +
        stats:::lag(divida, 1:1) +
        stats:::lag(cv, 0:0) +
        stats:::lag(tamanho, 3:3),
data = sa_con,
effect = "individual",
model = "twosteps",
collapse = FALSE,
transformation = "ld",
fsm = "full",
)

tex_m6 <- extract(m6,
    robust = TRUE,
    include.wald = FALSE,
    include.nobs = FALSE,
    include.sargan = FALSE
)

mysum <- summary(m6, robust = TRUE)

obs <- c(obs, nrow(sa_con))

sargan <- c(
    sargan,
    round(
        as.numeric(unlist(mysum[11])[2]), 3
    )
)

arbond <- c(
    arbond,
    round(
        as.numeric(unlist(mysum[13])[2]), 3
    )
)

# SA unconstrained.
sa_un <- subset(ds, dum_sa == 0)
sa_un <- filter_seq(sa_un, sa_un$ano, 4)
m7 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho |
    stats:::lag(inv, 2:2) +
        stats:::lag(sq_inv, 0:9) +
        stats:::lag(fcl_normalizado, 0:0) +
        stats:::lag(divida, 2:2) +
        stats:::lag(cv, 1:9) +
        stats:::lag(tamanho, 3:3),
data = sa_un,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)

tex_m7 <- extract(m7,
    robust = TRUE,
    include.wald = FALSE,
    include.nobs = FALSE,
    include.sargan = FALSE
)

mysum <- summary(m7, robust = TRUE)

obs <- c(obs, nrow(sa_un))

sargan <- c(
    sargan,
    round(
        as.numeric(unlist(mysum[11])[2]), 3
    )
)

arbond <- c(
    arbond,
    round(
        as.numeric(unlist(mysum[13])[2]), 3
    )
)


### Latex format

# Pass parameters.
models <- list(
    tex_m1, tex_m2, tex_m3,
    tex_m4, tex_m5, tex_m6, tex_m7
)

label <- list("teste" = 1)

model_names <- c(
    "FULL",
    "1", "2",
    "3", "4",
    "5", "6"
)

var_names <- c(
    "$INV_{t-1}$",
    "$INV_{t-1}^2$",
    "$log(EPU)$",
    "$FC$",
    "$DIV$",
    "$CV$",
    "$TAM$"
)
custom_gofs <- list(
    "Observações" = obs,
    "m2(p-valor)" = arbond,
    "Sargan (p-valor)" = sargan
)

custom_caption <- "TESTE CAPTION TESTE"
custom_label <- "TABLE LABEL NAME"
custom_scale <- 0.8

# Compute latex output
texreg(
    models,
    single.row = FALSE,
    stars = c(0.01, 0.05, 0.1),
    custom.header = label,
    custom.model.names = model_names,
    custom.coef.names = var_names,
    custom.note = "",
    digits = 3,
    custom.gof.rows = custom_gofs,
    caption = custom_caption,
    label = custom_label,
    center = FALSE,
    booktabs = TRUE,
    dcolumn = TRUE,
    table = TRUE,
    caption.above = TRUE,
    scalebox = custom_scale
)