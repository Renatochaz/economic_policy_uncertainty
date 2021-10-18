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



### Latex formato
textreg(m1)