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
ds$ln_iie <- log(ds$iee)
ds$ln_fbcf <- log(ds$fbcf)

# Set dataframe with definied panel indexes.
ds <- pdata.frame(ds, index = c("codigo", "ano"))

# Full model.
m1 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho + ln_iie |
    stats:::lag(inv, 0:9) +
        stats:::lag(sq_inv, 0:9) +
        stats:::lag(fcl_normalizado, 1:9) +
        stats:::lag(divida, 1:1) +
        stats:::lag(cv, 1:1) +
        stats:::lag(tamanho, 1:1),
data = ds,
effect = "individual",
model = "twosteps",
collapse = FALSE,
transformation = "ld",
fsm = "full",
)

summary(m1, robust = TRUE)

# KZ constrained.
kz_con <- subset(ds, dum_kz == 1)
kz_con <- filter_seq(kz_con, kz_con$ano, 4)
m2 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho
    + ln_fbcf |
    stats:::lag(inv, 2:2) +
        stats:::lag(sq_inv, 1:9) +
        stats:::lag(fcl_normalizado, 1:1) +
        stats:::lag(divida, 2:9) +
        stats:::lag(cv, 1:9) +
        stats:::lag(tamanho, 2:9),
data = kz_con,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)

summary(m2, robust = TRUE)

# KZ unconstrained.
kz_un <- subset(ds, dum_kz == 0)
kz_un <- filter_seq(kz_un, kz_un$ano, 4)
m3 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho + ln_fbcf |
    stats:::lag(inv, 1:9) +
        stats:::lag(sq_inv, 2:2) +
        stats:::lag(fcl_normalizado, 1:9) +
        stats:::lag(divida, 2:9) +
        stats:::lag(cv, 0:9) +
        stats:::lag(tamanho, 1:9),
data = kz_un,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)
summary(m3, robust = TRUE)

# SA constrained
sa_con <- subset(ds, dum_sa == 1)
sa_con <- filter_seq(sa_con, sa_con$ano, 4)
m4 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho + ln_fbcf |
    stats:::lag(inv, 2:9) +
        stats:::lag(sq_inv, 2:9) +
        stats:::lag(fcl_normalizado, 0:9) +
        stats:::lag(divida, 2:9) +
        stats:::lag(cv, 1:9) +
        stats:::lag(tamanho, 2:9),
data = sa_con,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)
summary(m4, robust = TRUE)

# SA unconstrained.
sa_un <- subset(ds, dum_sa == 0)
sa_un <- filter_seq(sa_un, sa_un$ano, 4)
m5 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho + ln_fbcf |
    stats:::lag(inv, 2:2) +
        stats:::lag(sq_inv, 1:9) +
        stats:::lag(fcl_normalizado, 0:0) +
        stats:::lag(divida, 2:9) +
        stats:::lag(cv, 0:0) +
        stats:::lag(tamanho, 1:1),
data = sa_un,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)
summary(m5, robust = TRUE)

# WW constrained.
ww_con <- subset(ds, dum_ww == 1)
ww_con <- filter_seq(ww_con, ww_con$ano, 4)
m6 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho + ln_fbcf |
    stats:::lag(inv, 2:9) +
        stats:::lag(sq_inv, 0:9) +
        stats:::lag(fcl_normalizado, 2:2) +
        stats:::lag(divida, 0:0) +
        stats:::lag(cv, 0:9) +
        stats:::lag(tamanho, 1:9),
data = ww_con,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)
summary(m6, robust = TRUE)

# WW unconstrained.
ww_un <- subset(ds, dum_ww == 0)
# ww_un <- filter_seq(ww_un, ww_un$ano, 4)
m7 <- pgmm(inv ~ stats:::lag(inv, 1:1) + stats:::lag(sq_inv, 1:1) +
    ln_epu + fcl_normalizado + divida + cv + tamanho + q_tobin + fbcf |
    stats:::lag(inv, 1:1) +
        stats:::lag(sq_inv, 1:1) +
        stats:::lag(fcl_normalizado, 2:2) +
        stats:::lag(divida, 0:0) +
        stats:::lag(cv, 0:0) +
        stats:::lag(tamanho, 0:0) |
    stats:::lag(q_tobin, 1:1) +
        stats:::lag(fbcf, 1:1),
data = ww_un,
effect = "individual",
model = "twosteps",
collapse = TRUE,
transformation = "ld",
fsm = "full",
)

summary(m7, robust = TRUE)
