## Link: https://cran.r-project.org/web/packages/vars/vignettes/vars.pdf
## VAR, SVAR, SVEC implementation within R package "vars"


library("vars")
data("Canada")
summary(Canada) 
# prod=productivity, e=log of employment,
# U=umeployment rate, rw= log real wage
plot(Canada, nc = 2, xlab = "")


# unit root test - Augmented Dickey-Fuller --------------------------------
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1

adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift", 
                         lags = 1))
adf2

# no idea how to conclude that the time series are integrated of order one


# select optimal lag length  -------------------
## select optimal lag length for an unrestricted VAR for a maximal lag length of eight.

VARselect(Canada, lag.max = 8, type = "both")

# AIC, FPE recommend optimal lag length, p=3
# HQ indicate p=2, SC indicate p=1


# est. of VAR(1) ----------------------------------------------------------

Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct <- VAR(Canada, p = 1, type = "both")
p1ct

summary(p1ct, equation = "e")
plot(p1ct, names = "e")


# diagnostic tests for VAR(1) ---------------------------------------------

ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial

norm1 <- normality.test(p1ct)
norm1$jb.mul

arch1 <- arch.test(p1ct, lags.multi = 5)
arch1$arch.mul

plot(arch1, names = "e")
plot(stability(p1ct), nc = 2)

# concluded that a VAR(1)-specification might be too restrictive
# stability tests do indicate somem derivations from parameter constancy
# the time invariant specification fo the VAR(2) and VAR(3) 
# will be maintained as tentative canditates for the conintegration analysis


# cointegration tests -----------------------------------------------------

# est. VECM where deterministic trend incl. in the cointegration relation

summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3,
              spec = "transitory"))

summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2,
              spec = "transitory"))

# indicate one cointegration relationship

# VECM is re-estimated with this restriction:
# ca.jo used values from Osterwald-Lenum (1992)
# and normalization of the long-run relationship with respect to real wages

vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace",
              ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)

## dun understand this part, check ca.jo, cajorls function


