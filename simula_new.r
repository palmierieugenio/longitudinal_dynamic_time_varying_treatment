# ===========================================
# t = 2 comparison with LONGER PRE-TREATMENT HISTORY
# Estimators: AIPW (unchanged), G-formula, SNMM G-est,
#             SYS-GMM β_D, Weighted SYS-GMM β_D, Weighted SYS-GMM β_D (√w)
# Instruments: | lag(y, 2:99)
# ===========================================

suppressPackageStartupMessages({
    library(ggplot2)
    library(plm)        # pgmm
    library(Formula)
    library(WeightIt)   # weightitMSM
})

# --- load package functions from local folders (your code) ---
funzioni <- list.files("funzioni", full.names = TRUE)
invisible(lapply(funzioni, source))
funzioni_pacchetto <- list.files("funzioni_pacchetto", full.names = TRUE)
invisible(lapply(funzioni_pacchetto, source))
# nuove funzioni
funzioni_new <- list.files("funzioni_nuove", full.names = TRUE)
invisible(lapply(funzioni_new, source))


# ---------- 7) Parameters & run ----------
effect<-5
params <- list(
    N = 2000,
    L_pre = 5,               # number of pre-treatment periods: t = -3, -2, -1
    beta  = effect,             # instantaneous direct effect
    gamma = 0.3,             # AR coefficient
    theta0 = 0.3, theta1 = 0.3, theta2 = -0.1,
    sd_alpha = 1.0,
    sd_eps0   = 1,
    sd_eps1_0 = 1, sd_eps1_1 = 1,
    sd_eps2_0 = 1, sd_eps2_1 = 1,
    a10 = -0.1, a11 = 0.08, a12 = 0.0,
    a20 = -0.1, a21 = 0.1, a22 = 0.1, a23 = 0.1, a24 = 0.0,
    # Covariate parameters
    x_rho = 0.5, sd_x = 1.0, sd_eta = 1.0,
    delta0 = 0.01, delta1 = 0.05, delta2 = 0.05,
    b1x = 0.01, b2x = 0.03
)

res_compare_t2 <- run_compare_t2(R = 200, params = params)
round(colMeans(res_compare_t2$draws), 3)
round(colMeans(res_compare_t2$draws)-effect, 3)


# --- Optional: adjust common x-range later ---
# print(res_compare_t2$plot + scale_x_continuous(limits = c(3, 7)))
