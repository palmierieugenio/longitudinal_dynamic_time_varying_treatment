# ---------- 2) Article estimator @ t=2 (UNCHANGED) ----------
# m1 = E[D2 | Y1, D1, X1], m2 = E[ΔY2 | Y1, D1, X1, D2=0]
build_tilde_t2_exact <- function(df, eps = 1e-4) {
    dat <- df
    dat$dY2 <- with(dat, Y2 - Y1)
    m1 <- glm(D2 ~ Y1 + D1 + X1, data = dat, family = binomial())
    M1 <- pmin(pmax(as.numeric(predict(m1, type="response")), eps), 1 - eps)
    m2 <- lm(dY2 ~ Y1 + D1 + X1, data = subset(dat, D2 == 0))
    M2 <- as.numeric(predict(m2, newdata = dat))
    (dat$dY2 - M2) / M1
}
estimate_tau_aipw_t2_exact <- function(df, eps = 1e-4) mean(build_tilde_t2_exact(df, eps))