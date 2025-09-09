# ---------- 4e) Parametric g-formula at t=2 (uses pre-treatment as controls) ----------
gformula_t2 <- function(df, L_pre) {
    pret <- intersect(c(paste0("Y_m", seq_len(L_pre)), paste0("X_m", seq_len(L_pre))), names(df))
    rhs <- paste(c("D2", "Y1", "D1", "X1", "Y0", "X0", pret), collapse = " + ")
    m <- lm(as.formula(paste("Y2 ~", rhs)), data = df)
    y1 <- predict(m, newdata = transform(df, D2 = 1))
    y0 <- predict(m, newdata = transform(df, D2 = 0))
    mean(y1 - y0)
}