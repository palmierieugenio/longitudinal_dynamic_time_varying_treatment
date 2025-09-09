# ---------- 4f) SNMM G-estimation (blip at t=2; uses pre-treatment in H2) ----------
snmm_gest_t2 <- function(df, L_pre, eps = 1e-6) {
    pret <- intersect(c(paste0("Y_m", seq_len(L_pre)), paste0("X_m", seq_len(L_pre))), names(df))
    m_ps <- glm(as.formula(paste("D2 ~", paste(c("Y1","D1","X1", pret), collapse = " + "))),
                data = df, family = binomial())
    p2   <- pmin(pmax(predict(m_ps, type = "response"), eps), 1 - eps)
    Z    <- df$D2 - p2
    
    m0   <- lm(as.formula(paste("Y2 ~", paste(c("Y1","D1","X1","Y0","X0", pret), collapse = " + "))),
               data = subset(df, D2 == 0))
    m0hat<- predict(m0, newdata = df)
    
    num  <- sum( Z * (df$Y2 - m0hat) )
    den  <- sum( Z * df$D2 )
    if (!is.finite(num/den)) return(NA_real_)
    as.numeric(num / den)
}