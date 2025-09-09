# ---------- 3) Panel with pre-treatment ----------
to_panel <- function(df, L_pre) {
    rows <- list()
    for (k in seq(L_pre, 1)) {
        rows[[length(rows)+1]] <- data.frame(
            individual_id = df$id,
            time_id = -k,
            y = df[[paste0("Y_m", k)]],
            D = 0L,
            x = df[[paste0("X_m", k)]]
        )
    }
    rows[[length(rows)+1]] <- data.frame(individual_id=df$id, time_id=0, y=df$Y0, D=0L,    x=df$X0)
    rows[[length(rows)+1]] <- data.frame(individual_id=df$id, time_id=1, y=df$Y1, D=df$D1, x=df$X1)
    rows[[length(rows)+1]] <- data.frame(individual_id=df$id, time_id=2, y=df$Y2, D=df$D2, x=df$X2)
    do.call(rbind, rows)
}

# ---------- 4) SYS-GMM @ t=2 (transformation="ld") ----------
gmm_sys_t2_betaD <- function(panel_df) {
    data_2 <- subset(panel_df, time_id <= 2)
    fit2 <- pgmm(
        Formula::Formula(
            y ~ lag(y, 1) + D + lag(D) + x |
                lag(y, 2:99)
        ),
        data   = data_2,
        index  = c("individual_id", "time_id"),
        effect = "individual",
        model  = "onestep",
        transformation = "ld"   # SYS-GMM
    )
    as.numeric(coef(fit2)["D"])
}

# ---------- 4b) IPW using pre-treatment history ----------
estimate_weights_from_panel <- function(panel_df, L_pre, trim = NULL) {
    wide <- reshape(panel_df, idvar = "individual_id", timevar = "time_id", direction = "wide")
    gcol <- function(nm) if (nm %in% names(wide)) wide[[nm]] else NULL
    
    data_wide <- data.frame(
        id = wide$individual_id,
        Y0 = gcol("y.0"), Y1 = gcol("y.1"),
        D1 = gcol("D.1"), D2 = gcol("D.2"),
        X0 = gcol("x.0"), X1 = gcol("x.1")
    )
    for (k in seq_len(L_pre)) {
        if (!is.null(gcol(paste0("y.", -k)))) data_wide[[paste0("Y_m", k)]] <- gcol(paste0("y.", -k))
        if (!is.null(gcol(paste0("x.", -k)))) data_wide[[paste0("X_m", k)]] <- gcol(paste0("x.", -k))
    }
    
    pret_terms <- intersect(paste0("Y_m", seq_len(L_pre)), names(data_wide))
    pret_terms <- union(pret_terms, intersect(paste0("X_m", seq_len(L_pre)), names(data_wide)))
    rhs_D1 <- paste(c("Y0", "X0", pret_terms), collapse = " + ")
    rhs_D2 <- paste(c("Y1", "Y0", "D1", "X1", pret_terms), collapse = " + ")
    
    f1 <- as.formula(paste("D1 ~", rhs_D1))
    f2 <- as.formula(paste("D2 ~", rhs_D2))
    
    w_raw <- WeightIt::weightitMSM(
        list(f1, f2),
        data = data_wide,
        stabilize = TRUE,
        method = "glm",
        family = binomial()
    )$weights
    
    w_raw[!is.finite(w_raw)] <- NA_real_
    if (!is.null(trim) && is.numeric(trim) && trim > 0 && trim < 0.5) {
        qs <- stats::quantile(w_raw, probs = c(trim, 1 - trim), na.rm = TRUE)
        w_raw <- pmin(pmax(w_raw, qs[1]), qs[2])
    }
    data.frame(id = data_wide$id, w2 = w_raw)
}

# ---------- 4c) my_pgmm SYS-GMM @ t=2 (IPW pesi) ----------
my_pgmm_sys_t2_betaD <- function(panel_df, L_pre) {
    data_2  <- subset(panel_df, time_id <= 2)
    w_df    <- estimate_weights_from_panel(panel_df, L_pre = L_pre)
    ids     <- unique(data_2$individual_id)
    pesi_vec <- w_df$w2[match(ids, w_df$id)]
    
    fit2 <- my_pgmm(
        Formula::Formula(
            y ~ lag(y, 1) + D + lag(D) + x |
                lag(y, 2:99)
        ),
        data   = data_2,
        index  = c("individual_id", "time_id"),
        effect = "individual",
        model  = "onestep",
        transformation = "ld",   # SYS-GMM
        pesi = pesi_vec
    )
    as.numeric(coef(fit2)["D"])
}

# ---------- 4d) my_pgmm SYS-GMM with √IPW ----------
my_pgmm_sys_t2_betaD_sqrt <- function(panel_df, L_pre) {
    data_2  <- subset(panel_df, time_id <= 2)
    w_df    <- estimate_weights_from_panel(panel_df, L_pre = L_pre)
    ids     <- unique(data_2$individual_id)
    pesi_vec <- sqrt(w_df$w2[match(ids, w_df$id)])
    
    fit2 <- my_pgmm(
        Formula::Formula(
            y ~ lag(y, 1) + D + lag(D) + x |
                lag(y, 2:99)
        ),
        data   = data_2,
        index  = c("individual_id", "time_id"),
        effect = "individual",
        model  = "onestep",
        transformation = "ld",   # SYS-GMM
        pesi = pesi_vec
    )
    as.numeric(coef(fit2)["D"])
}