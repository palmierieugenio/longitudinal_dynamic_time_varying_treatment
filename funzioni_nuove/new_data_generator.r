# ---------- 1) Simulator with L_pre pre-treatment periods (AR(1) carries forward) ----------
simulate_dpdm_long <- function(
        N, L_pre,
        beta, gamma, theta0, theta1, theta2,
        sd_alpha, sd_eps0, sd_eps1_0, sd_eps1_1, sd_eps2_0, sd_eps2_1,
        a10, a11, a12, a20, a21, a22, a23, a24,
        # Covariate process and effects
        x_rho , sd_x , sd_eta ,
        delta0 , delta1 , delta2,      # outcome slopes on X_t
        b1x , b2x ,                     # treatment slopes on X_0/X_1
        seed
) {
    set.seed(seed)
    invlogit <- function(x) 1/(1 + exp(-x))
    alpha_i <- rnorm(N, 0, sd_alpha)
    
    # X_t as AR(1) for t = -L_pre..2
    X_list <- vector("list", L_pre + 3); names(X_list) <- as.character(seq(-L_pre, 2))
    X_list[[1]] <- rnorm(N, 0, sd_x)
    for (tt in seq(-L_pre + 1, 2)) {
        eta <- rnorm(N, 0, sd_eta)
        X_list[[as.character(tt)]] <- x_rho * X_list[[as.character(tt - 1)]] + eta
    }
    
    # Y_t pre-treatment (no treatment before t = 1)
    Y_list <- vector("list", L_pre + 3); names(Y_list) <- as.character(seq(-L_pre, 2))
    Y_list[[as.character(-L_pre)]] <- alpha_i + delta0 * X_list[[as.character(-L_pre)]] + rnorm(N, 0, sd_eps0)
    for (tt in seq(-L_pre + 1, -1)) {
        e <- rnorm(N, 0, sd_eps0)
        Y_list[[as.character(tt)]] <- gamma * Y_list[[as.character(tt - 1)]] +
            0 + delta0 * X_list[[as.character(tt)]] + alpha_i + e
    }
    
    # t = 0
    eps0 <- rnorm(N, 0, sd_eps0)
    Y0   <- gamma * Y_list[["-1"]] + theta0 + delta0 * X_list[["0"]] + alpha_i + eps0
    
    # D1 (t=1)
    pD1 <- invlogit(a10 + a11 * Y0 + a12 * alpha_i + b1x * X_list[["0"]])
    D1  <- rbinom(N, 1, pD1)
    
    # t = 1 potential + realized
    eps1_0 <- rnorm(N, 0, sd_eps1_0); eps1_1 <- rnorm(N, 0, sd_eps1_1)
    Y1_0 <- gamma * Y0 + theta1 + delta1 * X_list[["1"]] + alpha_i + eps1_0
    Y1_1 <- beta  + gamma * Y0 + theta1 + delta1 * X_list[["1"]] + alpha_i + eps1_1
    Y1   <- ifelse(D1 == 1, Y1_1, Y1_0)
    
    # D2 (t=2)
    pD2 <- invlogit(a20 + a21 * Y1 + a22 * Y0 + a23 * D1 + a24 * alpha_i + b2x * X_list[["1"]])
    D2  <- rbinom(N, 1, pD2)
    
    # t = 2 potential + realized
    eps2_0 <- rnorm(N, 0, sd_eps2_0); eps2_1 <- rnorm(N, 0, sd_eps2_1)
    Y2_0 <- gamma * Y1 + theta2 + delta2 * X_list[["2"]] + alpha_i + eps2_0
    Y2_1 <- beta  + gamma * Y1 + theta2 + delta2 * X_list[["2"]] + alpha_i + eps2_1
    Y2   <- ifelse(D2 == 1, Y2_1, Y2_0)
    
    df <- data.frame(
        id = seq_len(N),
        Y2 = Y2, Y1 = Y1, Y0 = Y0,
        D2 = D2, D1 = D1,
        X2 = X_list[["2"]], X1 = X_list[["1"]], X0 = X_list[["0"]]
    )
    for (k in seq_len(L_pre)) {
        tt <- -k
        df[[paste0("Y_m", k)]] <- Y_list[[as.character(tt)]]
        df[[paste0("X_m", k)]] <- X_list[[as.character(tt)]]
    }
    df
}