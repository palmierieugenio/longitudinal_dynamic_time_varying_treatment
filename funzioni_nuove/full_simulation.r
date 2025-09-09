
# ---------- 5) One Monte Carlo draw ----------
one_run_all_t2 <- function(params, seed) {
    df <- simulate_dpdm_long(
        N=params$N, L_pre=params$L_pre,
        beta=params$beta, gamma=params$gamma,
        theta0=params$theta0, theta1=params$theta1, theta2=params$theta2,
        sd_alpha=params$sd_alpha, sd_eps0=params$sd_eps0,
        sd_eps1_0=params$sd_eps1_0, sd_eps1_1=params$sd_eps1_1,
        sd_eps2_0=params$sd_eps2_0, sd_eps2_1=params$sd_eps2_1,
        a10=params$a10, a11=params$a11, a12=params$a12,
        a20=params$a20, a21=params$a21, a22=params$a22, a23=params$a23, a24=params$a24,
        x_rho=params$x_rho, sd_x=params$sd_x, sd_eta=params$sd_eta,
        delta0=params$delta0, delta1=params$delta1, delta2=params$delta2,
        b1x=params$b1x, b2x=params$b2x, seed=seed
    )
    
    # Wide estimators (unchanged)
    est_aipw   <- estimate_tau_aipw_t2_exact(df)
    est_gform  <- tryCatch(gformula_t2(df, L_pre = params$L_pre),  error=function(e) NA_real_)
    est_snmm   <- tryCatch(snmm_gest_t2(df, L_pre = params$L_pre), error=function(e) NA_real_)
    
    # Panel estimators (SYS-GMM only)
    panel_df   <- to_panel(df, L_pre = params$L_pre)
    est_sys    <- tryCatch(gmm_sys_t2_betaD(panel_df),                    error=function(e) NA_real_)
    est_mysys  <- tryCatch(my_pgmm_sys_t2_betaD(panel_df, params$L_pre),  error=function(e) NA_real_)
    est_mysys2 <- tryCatch(my_pgmm_sys_t2_betaD_sqrt(panel_df, params$L_pre), error=function(e) NA_real_)
    
    data.frame(
        sim_id        = seed,
        AIPW          = est_aipw,
        G_FORMULA     = est_gform,
        SNMM_GEST     = est_snmm,
        SYS_GMM       = est_sys,
        W_SYS_GMM     = est_mysys,
        W_SYS_GMM_SQRT= est_mysys2,
        true_tau      = params$beta
    )
}

# ---------- 6) Runner + stacked density plots (SAME X-AXIS, rotated left labels) ----------
run_compare_t2 <- function(R = 400, params, seed_master = 2025) {
    set.seed(seed_master)
    seeds <- sample.int(1e8, R)
    out <- do.call(rbind, lapply(seeds, function(s) one_run_all_t2(params, s)))
    true_val <- unique(out$true_tau)
    
    long <- rbind(
        data.frame(estimator = "AIPW",               estimate = out$AIPW),
        data.frame(estimator = "G-formula",          estimate = out$G_FORMULA),
        data.frame(estimator = "SNMM G-est",         estimate = out$SNMM_GEST),
        data.frame(estimator = "SYS-GMM",            estimate = out$SYS_GMM),
        data.frame(estimator = "Weighted SYS-GMM",   estimate = out$W_SYS_GMM),
        data.frame(estimator = "Weighted SYS-GMM (√w)", estimate = out$W_SYS_GMM_SQRT)
    )
    long <- long[is.finite(long$estimate), ]
    long$estimator <- factor(
        long$estimator,
        levels = c("AIPW","G-formula","SNMM G-est","SYS-GMM","Weighted SYS-GMM","Weighted SYS-GMM (√w)")
    )
    
    xr  <- range(long$estimate, finite = TRUE)
    pad <- 0.05 * diff(xr)
    xlim_shared <- c(xr[1] - pad, xr[2] + pad)
    
    p_facets <- ggplot(long, aes(x = estimate, fill = estimator)) +
        geom_density(alpha = 0.35, color = NA, adjust = 1.0) +
        geom_vline(xintercept = true_val, linetype = "dashed") +
        scale_x_continuous(limits = xlim_shared) +
        facet_grid(rows = vars(estimator), scales = "free_y", switch = "y") +
        labs(
            title = "t = 2: AIPW / G-formula / SNMM and SYS-GMM variants",
            subtitle = "Panels share the same x-axis (dashed = true β).",
            x = "Estimate", y = "Density"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "none",
            strip.placement = "outside",
            panel.grid.minor = element_blank(),
            strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, face = "bold", margin = margin(r = 8))
        )
    
    print(p_facets)
    
    # Numeric summaries
    summ <- function(v) c(mean=mean(v,na.rm=TRUE),
                          bias=mean(v,na.rm=TRUE)-true_val,
                          rmse=sqrt(mean((v-true_val)^2,na.rm=TRUE)))
    stats_to_report <- c("AIPW","G_FORMULA","SNMM_GEST","SYS_GMM","W_SYS_GMM","W_SYS_GMM_SQRT")
    cat("\nSummary (R = ", nrow(out), "):\n", sep = "")
    for (nm in stats_to_report) {
        s <- summ(out[[nm]])
        cat(sprintf("%-22s -> Mean: %.3f | Bias: %.3f | RMSE: %.3f\n", nm, s["mean"], s["bias"], s["rmse"]))
    }
    
    invisible(list(draws = out, plot = p_facets, long = long, true_val = true_val, xlim = xlim_shared))
}
