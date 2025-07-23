bias_variance_decomposition <- function(theta_hat_sim, beta_true) {
    # Ensure input types
    theta_hat_sim <- as.matrix(theta_hat_sim)
    beta_true <- as.numeric(beta_true)
    
    # Mean of the estimates across simulations
    theta_hat_mean <- colMeans(theta_hat_sim)
    
    # Bias^2: squared difference between mean estimate and true value
    bias_squared <- (theta_hat_mean - beta_true)^2
    
    # Variance: variance of the estimates across simulations
    variances <- apply(theta_hat_sim, 2, var)
    
    # MSE = Bias^2 + Variance
    mse <- bias_squared + variances
    
    # Output as data frame
    result <- data.frame(
        Parameter = seq_along(beta_true),
        BiasSquared = bias_squared,
        Variance = variances,
        MSE = mse
    )
    
    return(result)
}