data_generator_SI <- function(N, times, rho, beta, theta, beta_confounder, effetto, statistic = somma_treatment,
                              sigma_alfa, sigma_alfa_t, sigma_y, sigma_D, burn_in, pre_treatment) {
    
    total_time <- burn_in + pre_treatment + times
    alpha <- rnorm(N, mean = 0, sd = sigma_alfa)                   # Individual effects
    alpha_t <- rnorm(total_time, mean = 0, sd = sigma_alfa_t)     # Time effects
    y_star <- rnorm(N, mean = 10, sd = sigma_y)                   # Initial y*
    
    X <- matrix(rnorm(N * total_time), N, total_time)             # Covariates
    U <- matrix(rnorm(N * total_time), N, total_time)             # Unobserved confounders
    y <- matrix(0, N, total_time)                                 # Response
    D <- matrix(0, N, total_time)                                 # Treatment
    
    # Initial value
    y[,1] <- alpha + rho * y_star + beta * X[,1] + beta_confounder * U[,1] + rnorm(N, 0, sigma_y) + alpha_t[1]
    
    for (t in 2:total_time) {
        if (t > burn_in + pre_treatment) {
            if (t == burn_in + pre_treatment + 1) {
                prob <- plogis(theta[1] + theta[2] * X[, t] + theta[3] * y[, t-1]) # (burn_in+1):(t-1)
            } else {
                prob <- plogis(theta[2] * X[, t] +  theta[3] * y[, t-1]+ theta[4] * D[, t-1]) # (burn_in+1):(t-1)
            }
            D[, t] <- rbinom(N, 1, prob)
        }
        
        y[,t] <- alpha + rho * y[,t-1] + beta * X[,t] + beta_confounder * U[,t-1] + 
            rnorm(N, 0, sigma_y) + alpha_t[t]
        
        w_t <- D[, 1:t]
        y[,t] <- y[,t] + statistic(w_t, effect = effetto)
    }
    
    D_cum<-t(apply(D, 1, cumsum))
    
    # Discard burn-in period, keep only last 'times' observations
    y <- y[, (burn_in+1):(times +pre_treatment+ burn_in)]
    X <- X[, (burn_in+1):(times +pre_treatment+ burn_in)]
    D<-D[, (burn_in+1):(times +pre_treatment+ burn_in)]
    y<-reshape::melt(y)
    X<-reshape::melt(X)
    D<-reshape::melt(D)
    D_cum<-reshape::melt(D_cum)
    data<-merge(y, X, by = c("X1", "X2"))
    colnames(data)<-c( "X1", "X2","y", "X")
    data<-merge(data, D, by = c("X1", "X2"))
    colnames(data)<-c( "X1", "X2","y", "X", "D")
    data<-merge(data, D_cum, by= c("X1", "X2"))
    colnames(data)<-c( "individual_id", "time_id","y", "X", "D", "D_cum")
    
    
    # # Create treatment history using Reduce to accumulate past values
    data <- data  %>%
        group_by(individual_id) %>%
        arrange(time_id) %>%
        mutate(treatment_history = Reduce(function(x, y) paste0(x, y), as.character(D), accumulate = TRUE)) %>%
        ungroup()
    
    # Create full treatment sequence as the treatment history in the last period for each individual
    data <- data %>%
        group_by(individual_id) %>%
        mutate(full_treatment_sequence = last(treatment_history)) %>%
        ungroup()
    
    data$treatment_history<-sapply(data$treatment_history,
                                   function(x) substr(x, pre_treatment+1, nchar(x)))
    data$treatment_history[data$treatment_history==""]<-"pre"
    
    data$full_treatment_sequence<-sapply(data$full_treatment_sequence,
                                         function(x) substr(x, pre_treatment+1, nchar(x)))
    
    data<-as.data.frame(data)
    data<-data[order(data$individual_id , data$time_id), ]
    return(data)
}
