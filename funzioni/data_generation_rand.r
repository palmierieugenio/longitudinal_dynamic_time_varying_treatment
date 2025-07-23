


data_generator_random <- function(N, times, rho, beta, beta_confounder, effetto, statistic=somma_treatment,
                                  sigma_alfa,  sigma_alfa_t, sigma_y, sigma_D,burn_in, pre_treatment) {
    alpha <- rnorm(N, mean=0, sd=sigma_alfa)  # Individual fixed effects
    alpha_t<-rnorm(times + burn_in+pre_treatment, mean=0, sd=sigma_alfa_t)
    y_star <- rnorm(N, mean=10, sd=sigma_y) # random initialization with mean 10
    X <- matrix(rnorm(N * (times + burn_in+pre_treatment)), nrow=N, ncol=(times + burn_in+pre_treatment))  # Covariates
    U<-matrix(rnorm(N * (times + burn_in+pre_treatment)), nrow=N, ncol=(times + burn_in+pre_treatment)) # not observed confounder
    y <- matrix(0, nrow=N, ncol=(times + burn_in+pre_treatment))  # Response variable
    # treatment
    D_pre <- matrix( 0, N, pre_treatment)
    D <- matrix( rbinom(N*times, 1,0.5 ), N, times)
    D<-cbind(D_pre, D)
    D_cum<-t(apply(D, 1, cumsum))
    
    # outcome
    y[,1] <- alpha + rho * y_star + beta * X[,1] +  beta_confounder * U[,1]+ rnorm(N, mean=0, sd=sigma_y)+alpha_t[1]
    
    for (t in 2:(times + burn_in+pre_treatment)) {
        if(t<=burn_in){
            y[,t] <- alpha + rho * y[,t-1] + beta * X[,t]+  beta_confounder * U[,t-1]+ rnorm(N, mean=0, sd=sigma_y)+alpha_t[t]
        }
        else{
            y[,t] <- alpha + rho * y[,t-1] + beta * X[,t]+  beta_confounder * U[,t-1] + rnorm(N, mean=0, sd=sigma_y)+alpha_t[t]
            w_t<-D[,seq_along(1:(t-burn_in))]
            y[,t]<- y[,t]+statistic(w_t, effect = effetto) 
        }
    }
    
    
    # Discard burn-in period, keep only last 'times' observations
    y <- y[, (burn_in+1):(times +pre_treatment+ burn_in)]
    X <- X[, (burn_in+1):(times +pre_treatment+ burn_in)]
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





