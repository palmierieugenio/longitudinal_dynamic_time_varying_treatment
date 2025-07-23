itera <- function(iter, N, times, rho_true, beta_true, theta_true, xi_true,
                  beta_confounder, effetto, burn_in, sigma_alfa,
                  sigma_alfa_t, sigma_y, sigma_D, pre_treatment, statistic   ) {
    # res Diff-GMM
    estimates_SI_cov_6 <- estimates_SI_cov_7 <- estimates_rand_6 <- estimates_rand_7 <- estimates_SI_6 <- estimates_SI_7 <- NULL
    estimates_SI_cov_8 <- estimates_rand_8 <- estimates_SI_8  <- NULL

    my_estimates_SI_cov_6 <-  my_estimates_SI_cov_7 <-  my_estimates_rand_6 <-  my_estimates_rand_7 <-  my_estimates_SI_6 <-  my_estimates_SI_7 <- NULL
    my_estimates_SI_cov_8 <-  my_estimates_rand_8 <-  my_estimates_SI_8  <- NULL
    # res SYS-GMM
    estimates_SI_cov_6_sys <- estimates_SI_cov_7_sys <- estimates_rand_6_sys <- estimates_rand_7_sys <- estimates_SI_6_sys <- estimates_SI_7_sys <- NULL
    estimates_SI_cov_8_sys <- estimates_rand_8_sys <- estimates_SI_8_sys  <- NULL
    
    my_estimates_SI_cov_6_sys <-  my_estimates_SI_cov_7_sys <-  my_estimates_rand_6_sys <-  my_estimates_rand_7_sys <-  my_estimates_SI_6_sys <-  my_estimates_SI_7_sys <- NULL
    my_estimates_SI_cov_8_sys<-  my_estimates_rand_8_sys <-  my_estimates_SI_8_sys  <- NULL
    
    total_start <- Sys.time()
    
    message("Starting random data simulations...")
    start_rand <- Sys.time()
    for (i in 1:iter) {
        iter_start <- Sys.time()
        
        data_random <- data_generator_random(N, times, rho_true, beta_true, beta_confounder, effetto,statistic,
                                             sigma_alfa,sigma_alfa_t, sigma_y ,sigma_D ,  burn_in, pre_treatment )
        
        # stime non pesate
        est_all_cond_rand <-  estimation_all_conditional(data_random,  transformation="d")
        est_all_cond_rand_sys <-  estimation_all_conditional(data_random,  transformation="ld")
        #  stime pesate
        my_est_all_cond_rand <-  my_estimation_all_conditional(data_random,  transformation="d")
        my_est_all_cond_rand_sys <-  my_estimation_all_conditional(data_random,  transformation="ld")
        
        # stime Difference GMM
        estimates_rand_6 <- rbind(estimates_rand_6, est_all_cond_rand$y6)
        estimates_rand_7 <- rbind(estimates_rand_7, est_all_cond_rand$y7)
        estimates_rand_8 <- rbind(estimates_rand_8, est_all_cond_rand$y8)
        
        my_estimates_rand_6 <- rbind(my_estimates_rand_6, my_est_all_cond_rand$y6)
        my_estimates_rand_7 <- rbind(my_estimates_rand_7, my_est_all_cond_rand$y7)
        my_estimates_rand_8 <- rbind(my_estimates_rand_8, my_est_all_cond_rand$y8)
        # Stime SYS-GMM
        estimates_rand_6_sys <- rbind(estimates_rand_6_sys, est_all_cond_rand_sys$y6)
        estimates_rand_7_sys <- rbind(estimates_rand_7_sys, est_all_cond_rand_sys$y7)
        estimates_rand_8_sys <- rbind(estimates_rand_8_sys, est_all_cond_rand_sys$y8)
        
        my_estimates_rand_6_sys <- rbind(my_estimates_rand_6_sys, my_est_all_cond_rand_sys$y6)
        my_estimates_rand_7_sys <- rbind(my_estimates_rand_7_sys, my_est_all_cond_rand_sys$y7)
        my_estimates_rand_8_sys <- rbind(my_estimates_rand_8_sys, my_est_all_cond_rand_sys$y8)
        
        elapsed <- Sys.time() - start_rand
        avg_time <- as.numeric(elapsed, units = "secs") / i
        est_remaining <- (iter - i) * avg_time
        
        message(sprintf("[Random] Iteration %d/%d | Elapsed: %.2f sec | Est. remaining: %.2f sec", 
                        i, iter, as.numeric(Sys.time() - iter_start, units = "secs"), est_remaining))
    }
    
    message("Starting SI data simulations...")
    start_SI <- Sys.time()
    for (j in 1:iter) {
        iter_start <- Sys.time()
        
        data_SI <- data_generator_SI(N, times, rho_true, beta_true, theta_true, beta_confounder, effetto,statistic,
                                     sigma_alfa,sigma_alfa_t, sigma_y ,sigma_D ,  burn_in, pre_treatment)
        
        
        
        # # dynamite
        # data_SI_test<-data_SI
        # data_SI_test$D<-factor(data_SI_test$D)
        # model<-obs(y ~ lag(y)+X +D+lag(D) + random(~1), family = "gaussian")
        # model2<-model+obs(D ~ lag(y)+X +lag(D), family = "bernoulli")
        # return(data_SI_test)
        # fit<-dynamite(model, data=data_SI_test,time = "time_id",
        #               group = "individual_id", iter=100, chains=1 )
        # 
        # 
        # fit2<-dynamite(model2, data=data_SI_test,time = "time_id",
        #               group = "individual_id", iter=100, chains=1 )
        # 
        # return(list(fit=fit, fit2=fit2))
       
        
        
        
        # stime non pesate
        est_all_cond_SI <- estimation_all_conditional(data_SI,  transformation="d")
        est_all_cond_SI_sys <- estimation_all_conditional(data_SI,  transformation="ld")
        # stime  pesate
        my_est_all_cond_SI <- my_estimation_all_conditional(data_SI,  transformation="d")
        my_est_all_cond_SI_sys <- my_estimation_all_conditional(data_SI,  transformation="ld")
        # stime Difference gmm
        estimates_SI_6 <- rbind(estimates_SI_6, est_all_cond_SI$y6)
        estimates_SI_7 <- rbind(estimates_SI_7, est_all_cond_SI$y7)
        estimates_SI_8 <- rbind(estimates_SI_8, est_all_cond_SI$y8)
        
        my_estimates_SI_6 <- rbind(my_estimates_SI_6, my_est_all_cond_SI$y6)
        my_estimates_SI_7 <- rbind(my_estimates_SI_7, my_est_all_cond_SI$y7)
        my_estimates_SI_8 <- rbind(my_estimates_SI_8, my_est_all_cond_SI$y8)
        
        # stime SYS gmm
        estimates_SI_6_sys <- rbind(estimates_SI_6_sys, est_all_cond_SI_sys$y6)
        estimates_SI_7_sys <- rbind(estimates_SI_7_sys, est_all_cond_SI_sys$y7)
        estimates_SI_8_sys <- rbind(estimates_SI_8_sys, est_all_cond_SI_sys$y8)
        
        my_estimates_SI_6_sys <- rbind(my_estimates_SI_6_sys, my_est_all_cond_SI_sys$y6)
        my_estimates_SI_7_sys <- rbind(my_estimates_SI_7_sys, my_est_all_cond_SI_sys$y7)
        my_estimates_SI_8_sys <- rbind(my_estimates_SI_8_sys, my_est_all_cond_SI_sys$y8)
        
        
        
        elapsed <- Sys.time() - start_SI
        avg_time <- as.numeric(elapsed, units = "secs") / j
        est_remaining <- (iter - j) * avg_time
        
        message(sprintf("[SI] Iteration %d/%d | Elapsed: %.2f sec | Est. remaining: %.2f sec", 
                        j, iter, as.numeric(Sys.time() - iter_start, units = "secs"), est_remaining))
    }
    
    message("Starting SI_cov data simulations...")
    start_SI_cov <- Sys.time()
    for (k in 1:iter) {
        iter_start <- Sys.time()
        
        data_SI_cov <- data_generator_SI_cov(N, times, rho_true, beta_true, theta_true, xi_true,
                                             beta_confounder, effetto, statistic,
                                             sigma_alfa,sigma_alfa_t, sigma_y ,sigma_D ,  burn_in, pre_treatment)
        
        # stime non pesate
        est_all_cond_SI_cov <- estimation_all_conditional(data_SI_cov,  transformation="d")
        est_all_cond_SI_cov_sys <- estimation_all_conditional(data_SI_cov,  transformation="ld")
        # stime  pesate
        my_est_all_cond_SI_cov <- my_estimation_all_conditional(data_SI_cov,  transformation="d")
        my_est_all_cond_SI_cov_sys <- my_estimation_all_conditional(data_SI_cov,  transformation="ld")
        
        # stime difference gmm
        estimates_SI_cov_6 <- rbind(estimates_SI_cov_6, est_all_cond_SI_cov$y6)
        estimates_SI_cov_7 <- rbind(estimates_SI_cov_7, est_all_cond_SI_cov$y7)
        estimates_SI_cov_8 <- rbind(estimates_SI_cov_8, est_all_cond_SI_cov$y8)
        
        my_estimates_SI_cov_6 <- rbind(my_estimates_SI_cov_6, my_est_all_cond_SI_cov$y6)
        my_estimates_SI_cov_7 <- rbind(my_estimates_SI_cov_7, my_est_all_cond_SI_cov$y7)
        my_estimates_SI_cov_8 <- rbind(my_estimates_SI_cov_8, my_est_all_cond_SI_cov$y8)
        
        # stime SYS gmm
        estimates_SI_cov_6_sys <- rbind(estimates_SI_cov_6_sys, est_all_cond_SI_cov_sys$y6)
        estimates_SI_cov_7_sys <- rbind(estimates_SI_cov_7_sys, est_all_cond_SI_cov_sys$y7)
        estimates_SI_cov_8_sys <- rbind(estimates_SI_cov_8_sys, est_all_cond_SI_cov_sys$y8)
        
        my_estimates_SI_cov_6_sys <- rbind(my_estimates_SI_cov_6_sys, my_est_all_cond_SI_cov_sys$y6)
        my_estimates_SI_cov_7_sys <- rbind(my_estimates_SI_cov_7_sys, my_est_all_cond_SI_cov_sys$y7)
        my_estimates_SI_cov_8_sys <- rbind(my_estimates_SI_cov_8_sys, my_est_all_cond_SI_cov_sys$y8)
        
        
        elapsed <- Sys.time() - start_SI_cov
        avg_time <- as.numeric(elapsed, units = "secs") / k
        est_remaining <- (iter - k) * avg_time
        
        message(sprintf("[SI_cov] Iteration %d/%d | Elapsed: %.2f sec | Est. remaining: %.2f sec", 
                        k, iter, as.numeric(Sys.time() - iter_start, units = "secs"), est_remaining))
    }
    
    total_elapsed <- Sys.time() - total_start
    message(sprintf("All simulations completed in %.2f seconds.", as.numeric(total_elapsed, units = "secs")))
    
    
    return(list(
        estimates_6 = list(rand = estimates_rand_6, SI = estimates_SI_6, SI_cov = estimates_SI_cov_6,
                           my_rand = my_estimates_rand_6, my_SI = my_estimates_SI_6, my_SI_cov = my_estimates_SI_cov_6,
                           rand_sys = estimates_rand_6_sys, SI_sys = estimates_SI_6_sys, SI_cov_sys = estimates_SI_cov_6_sys,
                           my_rand_sys = my_estimates_rand_6_sys, my_SI_sys = my_estimates_SI_6_sys, my_SI_cov_sys = my_estimates_SI_cov_6_sys),
        
        estimates_7 = list(rand = estimates_rand_7, SI = estimates_SI_7, SI_cov = estimates_SI_cov_7,
                           my_rand = my_estimates_rand_7, my_SI = my_estimates_SI_7, my_SI_cov = my_estimates_SI_cov_7,
                           rand_sys = estimates_rand_7_sys, SI_sys = estimates_SI_7_sys, SI_cov_sys = estimates_SI_cov_7_sys,
                           my_rand_sys = my_estimates_rand_7_sys, my_SI_sys = my_estimates_SI_7_sys, my_SI_cov_sys = my_estimates_SI_cov_7_sys),
       
         estimates_8 = list(rand = estimates_rand_8, SI = estimates_SI_8, SI_cov = estimates_SI_cov_8,
                           my_rand = my_estimates_rand_8, my_SI = my_estimates_SI_8, my_SI_cov = my_estimates_SI_cov_8,
                           
                           rand_sys = estimates_rand_8_sys, SI_sys = estimates_SI_8_sys, SI_cov_sys = estimates_SI_cov_8_sys,
                           my_rand_sys = my_estimates_rand_8_sys, my_SI_sys = my_estimates_SI_8_sys, my_SI_cov_sys = my_estimates_SI_cov_8_sys)
        
    ))
    
    # return(list(
    #     estimates_6 = list(rand = estimates_rand_6, SI = estimates_SI_6, SI_cov = estimates_SI_cov_6,
    #                        my_rand = my_estimates_rand_6, my_SI = my_estimates_SI_6, my_SI_cov = my_estimates_SI_cov_6),
    #     estimates_7 = list(rand = estimates_rand_7, SI = estimates_SI_7, SI_cov = estimates_SI_cov_7,
    #                        my_rand = my_estimates_rand_7, my_SI = my_estimates_SI_7, my_SI_cov = my_estimates_SI_cov_7),
    #     estimates_8 = list(rand = estimates_rand_8, SI = estimates_SI_8, SI_cov = estimates_SI_cov_8,
    #                        my_rand = my_estimates_rand_8, my_SI = my_estimates_SI_8, my_SI_cov = my_estimates_SI_cov_8)
    #     
    # ))
    
    
    
}



# data_test<-get_data(model,
#                     data = data_SI,
#                     time = "time_id",
#                     group = "individual_id")
# 
# code<-get_code(model,
#                data = data_SI,
#                time = "time_id",
#                group = "individual_id"
# )

# # return(list(model=code,data_test=data_test ))
# model <- rstan::stan_model(model_code =  code)
# 
# test<-rstan::vb(model, data_test)
# 
# fit$stanfit <-test
# return(test)

