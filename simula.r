library(plm)
library(dplyr)
library(reshape2)
library(tidyr)
library(WeightIt)
library(Formula)
library(MASS)
# library(bdsmatrix)# library(collapse)# library(zoo)# library(nlme)# library(sandwich)
# library(lattice)# library(lmtest)# library(maxLik)# library(Rdpack)
# library(stats)

funzioni<-list.files(path = "funzioni", full.names = TRUE)
lapply(funzioni, source)
funzioni_pacchetto<-list.files(path = "funzioni_pacchetto", full.names = TRUE)
lapply(funzioni_pacchetto, source)
# Parameters
iter<-100
N <- 200
times <- 5
pre_treatment<-5
# rho_true <- 0.3
rho_true <- 0
beta_true <- 0.5
beta_confounder<- 0
theta_true<-c(-1, 0.03, 0.1, 0.25) # theta 1 cost, theta 2 x, theta 3 y-1, theta 4 w-1
# theta_true<-c(-1, 0.05, 0.25, 0) 
# theta_true<-c(-1, 0.05, 0, 2) 
names(theta_true)<-c("cost", "x", "w", "y")
xi_true<-c(0.3, 0.05) # xi x-1, xi w-1
scala<-5
effetto <-1*scala
burn_in<-100
effetti_veri<-effects(rho_true, effetto)
sigma_alfa<-2 # sembra aumentare il bias
sigma_alfa_t<-0  # non sembra influire sul bias
sigma_y<-2# non sembra influire sul bias
sigma_D <-1

# Generate Full Data with Burn-in Period
# data_SI <- data_generator_SI(500, times, rho_true, beta_true, theta_true, beta_confounder, effetto,somma_treatment,
#                               sigma_alfa,sigma_alfa_t, sigma_y ,sigma_D ,  burn_in, pre_treatment)

# propensities<-estimate_propensity_score(data_SI)
# data_random <- data_generator_random(5000, times, rho_true, beta_true, beta_confounder, effetto,somma_treatment,
#                              sigma_alfa,sigma_alfa_t, sigma_y ,sigma_D ,  burn_in, pre_treatment)

# data_1<-data_SI[data_SI$time_id<=5+2, ]
# # 
# fit3_diff <- my_pgmm2(y ~ lag(y, 1)+X +D+lag(D) -1 | lag(y, 2:99),
#              data = data_SI, index = c("individual_id", "time_id"),
#              effect = "twoway", model = "twostep", time.dummies=T,
#              transformation ="d"
# )
# 
# fit3_sys <- pgmm(y ~ lag(y, 1)+X +D+lag(D) -1 | lag(y, 2:99),
#              data = data_1, index = c("individual_id", "time_id"),
#              effect = "twoway", model = "twostep", time.dummies=T,
#              transformation ="ld"
# )
# 
# fit3_w_diff <- my_pgmm(y ~ lag(y, 1)+X +D+lag(D) -1 | lag(y, 2:99),
#              data = data_1, index = c("individual_id", "time_id"),
#              effect = "twoway", model = "twostep", time.dummies=T,
#              transformation ="d", pesi=1/(propensities$p1*propensities$p2)
# )
# 
# 
# rbind(coef(fit3_diff),
#      coef(fit3_sys)[names(coef(fit3_sys))!="(Intercept)"],
#      coef(fit3_w_diff))



# summary(fit3)
# estimate all conditional
# est_all_cond_rand<-estimation_all_conditional(data_random)
# est_all_cond_SI<-estimation_all_conditional(data_SI)
# repeat simulation

res<-itera(iter, N, times, rho_true, beta_true, theta_true, xi_true, beta_confounder,
           effetto, burn_in, sigma_alfa, sigma_alfa_t, sigma_y, sigma_D,
           pre_treatment,statistic=somma_treatment)
# saveRDS(res, file="simula1.Rdata")
# res<-readRDS("simula1.Rdata")
# saveRDS(res, file="simula2.Rdata")
# simula2.Rdata
# iter<-200
# N <- 1000
# times <- 5
# pre_treatment<-5
# rho_true <- 0.3
# beta_true <- 0.08
# beta_confounder<- 0
# theta_true<-c(-1, 0.1, 1.5, 0.3)
# xi_true<-c(0.8, 2)
# scala<-100
# effetto <-1*scala
# burn_in<-30
# effetti_veri<-effects(rho_true, effetto)
# sigma_alfa<-10
# sigma_alfa_t<-10
# sigma_y<-10
# sigma_D <-1




# plot 7
# DIFF-GMM
 est7_rand<-data.frame("rand", "standard", "diff-GMM", melt(res$estimates_7$rand))
 est7_SI<-data.frame("SI", "standard","diff-GMM", melt(res$estimates_7$SI))
 est7_SI_cov<-data.frame("SI_COV","standard","diff-GMM", melt(res$estimates_7$SI_cov))
 my_est7_rand<-data.frame("rand", "weighted","diff-GMM", melt(res$estimates_7$my_rand))
 my_est7_SI<-data.frame("SI", "weighted","diff-GMM", melt(res$estimates_7$my_SI))
 my_est7_SI_cov<-data.frame("SI_COV","weighted","diff-GMM", melt(res$estimates_7$my_SI_cov))
 # SYS-GMM
 est7_rand_sys<-data.frame("rand", "standard", "sys-GMM", melt(res$estimates_7$rand_sys))
 est7_SI_sys<-data.frame("SI", "standard","sys-GMM", melt(res$estimates_7$SI_sys))
 est7_SI_cov_sys<-data.frame("SI_COV","standard","sys-GMM", melt(res$estimates_7$SI_cov_sys))
 my_est7_rand_sys<-data.frame("rand", "weighted","sys-GMM", melt(res$estimates_7$my_rand_sys))
 my_est7_SI_sys<-data.frame("SI", "weighted","sys-GMM", melt(res$estimates_7$my_SI_sys))
 my_est7_SI_cov_sys<-data.frame("SI_COV","weighted","sys-GMM", melt(res$estimates_7$my_SI_cov_sys))
 
 
colnames( my_est7_rand)<- colnames( my_est7_SI)<- colnames( my_est7_SI_cov)<-colnames(est7_rand)<- colnames(est7_SI)<- colnames(est7_SI_cov)<-c("process", "weighting", "estimation", "iter","contrast","value")
colnames( my_est7_rand_sys)<- colnames( my_est7_SI_sys)<- colnames( my_est7_SI_cov_sys)<-colnames(est7_rand_sys)<- colnames(est7_SI_sys)<- colnames(est7_SI_cov_sys)<-c("process", "weighting", "estimation", "iter","contrast","value")

res_plot<-rbind(est7_rand,
                est7_SI,
                est7_SI_cov,
                my_est7_rand, my_est7_SI, my_est7_SI_cov,
                
                est7_rand_sys,
                est7_SI_sys,
                est7_SI_cov_sys,
                my_est7_rand_sys, my_est7_SI_sys, my_est7_SI_cov_sys)

# Plot density
library(ggplot2)
# true values
vline_pos <- c("11-00" = effetti_veri$effects_7[1], "10-00" = effetti_veri$effects_7[2],
               "01-00" =  effetti_veri$effects_7[3],
               "11-01" = effetti_veri$effects_7[4], "11-10" = effetti_veri$effects_7[5],
               "10-01"=effetti_veri$effects_7[6])
# Convert named vector to data frame
vline_df <- data.frame(
    contrast = names(vline_pos),
    vline = as.numeric(vline_pos)
)

# Plot with vertical lines
p_standard<-res_plot[res_plot$weighting=="standard",]
p_weighted<-res_plot[res_plot$weighting=="weighted",]
plot_standard<- ggplot(p_standard, aes(x = value, color = process)) +
    geom_boxplot(outlier.shape = NA) +
    # facet_wrap(~contrast+estimation , scales = "free") +
    facet_grid(  contrast ~ estimation, scales = "fixed") +
    geom_vline(data = vline_df, aes(xintercept = vline), color = "black", linetype = "solid") +
    theme_minimal()+coord_flip()+
    coord_cartesian(xlim = quantile(p_standard$value, c(0.01, 0.99), na.rm=T))+
    theme(strip.text.y.right = element_text(angle = 0))

plot_weighted<- ggplot(p_weighted, aes(x = value, color = process)) +
    geom_boxplot(outlier.shape = NA) +
    # facet_wrap(~contrast+estimation , scales = "free") +
    facet_grid(  contrast ~ estimation, scales = "fixed") +
     geom_vline(data = vline_df, aes(xintercept = vline), color = "black", linetype = "solid") +
    theme_minimal()+coord_flip()+
    coord_cartesian(xlim = quantile(p_weighted$value, c(0.01, 0.99), na.rm=T))+
    theme(strip.text.y.right = element_text(angle = 0))
library(cowplot)
plot_grid(plot_standard, plot_weighted
          , labels=c("standard", "weighted"),
          ncol = 1, nrow = 2)


# ggplot(res_plot, aes(x = value, color = process)) +
#     geom_density() +
#     facet_wrap(~contrast, scales = "free") +
#     geom_vline(data = vline_df, aes(xintercept = vline), color = "black", linetype = "dashed") +
#     theme_minimal()


# plot 8
# DIFF-GMM
est8_rand<-data.frame("rand", "standard", "diff-GMM", melt(res$estimates_8$rand))
est8_SI<-data.frame("SI", "standard","diff-GMM", melt(res$estimates_8$SI))
est8_SI_cov<-data.frame("SI_COV", "standard","diff-GMM", melt(res$estimates_8$SI_cov))

my_est8_rand<-data.frame("rand", "weighted","diff-GMM", melt(res$estimates_8$my_rand))
my_est8_SI<-data.frame("SI", "weighted","diff-GMM", melt(res$estimates_8$my_SI))
my_est8_SI_cov<-data.frame("SI_COV", "weighted","diff-GMM", melt(res$estimates_8$my_SI_cov))
# SYS-GMM
est8_rand_sys<-data.frame("rand", "standard","sys-GMM", melt(res$estimates_8$rand_sys))
est8_SI_sys<-data.frame("SI", "standard","sys-GMM", melt(res$estimates_8$SI_sys))
est8_SI_cov_sys<-data.frame("SI_COV", "standard","sys-GMM", melt(res$estimates_8$SI_cov_sys))

my_est8_rand_sys<-data.frame("rand", "weighted","sys-GMM", melt(res$estimates_8$my_rand_sys))
my_est8_SI_sys<-data.frame("SI", "weighted","sys-GMM", melt(res$estimates_8$my_SI_sys))
my_est8_SI_cov_sys<-data.frame("SI_COV", "weighted","sys-GMM", melt(res$estimates_8$my_SI_cov_sys))



colnames(my_est8_rand)<- colnames(my_est8_SI)<- colnames(my_est8_SI_cov)<-colnames(est8_rand)<- colnames(est8_SI)<- colnames(est8_SI_cov)<-c("process","weighting", "estimation", "iter","contrast","value")
colnames(my_est8_rand_sys)<- colnames(my_est8_SI_sys)<- colnames(my_est8_SI_cov_sys)<-colnames(est8_rand_sys)<- colnames(est8_SI_sys)<- colnames(est8_SI_cov_sys)<-c("process","weighting", "estimation", "iter","contrast","value")


res_plot<-rbind(est8_rand,
                est8_SI,
                est8_SI_cov,
                my_est8_rand,
                my_est8_SI,
                my_est8_SI_cov,
                est8_rand_sys,
                est8_SI_sys,
                est8_SI_cov_sys,
                my_est8_rand_sys,
                my_est8_SI_sys,
                my_est8_SI_cov_sys)

# Plot density
library(ggplot2)
# true values
vline_pos <- c("111-000" = effetti_veri$effects_8[1], "110-000" = effetti_veri$effects_8[2],
               "101-000"=effetti_veri$effects_8[3], "100-000" =effetti_veri$effects_8[4],
               "011-000"=effetti_veri$effects_8[5],"010-000"=effetti_veri$effects_8[6],
               "001-000"=effetti_veri$effects_8[7])
# Convert named vector to data frame
vline_df <- data.frame(
    contrast = names(vline_pos),
    vline = as.numeric(vline_pos)
)

# Plot with vertical lines
# 
# 
plotto_seq<-c("111-000" ) # , "100-000"
# plotto_seq<-unique(res_plot$contrast)[1:3]
ggplot( subset(res_plot, contrast%in% plotto_seq )  , aes(x = value, color = process, )) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(contrast+estimation~weighting, scales = "fixed") +
    geom_vline(data =subset(vline_df, contrast %in%  plotto_seq) , aes(xintercept = vline), color = "black", linetype = "dashed") +
    theme_minimal()+coord_flip()+
    coord_cartesian(xlim = quantile(res_plot$value, c(0.01, 0.99), na.rm=T))




# bias_rand<-t(apply(res$estimates_7$rand, 1, function(x)abs(x-effetti_veri$effects_7)))
# bias_SI<-t(apply(res$estimates_7$SI, 1, function(x)abs(x-effetti_veri$effects_7)))
# bias_SI_cov<-t(apply(res$estimates_7$SI_cov, 1, function(x)abs(x-effetti_veri$effects_7)))
# 
# bias_my_rand<-t(apply(res$estimates_7$my_rand, 1, function(x)abs(x-effetti_veri$effects_7)))
# bias_my_SI<-t(apply(res$estimates_7$my_SI, 1, function(x)abs(x-effetti_veri$effects_7)))
# bias_my_SI_cov<-t(apply(res$estimates_7$my_SI_cov, 1, function(x)abs(x-effetti_veri$effects_7)))
# 
# 
# 
# bias_rand8<-t(apply(res$estimates_8$rand, 1, function(x)abs(x-effetti_veri$effects_8)))
# bias_SI8<-t(apply(res$estimates_8$SI, 1, function(x)abs(x-effetti_veri$effects_8)))
# bias_SI_cov8<-t(apply(res$estimates_8$SI_cov, 1, function(x)abs(x-effetti_veri$effects_8)))
# 
# 
# 
# rbind(rand=colMeans(bias_rand, na.rm = T),
# SI=colMeans(bias_SI, na.rm = T),
# SI_cov=colMeans(bias_SI_cov, na.rm = T),
# my_rand=colMeans(bias_my_rand, na.rm = T),
# my_SI=colMeans(bias_my_SI, na.rm = T),
# my_SI_cov=colMeans(bias_my_SI_cov, na.rm = T))
# 
# rbind(rand=colMeans(bias_rand8, na.rm = T),
#       SI=colMeans(bias_SI8, na.rm = T),
#       SI_cov=colMeans(bias_SI_cov8, na.rm = T))
# 
# rbind(rand=colMeans(res$estimates_7$rand, na.rm = T),
# SI=colMeans(res$estimates_7$SI, na.rm = T),
# SI_cov=colMeans(res$estimates_7$SI_cov, na.rm = T))
# 
# rbind(rand=colMeans(res$estimates_8$rand, na.rm = T),
#       SI=colMeans(res$estimates_8$SI, na.rm = T),
#       SI_cov=colMeans(res$estimates_8$SI_cov, na.rm = T))


bias_decomp_rand<-bias_variance_decomposition(res$estimates_7$rand, effetti_veri$effects_7)
bias_decomp_SI<-bias_variance_decomposition(res$estimates_7$SI[complete.cases(res$estimates_7$SI),], effetti_veri$effects_7)
bias_decomp_SI_cov<-bias_variance_decomposition(res$estimates_7$SI_cov[complete.cases(res$estimates_7$SI_cov),], effetti_veri$effects_7)

bias_decomp_my_rand<-bias_variance_decomposition(res$estimates_7$my_rand, effetti_veri$effects_7)
bias_decomp_my_SI<-bias_variance_decomposition(res$estimates_7$my_SI[complete.cases(res$estimates_7$my_SI),], effetti_veri$effects_7)
bias_decomp_my_SI_cov<-bias_variance_decomposition(res$estimates_7$my_SI_cov[complete.cases(res$estimates_7$my_SI_cov),], effetti_veri$effects_7)


bias_decomp_rand8<-bias_variance_decomposition(res$estimates_8$rand, effetti_veri$effects_8)
bias_decomp_SI8<-bias_variance_decomposition(res$estimates_8$SI[complete.cases(res$estimates_8$SI),], effetti_veri$effects_8)
bias_decomp_SI_cov8<-bias_variance_decomposition(res$estimates_8$SI_cov[complete.cases(res$estimates_8$SI_cov),], effetti_veri$effects_8)

bias_decomp_my_rand8<-bias_variance_decomposition(res$estimates_8$my_rand, effetti_veri$effects_8)
bias_decomp_my_SI8<-bias_variance_decomposition(res$estimates_8$my_SI[complete.cases(res$estimates_8$my_SI),], effetti_veri$effects_8)
bias_decomp_my_SI_cov8<-bias_variance_decomposition(res$estimates_8$my_SI_cov[complete.cases(res$estimates_8$my_SI_cov),], effetti_veri$effects_8)






# ggplot(res_plot, aes(x = value, color = process)) +
#     geom_density() +
#     facet_wrap(~contrast, scales = "free") +
#     geom_vline(data = vline_df, aes(xintercept = vline), color = "black", linetype = "dashed") +
#     theme_minimal()

# library(pglm)
# 
# fit_treatment<-pglm(D ~ lag(y, 1)+ lag(y, 2)+ lag(y, 3)+X ,
#                     data = data_1, index = c("individual_id", "time_id"),
#                     family = binomial, model="random", effect="twoways",
#                     time.dummies=T)

# data_3<-data_random[data_random$time_id<=5+1, ]
# my_data_3<-data_3
# my_data_3<-plm::pdata.frame(my_data_3, index = c("individual_id", "time_id"))
# my_data_3$lag_D<-plm::lag(my_data_3$D, 1)
# my_data_3$lag_D[is.na(my_data_3$lag_D)]<-0
# # my_data_3<-my_data_3[my_data_3$time!=1,]
# # Rename to match GMM expectations
# my_data_3$id <- my_data_3$individual_id
# my_data_3$time <- my_data_3$time_id
# my_data_3$X1 <- my_data_3$X  # assuming only 1 covariate for now
# my_data_3 <- my_data_3[, c("id", "time", "y", "X1","D", "lag_D" )]
# 
# # Create instruments and differenced variables
# L <- create_L(my_data_3$y, my_data_3$id, my_data_3$time)
# diff_X <- create_diff_X(my_data_3, c("y", "X1", "D",  "lag_D"))
# diff_y <- create_diff_y(my_data_3)

# # Run GMM
# results <- one_step_gmm(L, diff_X, diff_y)
# print(results)
# print(results)
