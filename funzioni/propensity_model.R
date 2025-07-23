

reshape_to_wide <- function(data_long) {
    # Ensure numeric time_id (just in case)
    data_long$time_id <- as.integer(data_long$time_id)
    
    # Pivot wider for each variable
    data_wide <- data_long %>%
        dplyr::select(individual_id, time_id, y, X, D, D_cum) %>%
        pivot_wider(
            names_from = time_id,
            values_from = c(y, X, D, D_cum),
            names_glue = "{.value}_{time_id}"
        )
    data_wide<-as.data.frame(data_wide)
    return(data_wide)
}

estimate_weights<-function(data_long){
    
data_wide<-reshape_to_wide(data_long)

# treatment_model1 <- glm(
#     D_6 ~ y_1+y_2+y_3+y_4+y_5+X_6,
#     data = data_wide,
#     family = binomial(link = "logit")
# )
# treatment_model2 <- glm(
#     D_7 ~ y_1+y_2+y_3+y_4+y_5+X_6+ D_6,
#     data = data_wide,
#     family = binomial(link = "logit")
# )
# 
# treatment_model3 <- glm(
#     D_8 ~ y_1+y_2+y_3+y_4+y_5+X_6+ D_6+D_7,
#     data = data_wide,
#     family = binomial(link = "logit")
# )


# predict propensity scores
# data_wide$p1<-predict(treatment_model1, type="response")
# data_wide$p2<-predict(treatment_model2, type="response")
# data_wide$p3<-predict(treatment_model3, type="response")

data_wide$w1<-weightitMSM(list( D_6 ~ y_1+y_2+y_3+y_4+y_5+X_6),
                          data = data_wide,
                          method = "glm")$weights
                          
data_wide$w2<-weightitMSM(list( D_6 ~ y_1+y_2+y_3+y_4+y_5+X_6,
                               D_7 ~ y_1+y_2+y_3+y_4+y_5+y_6+ +X_6+X_7+ D_6),
                           data = data_wide,
                           stabilize = TRUE,
                          method = "glm")$weights                          
                          

data_wide$w3<-weightitMSM(list( D_6 ~ y_1+y_2+y_3+y_4+y_5+X_6,
                  D_7 ~ y_1+y_2+y_3+y_4+y_5+y_6+X_6+X_7+ D_6,
            D_8 ~ y_1+y_2+y_3+y_4+y_5+y_6+y_7+X_6+X_7+X_8+ D_6+D_7),
            data = data_wide,
            stabilize = TRUE,
            method = "glm")$weights

return(data_wide)
}

