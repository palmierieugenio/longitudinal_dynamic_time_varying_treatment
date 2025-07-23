estimation_all <- function(datu, start=5) {
    data_1<-datu[datu$time_id<=start+1 , ]
    ids1<-data_1$individual_id[ data_1$treatment_history=="1"]
    data_1<-data_1[data_1$individual_id%in%ids1, ]
    fit1 <- pgmm(y ~ lag(y, 1)+lag(X)    | lag(y, 2:99),
                 data = data_1, index = c("individual_id", "time_id"), 
                 effect = "twoway", model = "twostep", time.dummies=T
    )
    
    data_2<-datu[datu$time_id<=start+2 , ]
    ids2<-data_2$individual_id[ data_2$treatment_history=="11"]
    data_2<-data_2[data_2$individual_id%in%ids2, ]
    
    fit2 <- pgmm(y ~ lag(y, 1)+lag(X)    | lag(y, 2:99),
                 data = data_2, index = c("individual_id", "time_id"), 
                 effect = "twoway", model = "twostep", time.dummies=T
    )
    
    data_3<-datu[datu$time_id<=start+3 , ]
    ids3<-data_3$individual_id[ data_3$treatment_history=="111"]
    data_3<-data_3[data_3$individual_id%in%ids3, ]
    fit3 <- pgmm(y ~ lag(y, 1)+lag(X)    | lag(y, 2:99),
                 data = data_3, index = c("individual_id", "time_id"), 
                 effect = "twoway", model = "twostep", time.dummies=T
    )
    fits<-list(fit1=fit1, fit2=fit2, fit3=fit3)
    return(fits)
    
}