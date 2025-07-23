estimation_all_conditional <- function(datu, start=5, transformation) {
    data_1<-datu[datu$time_id<=start+1, ]
 
    # fit1 <- my_pgmm2(y ~ lag(y, 1)+X +D  | lag(y, 2:99)  , #lag(X)
    #                  data = data_1, index = c("individual_id", "time_id"), 
    #                  effect = "twoway", model = "twostep", time.dummies=T,
    #                  transformation ="ld"
    # )
    # return(fit1)
    eff_y6<- tryCatch({
        fit1 <- pgmm(y ~ lag(y, 1)+X +D  | lag(y, 2:99)  , #lag(X)
                     data = data_1, index = c("individual_id", "time_id"), 
                     effect = "twoway", model = "twostep", time.dummies=T,
                     # transformation ="ld"
                     transformation =transformation
        )
        t11<-coef(fit1)["D"]
        eff_y6<-t11
        eff_y6
    }, error = function(e) {
        NA # generate NA row of same length
    })
    

    
    data_2<-datu[datu$time_id<=start+2, ]
    eff_y7<- tryCatch({
        
    fit2 <- pgmm(y ~ lag(y, 1)+X +D+lag(D)  | lag(y, 2:99)  ,
                 data = data_2, index = c("individual_id", "time_id"), 
                 effect = "twoway", model = "twostep", time.dummies=T,
                  transformation  =transformation
                 # transformation ="ld"
    )
    t12<-coef(fit2)["lag(D)"]
    t22<-coef(fit2)["D"]
    rho2<-coef(fit2)["lag(y, 1)"]
    eff_y7_11_00<-(t11*rho2)+t12+t22
    eff_y7_10_00<-(t11*rho2)+t12
    eff_y7_01_00<-t22
    eff_y7_11_01<-(t11*rho2)+t12
    eff_y7_11_10<-t22
    eff_y7_10_01<-(t11*rho2)+t12-t22
    eff_y7<-c(eff_y7_11_00,eff_y7_10_00, eff_y7_01_00, eff_y7_11_01,
              eff_y7_11_10, eff_y7_10_01)
    eff_y7
    }, error = function(e) {
        rep(NA, 6) # generate NA row of same length
    })
    
    data_3<-datu[datu$time_id<=start+3, ]
    eff_y8<- tryCatch({
    fit3 <- pgmm(y ~ lag(y, 1)+X +D+lag(D)  | lag(y, 2:99) ,
                 data = data_3, index = c("individual_id", "time_id"),
                 effect = "twoway", model = "twostep", time.dummies=T,
                 # transformation ="ld",
                 transformation  =transformation
    )
    t23<-coef(fit3)["lag(D)"]
    t33<-coef(fit3)["D"]
    rho3<-coef(fit3)["lag(y, 1)"]
    eff_y8_111_000<-(t11*rho2*rho3)+rho3*t12+rho3*t22+t23+t33
    eff_y8_110_000<-(t11*rho2*rho3)+rho3*t12+rho3*t22+t23
    eff_y8_101_000 <- (t11 * rho2 * rho3) + (rho3 * t12) + t33
    eff_y8_100_000 <- (t11 * rho2 * rho3) + (rho3 * t12)
    eff_y8_011_000 <- (rho3 * t22) + t23 + t33
    eff_y8_010_000 <- (rho3 * t22) + t23
    eff_y8_001_000 <- t33
    eff_y8<-c(eff_y8_111_000,eff_y8_110_000, eff_y8_101_000, eff_y8_100_000, eff_y8_011_000,
              eff_y8_010_000,eff_y8_001_000)
    eff_y8
    }, error = function(e) {
        rep(NA, 7) # generate NA row of same length
    })
    
    
   
    
    effects<-list(y6=eff_y6, y7=eff_y7, y8=eff_y8)
    names(effects$y6)<-c("1-0")
    names(effects$y7)<-c("11-00", "10-00", "01-00", "11-01", "11-10", "10-01")
    names(effects$y8)<-c("111-000", "110-000", "101-000","100-000", "011-000",
                         "010-000", "001-000")
    return(effects)
    
}