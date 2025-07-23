effects<-function(rho_true, effetto){
    # 6
    eff_y6<-effetto
    # 7
    eff_y7_11_00<-(effetto*rho_true)+effetto+effetto
    eff_y7_10_00<-(effetto*rho_true)+effetto
    eff_y7_01_00<-effetto
    eff_y7_11_01<-(effetto*rho_true)+effetto
    eff_y7_11_10<-effetto
    eff_y7_10_01<-(effetto*rho_true)+effetto-effetto
    # 8
    eff_y8_111_000<-(effetto*rho_true*rho_true)+rho_true*effetto+rho_true*effetto+effetto+effetto
    eff_y8_110_000<-(effetto*rho_true*rho_true)+rho_true*effetto+rho_true*effetto+effetto
    eff_y8_101_000 <- (effetto * rho_true * rho_true) + (rho_true * effetto) + effetto
    eff_y8_100_000 <- (effetto * rho_true * rho_true) + (rho_true * effetto)
    eff_y8_011_000 <- (rho_true * effetto) + effetto + effetto
    eff_y8_010_000 <- (rho_true * effetto) + effetto
    eff_y8_001_000 <- effetto
    
    effects_8<-c(eff_y8_111_000,eff_y8_110_000, eff_y8_101_000, eff_y8_100_000, eff_y8_011_000,
                eff_y8_010_000,eff_y8_001_000)
    return(list(effects_6=eff_y6,
                effects_7=c(eff_y7_11_00, eff_y7_10_00,
                            eff_y7_01_00, eff_y7_11_01,
                            eff_y7_11_10, eff_y7_10_01),
                effects_8=effects_8
                ))
}