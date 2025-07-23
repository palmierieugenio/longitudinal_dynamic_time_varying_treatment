somma_treatment<-function(w, effect){
    if(is.vector(w)){
        return(w*effect)
    }
    rowSums(w[,(ncol(w)-1):ncol(w)])*effect
}