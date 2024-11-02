## Function to add the * for significance when given the coefficient matrix (coefs) 
  ## and the corresponding p-values (pvalues)
star.pvalues <- function(pvalues, coefs){
  original <- coefs %>% as.data.frame() #store the data in case the
  
  coefs <- coefs %>% as.data.frame()
  coefs <- round(coefs, digits = 2) #round to 2 digits
  coefs[] <- lapply(coefs, function(x) as.character(as.numeric(x)))
  coefs[] <- lapply(coefs, function(x) str_remove(x, "0")) #remove the first 0 (before .)
  
  pvalues <- pvalues %>% as.data.frame()
  pvalues[is.na(pvalues)] <- 10 #avoid having to deal with NA 
  
  #pvalue <0.05 *, pvalue <0.01 **, pvalue < 0.001 ***
  for (i in 1:nrow(coefs)) {
    for (j in c(1:ncol(coefs))) {
      if (pvalues[i,j] < 0.001){
        coefs[i,j] <- paste0(coefs[i,j],"***")
      }else{if (pvalues[i,j] < 0.01){
        coefs[i,j] <- paste0(coefs[i,j],"**")
      }else{if (pvalues[i,j] < 0.05){
        coefs[i,j] <- paste0(coefs[i,j],"*")}
      }
      }
    }
  }
  
  # Issue: if a coefficient is below 0.005, it will round down to 0 and be an empty vector now
  # Go back to the original dataset and round with 3 digits
  coefs[coefs == ""] <- as.character(round(original[coefs == ""], 3)) %>% str_remove("0")
  
  
  return(coefs)
}


# df for working dataframe
# resDF for the formatting of the results dataframe
# pValueDF for the formatting of the p-value dataframe
# DV dependent variable
# IV independent variable
# ModelNum: number of the model, can be useful to keep track how many models we're running and avoid doublets
modelLinReg <- function(df, resDF, DV, IV, ModelNum) {
  temp <- df %>% dplyr::select(DV, IV)
  
  #linear regression and getting beta coefs with p-values
  reg <- lm.beta(lm(temp))
  tmp <- star.pvalues(summary(reg)$coefficients[,5], 
                      summary(reg)$coefficients[,2])
  tmp <- tmp %>% rownames_to_column()
  
  # R squared
  tmp[nrow(tmp)+1,1] <- "R squared"
  tmp[nrow(tmp), 2] <- round(summary(reg)$r.squared,3)
  tmp[nrow(tmp)+1,1] <- "Adjusted R squared"
  tmp[nrow(tmp), 2] <- round(summary(reg)$adj.r.squared,3)
  
  #Ajouter le nombre d'observations utilisées
  tmp[nrow(tmp)+1,1] <- "N"
  tmp[nrow(tmp), 2] <- round(length(reg$residuals),3)
  
  
  resDF <- resDF %>% left_join(tmp, by = "rowname")
  resDF[1,ncol(resDF)] <- paste0(DV, ".", ModelNum)
  
  P <- summary(reg)$coefficients[,5] %>% t() %>% t() %>% as.data.frame() %>% rownames_to_column()
  
  return(resDF)
}
