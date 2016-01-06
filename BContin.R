contin <- function (a,b,c,d) {
# a= True Positive
# b= False Positive
# c= False Negative
# d= True Negative
# This function assumes that the test result is on the left with the Positive result on top, and the Condition is on the Top, with the positive condition on the left.
#            D+  | D-
#        T+  a   |  b   |      
#        ----------------
#       T-   c   |  d   | 
       
  sensitivity <- (a/(a+c))
  specificity <- (d/(b+d))
  prevalence <- ((a+c)/(a+b+c+d))
  PPV <- (sensitivity*prevalence)/((sensitivity*prevalence)+((1-specificity)*(1-prevalence)))
  NPV <- ((d)/(c+d))
  DOR <- ((a/c)/(b/d))
  DLRPos <-(sensitivity/(1-specificity))
  DLRNeg <- (1-sensitivity)/specificity        
  names <- c("Sensitivity = ","Specificity = ","Prevalence = ", "PPV = ", "NPV = ", "Diagnostic Odds Ratio = ","DLRPos = ","DLRNeg = ")
  results <- c(sensitivity,specificity,prevalence,PPV,NPV,DOR,DLRPos,DLRNeg)
  table <- data.frame(names,results)
return(table)
}
