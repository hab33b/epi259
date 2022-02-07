# Breslow-Day test function ----
# source: http://www.math.montana.edu/~jimrc/classes/stat524/Rcode/breslowday.test.r

breslowday.test = function(x, OR=NA, printORi.s=TRUE){
  ## function to compute the Breslow Day test of homogeneity for a 2 by 2 by k table
  ## x is a three dim array, 2x2xk
  ## tests to see if all strata have the same OR
  ## if OR is not given, the Mantel-Haenszel estimate is used.
  if(is.na(OR)) {
    OR = mantelhaen.test(x)$estimate
    names(OR) = ""
  } 
  OR.i <- apply(x, 3,  function(x) x[1,1] * x[2,2] / x[1,2] /x[2,1])
  k = dim(x)[3]
  n11k = x[1,1,]
  n21k = x[2,1,]
  n12k = x[1,2,]
  n22k = x[2,2,]
  row1sums = n11k + n12k
  row2sums = n21k + n22k
  col1sums = n11k + n21k
  Amax = apply(cbind(row1sums,col1sums),1,min)
  ## Astar must be no more than col1sums and no more than row1sums
  bb = row2sums +row1sums * OR - col1sums*(1-OR)
  determ = sqrt(bb^2 + 4 * (1-OR) *  OR * row1sums * col1sums)
  Astar = (-bb + cbind( -determ, determ))/ (2 -2*OR)
  Astar = ifelse(Astar[,1] <= Amax & Astar[,1] >= 0, Astar[,1], Astar[,2])
  ## print(Astar)
  Bstar = row1sums - Astar
  Cstar = col1sums - Astar
  Dstar = row2sums - col1sums + Astar
  Var = apply(1 / (.5+cbind(Astar,Bstar,Cstar,Dstar)), 1, sum)^(-1)
  ## print(Var)
  X2 = sum( (x[1,1,] - Astar)^2/Var )
  pvalue = 1 - pchisq(X2,k-1)
  if(printORi.s) {
    out <- rbind(log(OR.i), 1/Var)
    dimnames(out)[[1]] <- c("log OR","Weight")
    print(out)
  }
  return(unlist(list(OR = OR, Stat = X2, df = k-1, pvalue = pvalue)))
}


## Usage:
##  biomassTB.Salary = array(c(38,12,102,141,12,9,136,383),dim=c(2,2,2),dimnames=list(exposure=c("exposed","not"), disease=c("case","control"),salary=c("<1000",">=1000")))
##  ORconfInterval( biomassTB.Salary)
##
## Data:  biomassTB.Salary 
## method             lower common odds ratio    upper confidence
##  Woolf           2.351913          4.028269 6.899469       0.95
##  Mantel-Haenszel 2.401977          4.158475 7.199451       0.95

##  breslowday.test( biomassTB.Salary, 4.02)
##           <1000    >=1000
## log OR 1.4764666 1.3230622
## Weight 0.1190846 0.1969553
##         OR       Stat         df     pvalue 
## 4.15000000 0.07150466 1.00000000 0.78915846 
# H0: OR between each strata is the same (assumption of homogeneity)