# MULTIVARIATE ANALYSIS FUNCTIONS
# used for disc-analysis.R

sqrtmat <- function(x)
{
  temp <- eigen(x, symmetric = T)
  if (length(temp$values)>1) outval <- temp$vectors %*% diag(sqrt(temp$values)) %*% t(temp$vectors)
  if (length(temp$values)==1) outval <- matrix(sqrt(temp$values),1,1)
  outval
}


mymanova <- function(x,grp, extract = 'mva')  
{
  p <- ncol(x)
  n <- nrow(x)
  grpnums <- sort(unique(grp))
  k <- length(grpnums)
  ni <- table(grp)
  xbar <- matrix(NA,k,p)
  xbarall <- apply(x,2,mean)
  for (i in 1:k) xbar[i,] <- apply(matrix(x[grp==grpnums[i],],sum(grp==grpnums[i]),p)
                                   ,2,mean)
  
  E <- matrix(0,p,p)
  for (i in 1:n) E <- E + c(x[i,]-xbar[grp[i]==grpnums,]) %*% 
    t(c(x[i,]-xbar[grp[i]==grpnums,]))
  # E <- diag(diag(E))   # for when nuE < p and you're desperate
  nuE <- n - k
  
  H <- matrix(0,p,p)
  for (i in 1:k) H <- H + ni[i]* c(xbar[i,]-xbarall) %*% 
    t(c(xbar[i,]-xbarall))
  nuH <- min(k-1,p)
  
  if (extract == "mva") {
    Lambda <- prod(1/(1+eigen(solve(sqrtmat(E)) %*% H %*% solve(sqrtmat(E)))$values))
    Lambda2F(Lambda,p,nuH,nuE)
  }
  
  else if (extract == "EH") {
    list(E = E, H = H)
  }
}



Lambda2F <- function(Lambda,dim,nuH,nuE,alpha=.05)
{
  df1 <- dim*nuH
  w <- nuH + nuE - .5*(dim+nuH+1)
  t <- 1
  if((dim^2 + nuH^2 - 5) > 0)
    t <- sqrt((dim^2 * nuH^2 - 4)/(dim^2 + nuH^2 - 5))
  df2 <- w*t - .5*(dim*nuH - 2)
  Fstat <- (1-Lambda^(1/t))/(Lambda^(1/t)) * df2/df1
  critval <- qf(1-alpha,df1,df2)
  pval <- 1 - pf(Fstat,df1,df2)
  return(list(Lambda=Lambda,w=w,t=t,df1=df1,df2=df2,Fstat=Fstat,critval=critval,pval=pval))
}

