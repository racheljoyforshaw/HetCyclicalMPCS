
#getranksdata_RF = function(y) {
  #betas = rq(y ~Xinstant , tau = c(1:19)/20)$coefficients # returns matrix with (intercept + Xinstant) rows and tau columns
   betas_pr = ivqr(dct ~ dyt | dytplus1 | 1, c(1:19)/20, grid = seq(-0.3,0.3,0.01), data =IVData)$coef
  

  #ranksTot = rankquant(y , cbind(1,Xinstant), betas)
  y = IVData$dct 
  Xd = cbind(1,IVData$dyt)
  betas = rbind(betas_pr$endg_var,betas_pr$exog_var)
  
  #ranksTot = rankquant(y , cbind(1,Xinstant), betas)
  nvals = length(y)
  nbeta = dim(betas)[2]
  rank = rep(1,nvals)	
  pred = matrix(y, nvals, nbeta)- Xd%*%betas
  predg0 = (pred >= 0)*1	
  ranksTot = 1+ apply(predg0,1, sum)
  
  #medinc = rep(0,20)
  #probitinc = matrix(c(0),20,4)
  probitRec = matrix(c(0),20,4)
  all = rep(0,20)
  IVData$RESPSTAT = ifelse(is.na(IVData),0,1)
  IVData$recession = ifelse(IVData$time>2007,1,0)
  for (i in 1:20) {
    #keep = (IVData$RESPSTAT== 1)*(ranksTot == i) # RESPSTAT is complete income responses
    #medinc[i] = median(IVData$yt[keep == 1]) # FINCBTXM is before tax income in last 12 months
    #probitRec[i,] = summary(glm(keep[IVData$RESPSTAT== 1] ~ IVData$FINCBTXM[IVData$RESPSTAT== 1], family = binomial(link = "probit"))	)$coefficients[2,]
    withRec[i]= sum((ranksTot == i)*(IVData$recession == 1))
    withoutRec[i]= sum((ranksTot == i)*(IVData$recession == 0))
    #allconsumers= sum((ranksTot == i)*(IVData$RESPSTAT  == 1))
    #withoutRec[i] = withoutRec[i]/allconsumers
    #withRec[i] = withRec[i]/allconsumers
    #probitRec [i,] = summary(glm((ranksTot == i)~ (IVData$recession == 1), family = binomial(link = "probit"))	)$coefficients[2,]
    # The link function provides the relationship between the linear predictor and the mean of the distribution function. 
    # probability of inclusion on income?
  }
  
  #barplot(rbind(probitRec,c(1:20)/20), xlab = "test", axisnames = TRUE,names.arg =c(1:20)/20)
  barplot(withoutRec[3:18], ylim=c(0, 700),main = "2009-2013")
  axis(1, at = 2.3 + (14.8)*c(0,1,2,3)/3, label = c(0.2,0.4,0.6,0.8),outer=NULL, lwd = 0)
  barplot(withRec [3:18], ylim=c(0, 700), main = "2003-2007")
  axis(1, at = 2.3 + (14.8)*c(0,1,2,3)/3, label = c(0.2,0.4,0.6,0.8),outer=NULL, lwd = 0)

          
          , ylim = c(0,0.15), col = "grey", yaxt="n", main = "HOME OWNERSHIP RATE WITHOUT MORTGAGE")
  axis(2, at = c(0,0.15,0.3), label = c("0.05", "0.10","0.15"))
  axis(1, at = 2.3 + (14.8)*c(0,1,2,3)/3, label = c(0.2,0.4,0.6,0.8),outer=NULL, lwd = 0)
  
  barplot(withoutRec[3:18], ylim = c(0,0.3), col = "grey", yaxt="n", main = "HOME OWNERSHIP RATE WITHOUT MORTGAGE")
  axis(2, at = c(0,0.15,0.3), label = c("0.10", "0.25","0.40"))
  axis(1, at = 2.3 + (14.8)*c(0,1,2,3)/3, label = c(0.2,0.4,0.6,0.8),outer=NULL, lwd = 0)
  
  
  validCUTenure = (df$CUTENURE == 1) + (df$CUTENURE == 2) + (df$CUTENURE == 4) # cutenure is housing tenure
  
  withoutmortall = rep(0,20)
  withmortall = rep(0,20)
  probitwithmortall  = matrix(c(0),20,4)
  probitwithoutmortall  = matrix(c(0),20,4)
  for (i in 1:20) {
    allconsumers= sum((ranksTot == i)*(validCUTenure  == 1))
    withoutmort= sum((ranksTot == i)*(df$CUTENURE == 2))
    withmort= sum((ranksTot == i)*(df$CUTENURE == 1))
    withoutmortall [i] = withoutmort/allconsumers
    withmortall [i] = withmort/allconsumers
    probitwithmortall [i,] = summary(glm((ranksTot == i)[validCUTenure  == 1] ~ (df$CUTENURE == 1)[validCUTenure  == 1], family = binomial(link = "probit"))	)$coefficients[2,]
    probitwithoutmortall  [i,] = summary(glm((ranksTot == i)[validCUTenure  == 1] ~ (df$CUTENURE == 2)[validCUTenure  == 1], family = binomial(link = "probit"))	)$coefficients[2,]
  }
  
  
  remprincipal = rep(0,20)
  validoutstandingbal = 1-is.na(df$QBLNCM1X) # Principal balance outstanding at beginning of month, three months ago
  probitoutstandingbal  = matrix(c(0),20,4)
  for (i in 1:20) {
    keep = (validoutstandingbal == 1)*(ranksTot == i)
    remprincipal[i] = median(df$QBLNCM1X[keep == 1])
    probitoutstandingbal [i,] = summary(glm(keep[validoutstandingbal == 1] ~ df$QBLNCM1X[validoutstandingbal == 1], family = binomial(link = "probit"))	)$coefficients[2,]
  }
  list(ninc = sum(df$RESPSTAT== 1), medinc = medinc , nmort = sum(validCUTenure ), withoutmortall =withoutmortall , withmortall = withmortall ,
       nrembal = sum(validoutstandingbal ), 	remprincipal=remprincipal, probitinc = probitinc, probitwithmortall   = probitwithmortall , 
       probitwithoutmortall   = probitwithoutmortall , probitoutstandingbal  = probitoutstandingbal )
}	