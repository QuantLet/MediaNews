setwd("c:~")
libraries                 = c("stochvol", "vars","lubridate")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

install.packages("stochvol")
library(stochvol)

uncert                    = function(dat11,dat_Z,a=1,b=ncol(dat11),mean1 = T,q=5){
Uncert1                   = matrix(rep(0,(nrow(dat_Z)-5)*ncol(as.matrix(dat11[,a:b]))),ncol=ncol(as.matrix(dat11[,a:b])))
Residual                  = matrix(rep(0,(nrow(dat_Z)-5)*ncol(as.matrix(dat11[,a:b]))),ncol=ncol(as.matrix(dat11[,a:b])))
  
  
for (i in a:b){
  print(i)
  y                       = numeric()
  ys                      = dat11[,i]
  y                       = cbind(y,ys)
  y_l                     = numeric()
  for (qq in 1:(q-1)){
    #y_l=c(ys[(qq+1):length(ys)],rep(NA,qq))
    y_ll                  = c(rep(NA,qq),ys[1:(length(ys)-qq)])
    y_l                   = cbind(y_l,y_ll)
  }
    y                     = cbind(y,y_l)
    y_0                   = ys
    
    dat_ZZ                = cbind(dat_Z)
    dat_ZZ                = dat_ZZ[-1*c(1:4),]
    dat_ZZ                = as.matrix(dat_ZZ)
    lm_selection          = lm(y_0[-c(1:q)] ~ dat_ZZ[-nrow(dat_ZZ),])
    F_Final               = as.matrix(dat_Z[,(abs(summary(lm_selection)$coefficients[,4][-c(1)])<0.1)])
    as.numeric((abs(summary(lm_selection)$coefficients[,4][-c(1)])<0.1))
    
    
    r                     = dim(F_Final)[2]
    q                     = 5
    
    
    
    if (dim(F_Final)[2]==0){ 
      Endog               = y[-c(1:4),]
    } else {
      dat_FFF             = numeric()
      for (qq in 1:(q-1)){
        dat_Flag          = matrix(rep(NA,qq*r),ncol=r)
        dat_Flag1         = rbind(as.matrix(dat_Flag),as.matrix(F_Final[1:(nrow(F_Final)-qq),]))
        dat_FFF           = cbind(dat_FFF,dat_Flag1)
      }
      dat_FFF             = cbind(F_Final,dat_FFF)
      Endog               = cbind(dat_FFF[-c(1:4),],y[-c(1:4),])
      
      EE_n                = numeric()
      for (rn in 1:r){
        for (qn in 1:q){
          EE_n            = cbind(EE_n,Endog[,(r*(qn-1)+rn)])
        }
      }
      Endog[,1:(q*r)]     = EE_n      
    }
    
    if (dim(F_Final)[2]!=0){
      colnames(Endog)[1:(length(colnames(Endog))-5)
                      ]   = c(1:(length(colnames(Endog))-5))
    }
    XX                    = numeric()
    for (nn1 in 1:r){
      for (mm1 in 1:q){
        XX                = Endog[,((mm1-1)*r+nn1)]
      }
    }
    Endog[,(q*r)]         = XX
    lt = length(colnames(Endog))
    colnames(Endog)[lt]   = "y_4"
    colnames(Endog)[lt-1] = "y_3"
    colnames(Endog)[lt-2] = "y_2"
    colnames(Endog)[lt-3] = "y_1"
    
    var_e                 = VAR(as.matrix(Endog),type="none")
    ###########################################
    ######################################################################################################
    ##
    if (dim(F_Final)[2]==0){
      restrict1           = matrix(as.numeric(upper.tri(matrix(1,q,q),diag=TRUE)),nrow=q)
      var_ee              = restrict(var_e, method = "man", resmat = restrict1)
    }else {
      
      matrix_FM           = matrix(as.numeric(upper.tri(matrix(1,q,q),diag=TRUE)),nrow=q)
      matrix_F            = matrix(1, nrow = r, ncol = r) %x% matrix_FM
      matrix_0            = matrix(rep(0,q*r*q),ncol=q)
      matrix_j            = matrix_F[1:q,]
      matrix_yj           = matrix(as.numeric(upper.tri(matrix(1,q,q),diag=TRUE)),nrow=q)
      restrict            = rbind(cbind(matrix_F,matrix_0),cbind(matrix_j,matrix_yj))
      var_ee              = restrict(var_e, method = "man", resmat = restrict)
    }
    
    FAVAR_Resid           = as.matrix(resid(var_ee))[,(ncol(Endog)-4):ncol(Endog)]
    Resid_Y1              = FAVAR_Resid
    
    
    Uncert_Y1=matrix(rep(0,length(Resid_Y1)),ncol=ncol(Resid_Y1))
    for (yi1 in 1:ncol(Resid_Y1)){
      draws_yi1           = svsample(Resid_Y1[,yi1], draws = 5000, burnin = 1000)
      Uncert_Y1[,yi1]     = draws_yi1$summary$latent[,6]
    }
    SV_Y1_V               = (Uncert_Y1)^2
    D1_Y                  = SV_Y1_V
    ##############################################
    ###############################################
    #############################################
    
    Uncert_FF1            = matrix(rep(0,nrow(D1_Y)),ncol=1)
    for (oo in 1:nrow(D1_Y)) {
      diagMY1             = diag(D1_Y[oo,])
      Y_F1                = diagMY1
      MM1                 = Y_F1
      #I=matrix(rep(1,nrow(MM1)),ncol=1)
      I                   = matrix(c(1,rep(0,(nrow(MM1)-1))),ncol=1)
      Uncert_FF1[oo,1]    = (t(I)%*%MM1%*%I)^0.5
    } # for oo
    
    Residual[,i-a+1]      = Resid_Y1[,1]
    Uncert1[,i-a+1]       = Uncert_FF1
  }
  
  if (mean1 == T){
    First                 = apply(Residual,1,mean)
  } else {
    First                 = princomp(Residual)$scores[,1]
  }
  Uncert_mean             = apply(Uncert1, 1, mean)
  U                       = data.frame(First,Uncert_mean)
}


#market aggregated news.
dat_news                  = as.matrix(read.csv("1996.csv",header=F))
dat_news[,1]              = as.character(as.Date(dat_news[,1],"%m/%d/%y"))
dat_news[,1]              = format(as.Date(dat_news[,1]), "%Y%m")
dat_new                   = as.numeric(dat_news[,2])-as.numeric(dat_news[,4])
###############################################################
#for paper work

Positive                  = as.numeric(dat_news[,2])
Negative                  = as.numeric(dat_news[,4])
Optimism                  = Positive - Negative

Orig_News                 = data.frame(Optimism,Positive,Negative)
Orig_News                 = Orig_News[match('199601',dat_news[,1]):match('201012',dat_news[,1]),]
RInd_new                  = Orig_News[,ncol(Orig_News)]
Orig_News                 = Orig_News[,-ncol(Orig_News)]
head(Orig_News)

scores                    = cbind(Optimism, Positive, Negative)

################################################################
ym                        = dat_news[,1]
news                      = aggregate(dat_new,by = list(ym),FUN = "mean")
news                      = news[match('199601',news[,1]):match('201012',news[,1]),]
dat_news                  = news
dat_news                  = as.matrix(dat_news[,2])
endDate                   = ymd(20101231)


Date                      = endDate %m-% months(c(0:(dim(news)[1]-1)))
Date                      = rev(Date)
nDate                     = Date[(length(Date)-174):length(Date )]


load("dat_Z.RData")
ndat_Z                    = dat_Z[(nrow(dat_Z)-179):(nrow(dat_Z)),]


nndat_Z                   = as.matrix(cbind(ndat_Z,as.matrix(dat_news)))
testt                     = uncert(as.matrix(dat_news),nndat_Z,mean=F)
newssent2                 = testt[,2]
newssent1                 = testt[,1]




k                         = 12
Date                      = nDate[-c(1:(k-1))]



newssent1                 = rollmean(newssent1,12)
Newssent1                 = scale(newssent1)


Newssent2                 = rollmean(scale(newssent2),k)

load("nBWsent.RData")
BWsent                    = scale(nBWsent)[-c(1:(k-1))]

load("nPLS.RData")
PLSsent                   = scale(nPLS)[-c(1:(k-1))]

plot(Date,scale(Newssent1),type="l",ylim=c(-3.5,3.5),ylab="Sentiment")
lines(Date,PLSsent,type="l",col="blue",lty =2)
lines(Date,BWsent,type="l",col="red",lty=5)
legend("topright",legend = c("Newssent1", "PLSsent","BWsent"),
       text.width = strwidth("Newssent1"),
       lty=c(1,2,5),col=c("black","blue","red"), title = "Sentiment Types")


plot(Date,scale(Newssent2),type="l",ylim=c(-2,3.5),ylab="Sentiment")
lines(Date,PLSsent,type="l",col="blue",lty =2)
lines(Date,BWsent,type="l",col="red",lty=5)
legend("topright",legend = c("Newssent2", "PLSsent","BWsent"),
       text.width = strwidth("Newssent2"),
       lty=c(1,2,5),col=c("black","blue","red"), title = "Sentiment Types")
