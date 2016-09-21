setwd("c:~")
library(data.table)
################################################
#download "StreamReuters.RData" through following link: https://www.dropbox.com/s/cn1jp9l58vtc9cn/StreamReuters.RData?dl=0
load("StreamReuters.RData") 
fama                                                                = read.csv("F-F_Research_Data_Factors_daily.CSV")
fama                                                                = fama[(which(as.numeric(as.character(fama[,1]))>=20030101 & as.numeric(as.character(fama[,1]))<=20030108)[1]:which(as.numeric(as.character(fama[,1]))>=20141201 & as.numeric(as.character(fama[,1]))<=20141208)[1]),]
fama[,2:5]                                                          =fama[,2:5]/100

us                                                                  = as.matrix(price)

date                                                                = as.Date(as.character(us[,1]),"%Y-%m-%d")
us[,1]                                                              = format(date,"%Y%m%d")
us                                                                  = us[(which(as.numeric(as.character(us[,1]))>=20030101 & as.numeric(as.character(us[,1]))<=20030108)[1]:which(as.numeric(as.character(us[,1]))>=20141201 & as.numeric(as.character(us[,1]))<=20141208)[1]),]
us                                                                  = as.matrix((us))
us[is.na(us)]                                                       = 0
us[,-1]                                                             = matrix(as.numeric(us[,-1]),nrow=nrow(us[,-1]))
us[is.na(us)]                                                       = 0
ust                                                                 = diff(log(matrix(as.numeric(us[,-1]),nrow=nrow(us[,-1]))))




uu                                                                  = ust


dateuu                                                              = as.numeric(us[-1,1])
datefama                                                            = fama[,1]
commondate                                                          = Reduce(intersect, list(dateuu,datefama))

uu                                                                  = uu[(dateuu %in% commondate),]
fama                                                                = fama[(datefama %in% commondate),]


data1                                                               = cbind(uu,fama)
mkto                                                                = data1$Mkt.RF + data1$RF
data1                                                               = cbind(data1,mkto)

shift                                                               = function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out                                                               = NULL
  abs_shift_by                                                      = abs(shift_by)
  if (shift_by > 0 )
    out                                                             = c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out                                                             = c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out                                                             = x
  out
}

data1$mkto_lag1                                                     = shift(data1$mkto,-1)
data1$mkto_lag2                                                     = shift(data1$mkto,-2)
data1$mkto_lag3                                                     = shift(data1$mkto,-3)
data1$mkto_lag4                                                     = shift(data1$mkto,-4)
data1$mkto_lag5                                                     = shift(data1$mkto,-5)
data1                                                               = cbind(commondate,data1)

code                                                                = data1[,1]
code                                                                = substr(code,1,6)
codeu                                                               = unique(code)

numrow                                                              = length(codeu)-12
numcol                                                              = length(2:(ncol(data1)-10))
Beta                                                                = matrix(NA,ncol=numcol,nrow=numrow)

for (j in 2:(ncol(data1)-10)){
  print(j)
  
  for (kk in codeu[-c(1:12)]){
    startt                                                          = codeu[which(codeu==as.numeric(kk))-12]
    endt                                                            =  codeu[which(codeu==as.numeric(kk))-1]
    start                                                           = which(as.numeric(code) == startt)[1]
    tendt                                                           = which(as.numeric(code) == endt)
    end                                                             = tendt[length(tendt)]
    
    regressiondata                                                  = data1[start:end,c(j,ncol(data1)-9,( (ncol(data1)-4):(ncol(data1)) ))]
    regressiondata[regressiondata==Inf|regressiondata==-Inf]        =NA
    regressiondata                                                  = regressiondata[!is.na(regressiondata[,1]),]
    if (sum(!is.na(regressiondata[,1]))<28){
      next
    }
    rowlocation                                                     = which(codeu==kk)-12
    Beta[rowlocation,j-1]                                           = sum(summary(lm(regressiondata))$coefficients[-1,1])
  }
}

BetaF                                                               = data.frame(codeu[-c(1:12)],Beta)
colnames(BetaF)                                                     = colnames(us)
#colnames(BetaF)                                                    = colnames(us)[-2]
BetaFinal                                                           = BetaF[,which(as.numeric(apply(BetaF[-1],2,sum)!=0)==1)]
write.csv(BetaFinal,"BetaFinal.csv",row.names=F)    


disfinal                                                            = read.csv("disfinal.csv")
disfinal                                                            = as.data.table(disfinal)
date                                                                = as.Date(disfinal[,date])
beta_news_opt                                                       = scale(disfinal[,Sentiment])
beta_news_dis                                                       = scale(disfinal[,disbeta])
BW                                                                  = scale(disfinal[,BW])
PLS                                                                 = scale(disfinal[,PLS])

plot(date,beta_news_opt,type="l")
lines(date,BW,type="l",col="red")
lines(date,PLS,type="l",col="blue")

plot(date,beta_news_opt,type="l",ylim=c(-3.5,3.5),ylab="Sentiment")
lines(date,BW,type="l",col="blue",lty =2)
lines(date,PLS,type="l",col="red",lty=5)
#lines(Date,Newssent1t,type="l",col="green",lty=5)
legend("topright",legend = c("beta_news_opt", "BW","PLS"),
       text.width = strwidth("beta_news_opt"),
       lty=c(1,2,5),col=c("black","blue","red"), title = "Sentiment Types")

##a news with very high sentiment when the current news sentiment is very volatile,then this news migh be
#might be destroyed by this volatility, it's not convincing to believe this news at the moment.
#also, uncertainty paper defines that...second momentum...
plot(date,beta_news_dis,type="l",ylim=c(-2,5),ylab="Sentiment")
lines(date,BW,type="l",col="blue",lty =2)
lines(date,PLS,type="l",col="red",lty=5)
#lines(Date,Newssent1t,type="l",col="green",lty=5)
legend("topright",legend = c("beta_news_dis", "BW","PLS"),
       text.width = strwidth("beta_news_opt"),
       lty=c(1,2,5),col=c("black","blue","red"), title = "Sentiment Types")


