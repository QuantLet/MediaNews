install.packages("zoo")
library(zoo)
library(ggplot2)
unrate.df = read.csv("NNTAdata.csv")

NBER = unrate.df$NBER
drecession <- diff(NBER)
date = as.yearmon(seq(as.Date("1996-01-01"),as.Date("2014-12-31"),by = "months"),"%Y%m")

recession.start <- date[which(drecession==1)+1]
recession.end <- date[which(drecession==-1)]
recession.df <- data.frame(recession.start,recession.end)
colnames(recession.df) <- c("start","end")


NumNews = unrate.df$NumNews
NNTA = unrate.df$NNTA
NNTA_size = unrate.df$NNTA_size
NNTA_cent = unrate.df$NNTA_cent
unrate.df = data.frame(date=date, NumNews = NumNews,NNTA = NNTA,NNTA_size = NNTA_size,
                       NNTA_cent = NNTA_cent)
ggplot(unrate.df) +  
  geom_line(aes(x=date,y=NNTA,col="NNTA"),linetype=11,size = 1.1) + 
  geom_line(aes(x=date,y=NNTA_size,col="NNTA_size"),linetype=11,size = 1.1) + 
  geom_line(aes(x=date,y=NNTA_cent,col="NNTA_cent"),linetype=11,size = 1.1) + 
  theme_bw() +
  geom_rect(data=recession.df,aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf),
            fill='blue',alpha=0.2) +  labs(x="", y="") + 
  scale_colour_manual("", labels = c(expression("NNTA"),expression("NNTA"^sz),expression("NNTA"^ctr)),
                    breaks=  c("NNTA","NNTA_size","NNTA_cent"),
                    values = c("NNTA" = "red","NNTA_size"="purple","NNTA_cent" = "brown")) +
  theme(legend.text.align = 0) +
  scale_linetype_manual("NNTA",values = c(1,11)) +
  
  scale_linetype_manual("NNTA_size",values = c(1,11)) +
  scale_linetype_manual("NNTA_cent",values = c(1,11)) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1)) + theme(legend.position = c(0.14, 0.8))

