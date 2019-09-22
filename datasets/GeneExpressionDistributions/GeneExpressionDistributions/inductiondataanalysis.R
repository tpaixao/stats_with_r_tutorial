#
#
#  Induction Data Analysis
#
#
require(openxlsx)
require(cramer)
getwd()
setwd("~/Documents/Courses/SQB-Interface2019/DataSets/GeneExpressionDistributions")
dir()


data<-read.xlsx("InductionData.xlsx")
colnames(data)


#
#
#   Induction after 12h
#
#

plot(NA,xlim=c(0,4000),ylim=c(0,1),xlab="log(pixel intensity)",ylab="CDF")
color<-rainbow(ncol(data))
for (i in 1:ncol(data)) {
	lines(ecdf(data[,i]),cex=0.5,col=color[i])
	text(median(data[,i])-0.05,0.5,colnames(data)[i],cex=2,col=color[i],srt=80)
}


plot(NA,xlim=c(1,4),ylim=c(0,1),xlab="log(pixel intensity)",ylab="CDF")
color<-rainbow(ncol(data))
for (i in 1:ncol(data)) {
	lines(ecdf(log(data[,i],10)),cex=0.5,col=color[i])
	text(median(log(data[,i],10))-0.05,0.5,colnames(data)[i],cex=2,col=color[i],srt=80)
}


ks.test(data[,1],data[,2])
wilcox.test(data[,1],data[,2])

qqplot(log(data[,1],10),log(data[,2],10),xlab=colnames(data)[1],ylab=colnames(data)[2])
abline(a=0,b=1,lty=2)






plot(NA,xlim=c(1,4),ylim=c(0,1),xlab="log(pixel intensity)",ylab="CDF")
for (i in c(1,3,5,7)) {
	lines(ecdf(log(data[,i],10)),cex=0.5,col=color[i])
	text(median(log(data[,i],10))-0.05,0.5,colnames(data)[i],col=color[i],srt=80)
}

plot(NA,xlim=c(1,4),ylim=c(0,1),xlab="log(pixel intensity)",ylab="CDF")
for (i in c(2,4,6,8)) {
	lines(ecdf(log(data[,i],10)),cex=0.5,col=color[i])
	text(median(log(data[,i],10))-0.05,0.5,colnames(data)[i],col=color[i],srt=80)
}



wilcox.test(data[,1], data[,2]/median(ptcda_glucose[,2])*median(data[,1]))
wilcox.test(data[,1], data[,3]/median(data[,3])*median(data[,1]))
wilcox.test(data[,1], data[,4]/median(data[,4])*median(data[,1]))

ks.test(data[,1], data[,2]/median(data[,2])*median(data[,1]))
ks.test(data[,1], data[,3]/median(data[,3])*median(data[,1]))
ks.test(data[,1], data[,4]/median(data[,4])*median(data[,1]))

cramer.test(data[,1], data[,2]/median(data[,2])*median(data[,1]))
cramer.test(data[,1], data[,3]/median(data[,3])*median(data[,1]))
cramer.test(data[,1], data[,4]/median(data[,4])*median(data[,1]))

qqplot(log(data[,1],10),log(data[,2]/median(data[,2])*median(data[,1]),10),xlab=colnames(data)[1],ylab=colnames(data)[2],col=rgb(0.3,0.6,1),pch=19)
abline(a=0,b=1,lty=2)



