#
#
# Life Expectancy Plot
#
#
setwd("~/Documents/Courses/SQB-Interface2019/DataSets/HumanLifeExpectancy")
dir()

help(read.table)

mytable<-read.table("HumanLifeExpectancy.txt", header=TRUE,sep="\t")

mytable
colnames(mytable)
rownames(mytable)

#colnames(mytable)[2]<-"Life Expectancy"

mytable[,"Country"]
mytable$Country

mytable[,"Life Expectancy"]
mytable$Life.Expectancy

#colnames(mytable)[2]<-"Life Expectancy"

attributes(mytable)

help(plot)

plot(mytable)
plot(mytable$Year, mytable$Life.Expectancy)
plot(mytable[,"Year"], mytable[,"Life.Expectancy"])
plot(mytable[,1], mytable[,2])

plot(mytable$Year, mytable$Life.Expectancy,main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)", cex=2,pch=16)



# Let's explore the 
p<-1:25
color<-rainbow(25)
plot(p,p,pch=p,cex=0.1*p,col=color)





color<-rainbow(8)

help(rgb)

#rgb(1,0,0,1)

color[mytable$Country]

plot(mytable$Year, mytable$Life.Expectancy,main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)", cex=2,pch=16,col=color[mytable$Country])

plot(mytable$Year, mytable$Life.Expectancy,main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)", cex=2,pch=16,col=rgb(1,0,0,1))

plot(mytable$Year, mytable$Life.Expectancy,main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)", cex=2,pch=16,col="lightgray")


gradient<-1:8/8
nrow(mytable)
plot(mytable$Year, mytable$Life.Expectancy,main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)", cex=2,pch=16,col=rgb(gradient[mytable$Country],0,0,1))


plot(mytable$Year, mytable$Life.Expectancy,main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)", cex=3,pch=16,col=color[mytable$Country],cex.lab=1.5)

lines(mytable$Year, mytable$Life.Expectancy,lwd=2,lty=2)
help(text)
text(mytable$Year, mytable$Life.Expectancy,mytable$Country,col=color[mytable$Country])


plot(mytable$Year, mytable$Life.Expectancy,t="b",main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)")

plot(mytable$Year, mytable$Life.Expectancy,t="p",main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)", cex=3,pch=16,col=color[mytable$Country],cex.lab=1.5, cex.axis=1.5,cex.main=2)

lines(mytable$Year, mytable$Life.Expectancy,lwd=2,lty=2)


plot(mytable$Year, mytable$Life.Expectancy,t="b",main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)")


mytable$Normalised.Life.Expectancy<-mytable$Life.Expectancy

mi<-min(mytable$Life.Expectancy)
ma<-max(mytable$Life.Expectancy)
range<-ma-mi

range<-max(mytable$Life.Expectancy)-min(mytable$Life.Expectancy)

mytable$Normalised.Life.Expectancy<-(mytable$Life.Expectancy-mi)/range


plot(mytable$Year, mytable$Normalised.Life.Expectancy,t="b",main="Life expectancy",xlab="Year", ylab="Life expectancy (Year)")


getwd()

help(write.table)
colnames(mytable)

write.table(mytable[,c(1,4,3)],"NormalisedLifeExpectancy.txt",sep="\t",row.names=FALSE)


#Using Excel files


require(openxlsx)
read.xlsx("LifeExpectancy.xlsx")
write.xlsx(mytable,"NormalisedLifeExpectancy.xlsx")
