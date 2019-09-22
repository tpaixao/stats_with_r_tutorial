setwd("~/Documents/Courses/SQB-Interface2019/DataSets/IL-10AllelesDataSet")


alleles_wtgfp<-read.table("IL-10Alleles",header=TRUE)
allele_wt<-read.table("IL-10Alleles_controlwt",header=TRUE)
allele_gfp<-read.table("IL-10Alleles_controlgfp",header=TRUE)

plot(allele_wt, xlim=c(0,4), ylim=c(0,4), col="red", cex=0.5)
points(allele_gfp, xlim=c(0,4), ylim=c(0,4),col="green", cex=0.5)

plot(alleles_wtgfp, xlim=c(0,4), ylim=c(0,4), col="black", cex=0.5)

abline(v=1.4)
abline(h=1.4)

mgfp<-1.4
mwt<-1.4



mgfp<-mean(allele_wt[,"gfp"])
mwt<-mean(allele_gfp[,"wt"])

plot(alleles_wtgfp, xlim=c(0,4), ylim=c(0,4), col="black", cex=0.5)

abline(v=mwt, col="darkgreen")
abline(h=mgfp, col="darkred")

tl<-length(which(alleles_wtgfp[,"wt"]<mwt & alleles_wtgfp[,"gfp"]>mgfp))
text(0.2,3.0,tl,cex=1, col="gray")

tr<-length(which(alleles_wtgfp[,"wt"]>=mwt & alleles_wtgfp[,"gfp"]>mgfp))
text(3,3.0,tr,cex=1, col="gray")

bl<-length(which(alleles_wtgfp[,"wt"]<mwt & alleles_wtgfp[,"gfp"]<=mgfp))
text(0.2,0.2,bl,cex=1, col="gray")

br<-length(which(alleles_wtgfp[,"wt"]>=mwt & alleles_wtgfp[,"gfp"]<=mgfp))
text(3,0.2,br,cex=1, col="gray")

data<-matrix(c(tl,bl, tr,br),2,2)
fisher.test(data)
sum(c(tl,bl, tr,br))


plot(allele_wt, xlim=c(0,4), ylim=c(0,4), col="red", cex=0.2)
points(allele_gfp, xlim=c(0,4), ylim=c(0,4),col="green", cex=0.2)


rgwt<-lm(allele_wt[,"gfp"] ~ allele_wt[,"wt"])
rggfp<-lm(allele_gfp[,"wt"] ~ allele_gfp[,"gfp"])


attributes(rggfp)
rggfp$coefficients[[1]]
rggfp$coefficients[[2]]


plot(alleles_wtgfp, xlim=c(0,4), ylim=c(0,4), col="black", cex=0.5)
abline(rgwt,col="red")
abline(a=-0.506/0.08177,b=1/0.08177,col="green")
abline(a=rggfp$coefficients[[1]]/rggfp$coefficients[[2]]
,b=1/rggfp$coefficients[[2]]
,col="green")


tl<-length(which(alleles_wtgfp[,"gfp"]>=alleles_wtgfp[,"wt"]*rgwt$coefficient[[2]]+rgwt$coefficient[[1]] & 	
			alleles_wtgfp[,"wt"]<=alleles_wtgfp[,"gfp"]*rggfp$coefficient[[2]]+rggfp$coefficient[[1]]))
text(0.2,3.0,tl,cex=1, col="gray")

tr<-length(which(alleles_wtgfp[,"gfp"]>=alleles_wtgfp[,"wt"]*rgwt$coefficient[[2]]+rgwt$coefficient[[1]] & 	
			alleles_wtgfp[,"wt"]>alleles_wtgfp[,"gfp"]*rggfp$coefficient[[2]]+rggfp$coefficient[[1]]))
text(3,3.0,tr,cex=1, col="gray")

bl<-length(which(alleles_wtgfp[,"gfp"]<alleles_wtgfp[,"wt"]*rgwt$coefficient[[2]]+rgwt$coefficient[[1]] & 	
			alleles_wtgfp[,"wt"]<=alleles_wtgfp[,"gfp"]*rggfp$coefficient[[2]]+rggfp$coefficient[[1]]))
text(0.2,0.2,bl,cex=1, col="gray")

br<-length(which(alleles_wtgfp[,"gfp"]<alleles_wtgfp[,"wt"]*rgwt$coefficient[[2]]+rgwt$coefficient[[1]] & 	
			alleles_wtgfp[,"wt"]>alleles_wtgfp[,"gfp"]*rggfp$coefficient[[2]]+rggfp$coefficient[[1]]))
text(3,0.2,br,cex=1, col="gray")


tab<-matrix(c(tl,bl,tr,br),2,2)
fisher.test(tab)
