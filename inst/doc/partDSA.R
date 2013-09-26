### R code from vignette source 'partDSA.Rnw'

###################################################
### code chunk number 1: partDSA.Rnw:144-147
###################################################
y.out=as.factor(sample(c("a", "b", "c"), 50, TRUE) )
x1=rexp(50)
x2=runif(50)


###################################################
### code chunk number 2: partDSA.Rnw:153-155
###################################################
library(partDSA)
#model1<-partDSA(x=data.frame(x1,x2),y=y.out)


###################################################
### code chunk number 3: partDSA.Rnw:163-166
###################################################
y.out.test=as.factor(sample(c("a", "b", "c"), 100, TRUE) )
x1.test=rexp(100)
x2.test=runif(100)


###################################################
### code chunk number 4: partDSA.Rnw:170-171
###################################################
model2<-partDSA(x=data.frame(x1,x2),y=y.out,x.test=data.frame(x1=x1.test,x2=x2.test),y.test=y.out.test)


###################################################
### code chunk number 5: partDSA.Rnw:348-349
###################################################
model4<-partDSA(x=data.frame(x1,x2),y=y.out,control=DSA.control(missing="no",cut.off.growth=2))


###################################################
### code chunk number 6: partDSA.Rnw:355-356
###################################################
print(model4)


###################################################
### code chunk number 7: partDSA.Rnw:449-452
###################################################
data("GBSG2", package = "TH.data")
mdl1<-partDSA(x=data.frame(GBSG2[,c(1:8)]),y=as.factor(GBSG2$cens),control=DSA.control(cut.off.growth=5,loss.function="gini",minbuck=10))
print(mdl1)


