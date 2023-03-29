
##############################################################
#   p. 222, ex 6.7
#  ex67.r      2022/11/22
##############################################################


ex67 <- read.csv('/Users/wang.c/Desktop/111 NCCU/EOA/r講義/ex67.csv',header=T)

ex67.aov1 <- aov(EC~as.factor(Salinity)+as.factor(Water)+Texture,data=ex67)

summary(ex67.aov1)


ex67.aov2 <- aov(EC~ Texture*as.factor(Salinity)*as.factor(Water),data=ex67) #相乘是交互作用
summary(ex67.aov2)                 #(7.a) anova table  with interation relationship                                                                                 # p. 641

tapply(ex67[,4],INDEX=ex67[,1]*ex67[,2],mean)  

aggregate(ex67$EC, list(ex67$Salinity,ex67$Water,ex67$Texture), FUN=mean)

aggregate(ex67$EC, list(ex67$Salinity,ex67$Texture), FUN=mean)

aggregate(ex67$EC, list(ex67$Water,ex67$Texture), FUN=mean)

aggregate(ex67$EC, list(ex67$Salinity,ex67$Water), FUN=mean)

###########################
contr.poly(3) 

xA <- zapsmall(scale(contr.poly(3),scale=1/sqrt(c(2,6))), digits=15)
orth.xA1 <- matrix(rep(c(xA),rep(18,6)),ncol=2)

orth.xA2 <- cbind(rep(c(xA[,1]),18),rep(c(xA[,2]),18))

orth.xA3 <- cbind(rep(rep(c(xA[,1]),rep(3,3)),6),rep(rep(c(xA[,2]),rep(3,3)),6))

ex67.orth <- data.frame(ex67,orth.xA2,orth.xA3)
names(ex67.orth) <- c(names(ex67),'W(lin)','W(quad)','S(lin)','S(quad)')

ex67.aov3 <- aov(EC~ Texture*S(lin)*S(quad)*W(lin)*W(quad),data=ex67.orth)
summary(ex67.aov3)                                                                                                # p. 642









