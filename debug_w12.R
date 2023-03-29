ex67 <- read.csv('/Users/wang.c/Desktop/111 NCCU/EOA/r講義/ex67.csv',header=T)

level2<- c(0,5,15)
#calculate linear
subtract_mean <- level2-mean(level2)
linear.contrast <- subtract_mean*3/2
B <- sum(linear.contrast^2)
C <- sum(linear.contrast^3)
b <- -C/B
d <- -b/3
quadratic <- linear.contrast^2+b*linear.contrast +d
quadratic.contrast <-quadratic*111
contrast_unbalanced <- data.frame(linear.contrast,quadratic.contrast)


level1<- c(2,8,16)
contr.poly(3) 
poly(level1,2)
#calculate linear
subtract_mean <- level1-mean(level1)
linear.contrast <- subtract_mean*3/5
B <- sum(linear.contrast^2)
C <- sum(linear.contrast^3)
b <- -C/B
d <- -b/3
quadratic <- linear.contrast^2+b*linear.contrast +d
quadratic.contrast <-quadratic*21
contrast_unbalanced_2 <- data.frame(linear.contrast,quadratic.contrast)

xA<-contrast_unbalanced
xB<-contrast_unbalanced_2

orth.xA1 <- matrix(rep(c(xA),rep(18,6)),ncol=2)

orth.xA2 <- cbind(rep(c(xA[,1]),18),rep(c(xA[,2]),18))

orth.xA3 <- cbind(rep(rep(c(xB[,1]),rep(3,3)),6),rep(rep(c(xB[,2]),rep(3,3)),6))

ex67.orth2 <- data.frame(ex67,orth.xA2,orth.xA3)
names(ex67.orth2) <- c(names(ex67),'Wlin','Wquad','Slin','Squad')

ex67.aov3 <- aov(EC~ Texture*Slin*Squad*Wlin*Wquad,data=ex67.orth2)
summary(ex67.aov3)                                                                                                        # p. 642



