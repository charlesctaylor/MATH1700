### methods based on normal assumptions

# use first two variablesof iris data
# and first two species

library(MASS)

myir=iris[1:100,c(1,2,5)]
myir$Species=factor(myir$Species)

## compute summary statistics, including pooled covariance
xbar=apply(myir[,1:2],2,mean) # overall mean
xbar1=apply(myir[1:50,1:2],2,mean)
xbar2=apply(myir[51:100,1:2],2,mean)
Sig1=cov(myir[1:50,1:2])
Sig2=cov(myir[51:100,1:2])
Sigp=(48*Sig1+48*Sig2)/96
(aa=t(solve(Sigp)%*%(xbar1-xbar2))) # coefficents of x1 and x2 (a1 and a2 in notes)
(a0=-t(solve(Sigp)%*%(xbar1-xbar2))%*%(xbar1+xbar2)/2) #a0
### so allocate x to group 1 if:   -11.43636 x1  +   14.14303 x2 + 18.73906 > 0
## plot the above
plot(myir[,1],myir[,2],pch=c(rep(1,50),rep(2,50)),xlab="sepal length",ylab="sepal width",cex=1.5)
## boundary
abline(-a0/aa[2],-aa[1]/aa[2])

## confusion matrix:
table(as.numeric(a0)+as.matrix(myir[,1:2])%*%as.numeric(aa)>0,myir$Species)


## the function lda(.) in library(MASS) does this automatically
(ld1=lda(Species~.,data=myir))
table(predict(ld1)$class,myir$Species)



### nonparametric methods: k-nearest neighbour and local averaging
library(spatstat) # used to draw a nice picture (only for 2 variables)

Set=(iris$Species=="setosa"); Vers=(iris$Species=="versicolor"); 
Virg=(iris$Species=="virginica")
library(spatstat)
x1=iris$Sepal.Length
x2=iris$Sepal.Width
plot(x1[Set],x2[Set],xlab="sepal length",
ylab="sepal width",ylim=c(1.8,4.5),xlim=c(4,7.5),pch=19)
points(x1[Vers],x2[Vers],pch=3)
dat=data.frame(x1=x1[!Virg],x2=x2[!Virg],groups=iris$Species[!Virg])
p1=ppp(x=x1[!Virg],y=x2[!Virg],window=owin(c(3.5,8),c(1.7,4.7)))
p2=dirichlet(p1)
n=length(p2$tiles)
n1=dim(unique(dat[1:50,1:2]))[1]
for (i in 1:n1){polygon(x=p2$tiles[[i]][[4]][[1]]$x,y=p2$tiles[[i]][[4]][[1]]$y,density=10)}
for (i in (n1+1):n){polygon(x=p2$tiles[[i]][[4]][[1]]$x,y=p2$tiles[[i]][[4]][[1]]$y)}
points(x1[Set],x2[Set],pch=19)

### what is the error rate of this nearest neighbour classifier?
### does not need any R code!

## now try to choose k by cross-validation, with m=n (leave-one-out cross-validation)
dd=as.matrix(dist(dat[,1:2])) # the distance matrix (has zeros in the diagonal)
dd[1:5,1:5]
diag(dd)=max(dd)+1 # prevents choosing "self" as the nearest
dd[1:5,1:5] # compare with previous
# for nearest nighbour:
table(dat[apply(dd,1,which.min),3],dat[,3]) # try to understand this by breaking it down


#### kernels:
library(fields)
x1=iris[1:50,1:2]
x2=iris[51:100,1:2]
x3=iris[101:150,1:2]
h=0.15
g1=seq(4.2,8,l=201); g2=seq(1.9,4.5,l=201)
locs1=cbind(rep(g1,201),rep(g2,rep(201,201)))
yy=rdist(x1,locs1);yy=exp(-yy^2/(2*h^2))
f1=apply(yy,2,sum)
yy=rdist(x2,locs1);yy=exp(-yy^2/(2*h^2))
f2=apply(yy,2,sum)
yy=rdist(x3,locs1);yy=exp(-yy^2/(2*h^2))
f3=apply(yy,2,sum)
fall=cbind(f1,f2,f3)
labs=apply(fall,1,which.max)
image(g1,g2,matrix(labs,201,201),xlab="sepal length",ylab="sepal width",
main="kernel classifier, bandwidth=0.15",zlim=c(0,5))
 points(x1[,1],x1[,2],pch=19)
 points(x2[,1],x2[,2],pch=3)
 points(x3[,1],x3[,2],pch=2)
text(4.75,4.25, "setosa")
text(5.5,2.1,"versicolor")
text(7,4,"virginica")
## confusion matrix
locs1=rbind(x1,x2,x3)
yy=rdist(x1,locs1);yy=exp(-yy^2/(2*h^2))
f1=apply(yy,2,sum)
yy=rdist(x2,locs1);yy=exp(-yy^2/(2*h^2))
f2=apply(yy,2,sum)
yy=rdist(x3,locs1);yy=exp(-yy^2/(2*h^2))
f3=apply(yy,2,sum)
fall=cbind(f1,f2,f3)
labs=apply(fall,1,which.max)



