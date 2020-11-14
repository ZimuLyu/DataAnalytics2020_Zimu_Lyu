library(e1071)
set.seed(1)

# generate observations belonging to two classes
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
x
y
plot(x, col=(3-y)) # not linearly separable

# encode the response as a factor variable
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10, scale=FALSE)
# scale=FALSE: not scale each feature to N(0, 1).
plot(svmfit , dat)

svmfit$index
summary(svmfit)

# change cost to 0.1
svmfit2 <- svm(y ~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit2 , dat)
svmfit2$index


set.seed(1)
tune.out <- tune(svm, y ~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out) # cost=0.1 is best
bestmod <- tune.out$best.model # store the best model
summary(bestmod)

# the test data set
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <-predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

# use cost = 0.01
svmfit3 <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred <- predict(svmfit3, testdat)
table(predict=ypred, truth=testdat$y)

# linearly separable
x[y==1,] <- x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

# very large cost
dat <- data.frame(x=x,y=as.factor(y))
svmfit <-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit) # 3 support vectors
plot(svmfit, dat)

# cost = 1
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit) # 7support vectors
plot(svmfit, dat)

# Khan dataset
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain))
out <- svm(y ~., data=dat, kernel="linear", cost=10)
summary(out)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y) # 2 test set errors

# Group 3
# SVM 1

## generate the data
n <- 150
p <- 2 # dimension
sigma <- 1 # variance
meanpos <- 0 # center of the distribution of positive examples
meanneg <- 3 # center of the distribution of negative examples
npos <- round(n/2) # 75 positive examples
nneg <- n-npos # 75 negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)
head(x)
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
## visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

## split the data, 80% training
ntrain <- round(n*0.8) # 120
tindex <- sample(n,ntrain) # random indexes
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain <- rep(0,n)
istrain[tindex] <- 1

# visualize
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),
       col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))


# SVM 2

library(rpart)
data(Ozone, package="mlbench")

## split the data
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])

## train and predict
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
summary(svm.model)
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)


# SVM 3

data(iris)
attach(iris)

## classification mode

model <- svm(Species ~ ., data = iris)
summary(model)

x <- subset(iris, select = -Species)
y <- Species
model2 <- svm(x, y) 
print(model2)
summary(model2)

pred <- predict(model, x)
table(pred, y)
# or
pred <- fitted(model)
table(pred, y)

# compute decision values and probabilities
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## regression mode on two dimensions

x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)
m <- svm(x, y)
new <- predict(m, x)
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

## density-estimation

# create 2-dim
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)
head(X)

# traditional way
m <- svm(X, gamma = 0.1)
# formula interface
m <- svm(~., data = X, gamma = 0.1)
# or
m <- svm(~ a + b, gamma = 0.1)

# test
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict(m, newdata)
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

# weights
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)
summary(m)

# SVM 4

library(kernlab)
data(promotergene)

ind <- sample(1:dim(promotergene)[1],20)
genetrain <- promotergene[-ind, ]
genetest <- promotergene[ind, ]

gene <-  ksvm(Class~.,data=genetrain,kernel="rbfdot",
              kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
summary(gene)
genetype <- predict(gene,genetest,type="probabilities")
result <- cbind(genetest[1], genetype)
result


# SVM 5

m1 <- matrix(c( 
  0,    0,    0,    1,    1,    2,     1, 2,    3,    2,    3, 3, 0, 1,2,3, 
  0, 1, 2, 3, 
  1,    2,    3,    2,    3,    3,     0, 0,    0,    1, 1, 2, 4, 4,4,4,    0, 
  1, 2, 3, 
  1,    1,    1,    1,    1,    1,    -1,-1,  -1,-1,-1,-1, 1 ,1,1,1,     1, 
  1,-1,-1 
), ncol = 3 ) 
Y <- m1[,3] 
X <- m1[,1:2] 
df <- data.frame(X, Y)
df

par(mfcol=c(4,2)) 
for(cost in c( 1e-3 ,1e-2 ,1e-1, 1e0,  1e+1, 1e+2 ,1e+3)) { 
  model.svm <- svm(Y ~ ., data = df, type = "C-classification", 
                    kernel = "linear", cost = cost, scale = FALSE) 
  plot(x=0, ylim=c(0,5), xlim=c(0,3), main=paste("cost: ", cost, "#SV: ", nrow(model.svm$SV))) 
  points(m1[m1[,3]>0,1], m1[m1[,3]>0,2], pch=3, col="green") 
  points(m1[m1[,3]<0,1], m1[m1[,3]<0,2], pch=4, col="blue") 
  points(model.svm$SV[,1], model.svm$SV[,2], pch=18, col="red") 
}


# SVM 6

data(spam)
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]

filter <- ksvm(type~., data=spamtrain, kernel="rbfdot", kpar=list(sigma=0.05), C=5, cross=3)
filter

mailtype <- predict(filter,spamtest[,-58])
table(mailtype,spamtest[,58])


# SVM 7

data(iris)

# create a kernel function
rbf <- rbfdot(sigma=0.1)
rbf

# train a bound constraint support vector machine
irismodel <- ksvm(Species~., data=iris, type="C-bsvc", kernel=rbf, C=10, prob.model=TRUE)
irismodel

fitted(irismodel)
predict(irismodel, iris[,-5], type="probabilities")


# SVM 8

x <- rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
y <- matrix(c(rep(1,60), rep(-1,60)))

svp <- ksvm(x, y, type="C-svc")
plot(svp, data=x)

K <- as.kernelMatrix(crossprod(t(x)))
K
svp2 <- ksvm(K, y, type="C-svc")
svp2

xtest <- rbind(matrix(rnorm(20),2),matrix(rnorm(20,mean=3),2))
Ktest <- as.kernelMatrix(crossprod(t(xtest),t(x[SVindex(svp2), ])))
predict(svp2, Ktest)


# SVM 9

## create custom kernel 

k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)
gene <- ksvm(Class~., data=promotergene[c(1:20, 80:100),], kernel=k, C=5, cross=5)
gene

## use text with string kernels
data(reuters)
reuters[[1]]
is(reuters)
tsv <- ksvm(reuters, rlabels, kernel="stringdot", kpar=list(length=5), cross=3, C=10)
tsv

## regression
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)
regm <- ksvm(x, y, epsilon=0.01, kpar=list(sigma=16), cross=3)
plot(x, y, type="l")
lines(x, predict(regm,x), col="red")

# SVM 10 and 11 are included in SVM 9
