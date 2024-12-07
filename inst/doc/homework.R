## -----------------------------------------------------------------------------
anscombe <- data.frame(
  x1=c(10,8,13,9,11,14,6,4,12,7,5),
  x2=c(10,8,13,9,11,14,6,4,12,7,5),
  x3=c(10,8,13,9,11,14,6,4,12,7,5),
  x4=c(8,8,8,8,8,8,8,19,8,8,8),
  y1=c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68),
  y2=c(9.14,8.14,8.74,8.77,9.26,8.1,6.13,3.1,9.13,7.26,4.74),
  y3=c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73),
  y4=c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.5,5.56,7.91,6.89)
)
# show results from four regression analyses
with(anscombe,print(summary(lm(y1~x1))))
with(anscombe,print(summary(lm(y2~x2))))
with(anscombe,print(summary(lm(y3~x3))))
with(anscombe,print(summary(lm(y4~x4))))


## -----------------------------------------------------------------------------
  y1=c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)
  y2=c(9.14,8.14,8.74,8.77,9.26,8.1,6.13,3.1,9.13,7.26,4.74)
  y3=c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
  y4=c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.5,5.56,7.91,6.89)
  fs=c(y1,y2,y3,y4)
  mean(fs)
  median(fs)
  quantile(fs,probs=(0.25))
  quantile(fs,probs=(0.75))
  hist(fs)

## -----------------------------------------------------------------------------
anscombe <- data.frame(
  x1=c(10,8,13,9,11,14,6,4,12,7,5),
  x2=c(10,8,13,9,11,14,6,4,12,7,5),
  x3=c(10,8,13,9,11,14,6,4,12,7,5),
  x4=c(8,8,8,8,8,8,8,19,8,8,8),
  y1=c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68),
  y2=c(9.14,8.14,8.74,8.77,9.26,8.1,6.13,3.1,9.13,7.26,4.74),
  y3=c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73),
  y4=c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.5,5.56,7.91,6.89)
)
pdf(file="fig_more_anscombe.pdf",width = 8.5,height = 8.5)
par(mfrow=c(2,2),mar=c(3,3,3,1))
with(anscombe,plot(x1,y1,xlim=c(2,20),ylim=c(2,14),pch=19,col="darkblue",cex=2,las=1))
title("Set 1")
with(anscombe,plot(x2,y2,xlim=c(2,20),ylim=c(2,14),pch=19,col="darkblue",cex=2,las=1))
title("Set 2")
with(anscombe,plot(x3,y3,xlim=c(2,20),ylim=c(2,14),pch=19,col="darkblue",cex=2,las=1))
title("Set 3")
with(anscombe,plot(x4,y4,xlim=c(2,20),ylim=c(2,14),pch=19,col="darkblue",cex=2,las=1))
title("Set 4")
dev.off()

## -----------------------------------------------------------------------------
n<-1000
u<-runif(n)
t=1
x<-(-2*t^2*log(1-u))^(1/2)#F(x)=1-e^(-(x^2)/2*t^2)
hist(x,prob=TRUE,main=expression(f(x)==x/(t^2)*exp(-(x^2)/2*t^2)))
y<-seq(0,10,.1)
lines(y,y/(t^2)*exp(-(y^2)/2*t^2))

## -----------------------------------------------------------------------------
n<-1000
u<-runif(n)
t=0.5
x<-(-2*t^2*log(1-u))^(1/2)#F(x)=1-e^(-(x^2)/2*t^2)
hist(x,prob=TRUE,main=expression(f(x)==x/(t^2)*exp(-(x^2)/2*t^2)))
y<-seq(0,20,.1)
lines(y,y/(t^2)*exp(-(y^2)/2*t^2))

## -----------------------------------------------------------------------------
n<-1000
u<-runif(n)
t=2
x<-(-2*t^2*log(1-u))^(1/2)#F(x)=1-e^(-(x^2)/2*t^2)
hist(x,prob=TRUE,main=expression(f(x)==x/(t^2)*exp(-(x^2)/2*t^2)))
y<-seq(0,10,.1)
lines(y,y/(t^2)*exp(-(y^2)/2*t^2))

## -----------------------------------------------------------------------------
n<-1000
p1<-0.75
p2<-1-p1
u<-runif(n)
v<-runif(n)
z1<-(-2*log(u))^(1/2)*cos(2*pi*v)
z2<-(-2*log(u))^(1/2)*sin(2*pi*v)+3
r<-sample(c(0,1),n,replace = TRUE,prob = c(p1,p2))
x<- r*z1+(1-r)*z2
hist(x,main=expression(p1==0.75))

## -----------------------------------------------------------------------------
n<-1000
p1<-0.6
p2<-1-p1
u<-runif(n)
v<-runif(n)
z1<-(-2*log(u))^(1/2)*cos(2*pi*v)
z2<-(-2*log(u))^(1/2)*sin(2*pi*v)+3
r<-sample(c(0,1),n,replace = TRUE,prob = c(p1,p2))
x<- r*z1+(1-r)*z2
hist(x,main=expression(p1==0.6))

## -----------------------------------------------------------------------------
n<-1000
p1<-0.5
p2<-1-p1
u<-runif(n)
v<-runif(n)
z1<-(-2*log(u))^(1/2)*cos(2*pi*v)
z2<-(-2*log(u))^(1/2)*sin(2*pi*v)+3
r<-sample(c(0,1),n,replace = TRUE,prob = c(p1,p2))
x<- r*z1+(1-r)*z2
hist(x,main=expression(p1==0.5))

## -----------------------------------------------------------------------------
n<-1000
p1<-0.4
p2<-1-p1
u<-runif(n)
v<-runif(n)
z1<-(-2*log(u))^(1/2)*cos(2*pi*v)
z2<-(-2*log(u))^(1/2)*sin(2*pi*v)+3
r<-sample(c(0,1),n,replace = TRUE,prob = c(p1,p2))
x<- r*z1+(1-r)*z2
hist(x,main=expression(p1==0.4))

## -----------------------------------------------------------------------------
n<-1000
p1<-0.25
p2<-1-p1
u<-runif(n)
v<-runif(n)
z1<-(-2*log(u))^(1/2)*cos(2*pi*v)
z2<-(-2*log(u))^(1/2)*sin(2*pi*v)+3
r<-sample(c(0,1),n,replace = TRUE,prob = c(p1,p2))
x<- r*z1+(1-r)*z2
hist(x,main=expression(p1==0.25))

## -----------------------------------------------------------------------------
n <- 10000
t <- 10 
lambda <- 1    
alpha <- 2     
gamma <- 3   
N_t <- rpois(n, lambda = lambda * t)
X_t <- numeric(n)  
for (i in 1:n)
{
  if (N_t[i] > 0) 
  {X_t[i] <- sum(rgamma(N_t[i], shape = alpha, rate = gamma))}  
   else 
  { X_t[i] <- 0 }
}
u <- mean(X_t)  
v <- var(X_t)    
u1 <- lambda * t * alpha / gamma
v1 <- lambda * t * (alpha / gamma^2)
cat("估计的 X(10) 均值: ", u,"\n")
cat("理论 X(10) 均值: ", u1, "\n")
cat("估计的 X(10) 方差: ", v, "\n")
cat("理论 X(10) 方差: ", v1, "\n")

## -----------------------------------------------------------------------------
n <- 10000
t <- 10 
lambda <- 2    
alpha <- 3     
gamma <- 5      
N_t <- rpois(n, lambda = lambda * t)
X_t <- numeric(n)  
for (i in 1:n)
{
  if (N_t[i] > 0) 
  {X_t[i] <- sum(rgamma(N_t[i], shape = alpha, rate = gamma))}  
   else 
  { X_t[i] <- 0 }
}
u <- mean(X_t)  
v <- var(X_t)    
u1 <- lambda * t * alpha / gamma
v1 <- lambda * t * (alpha / gamma^2)
cat("估计的 X(10) 均值: ", u,"\n")
cat("理论 X(10) 均值: ", u1, "\n")
cat("估计的 X(10) 方差: ", v, "\n")
cat("理论 X(10) 方差: ", v1, "\n")

## -----------------------------------------------------------------------------
n <- 10000
t <- 10 
lambda <- 5    
alpha <- 1    
gamma <- 5      
N_t <- rpois(n, lambda = lambda * t)
X_t <- numeric(n)  
for (i in 1:n)
{
  if (N_t[i] > 0) 
  {X_t[i] <- sum(rgamma(N_t[i], shape = alpha, rate = gamma))}  
   else 
  { X_t[i] <- 0 }
}
u <- mean(X_t)  
v <- var(X_t)    
u1 <- lambda * t * alpha / gamma
v1 <- lambda * t * (alpha / gamma^2)
cat("估计的 X(10) 均值: ", u,"\n")
cat("理论 X(10) 均值: ", u1, "\n")
cat("估计的 X(10) 方差: ", v, "\n")
cat("理论 X(10) 方差: ", v1, "\n")

## -----------------------------------------------------------------------------
 m <- 1e4; x<- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
 t <- runif(m, min=0, max=x)
 theta.hat <- mean(30*t^2*(1-t)^2) * x
 round(theta.hat,5)
 print(pbeta(x, shape1=3, shape2=3))

## -----------------------------------------------------------------------------
f <- function(n, a) 
  {
  u <- runif(n) 
  x <- a * sqrt(-2 * log(u)) 
  }
g <- function(n, a) 
  {
  u <- runif(n/2) 
  x <- a * sqrt(-2 * log(u)) 
  xp <- a * sqrt(-2 * log(1 - u)) 
  return(c(x, xp))
  }
sd1 <- function(n, a) {
  x1 <- f(n, a)
  x2 <- f(n, a)
  x3 <- g(n, a)
  mean1 <- (x1 + x2) / 2
  mean2 <- (x3[1:(n/2)] + x3[(n/2 + 1):n]) / 2
  percent_reduction <- 100 * (var(mean1) - var(mean2)) / var(mean1)
}
 print(sd1(10000,1))

## -----------------------------------------------------------------------------
n=1e4;
est <- sd <- numeric(2)
g <- function(x) 
  {
  x^2*exp(-x^2/2)/sqrt(2*pi) * (x > 1)
  }
x<-rnorm(n)
f1g <- g(x)/exp(1-x)
u1 <- mean(f1g)
sd1 <- sd(f1g)
x <- rexp(n, 1)
f2g <- g(x)*x^2
u2 <- mean(f2g)
sd2 <- sd(f2g)
print(c(u1,u2,sd1,sd2))

## -----------------------------------------------------------------------------
quick_sort<-function(x)
  {
  num<-length(x)
      if(num==0||num==1){return(x)
        }else{
          t<-x[1]
          y<-x[-1]
          lower<-y[y<t]
          upper<-y[y>=t]
          return(c(quick_sort(lower),t,quick_sort(upper)))
        }
}
n<-c(1e4,2e4,4e4,6e4,8e4)
m<-100;y<-numeric(m);average<-numeric(5)
for(k in 1:5){
  for(j in 1:m){
    a<-sample(1:n[k])
    y[j]<-system.time(quick_sort(a))[1]
  }
  average[k]<-mean(y)
} 
average
z<-n*log(n)
lm(average~z)
plot(z,average)
lines(z,average)

## -----------------------------------------------------------------------------
skewness_values <- function(a,b){
   sqrt.b1<-numeric(m)
   c<-numeric(length(b))
   for (i in 1:m) {
    x <- mean(a[i,])
    y <- sd(a[i,])
    sqrt.b1[i] <- if((mean((a[i,] - x)^3) / y^3) >= 0){
    sqrt((mean((a[i,] - x)^3) / y^3))
    }else{
      sqrt.b1[i] <- NA
    }
   }
   sqrt.b1 <- na.omit(sqrt.b1)
   c <- quantile(sqrt.b1, probs = b)
   print(b)
   print(c)
}

normal_b<- function(n,b){
  t<-qnorm(b, mean = 0, sd = sqrt(6/m))
  print(t)
}

standard_errors <- function(b,x_values,m) {
    se <- sqrt((b*(1-b)) / (m * x_values^2)) 
    print(se)
}
n<-1000
m<-10000
b <- c(0.025, 0.05, 0.95, 0.975)
x_values <- qnorm(b)

a<- matrix(rnorm(m * n), nrow = m, ncol = n)

skewness_values(a,b)
normal_b(n,b)
standard_errors(b,x_values,m)


## -----------------------------------------------------------------------------
n <- 1e3 
alpha <- 0.05
m <- 1e3

x <- rnorm(n, mean=1, sd=1) 
y <- rnorm(n, mean=0, sd=1)  

pearson_res <- cor.test(x, y, method = "pearson")
pearson_p_value <- pearson_res$p.value

spearman_res <- cor.test(x, y, method = "spearman")
spearman_p_value <- spearman_res$p.value

kendall_res <- cor.test(x, y, method = "kendall")
kendall_p_value <- kendall_res$p.value

cat("Pearson相关性检验:\n")
cat("相关系数:", pearson_res$estimate, "\n")
cat("p值:", pearson_p_value, "\n\n")

cat("Spearman相关性检验:\n")
cat("相关系数:", spearman_res$estimate, "\n")
cat("p值:", spearman_p_value, "\n\n")

cat("Kendall相关性检验:\n")
cat("相关系数:", kendall_res$estimate, "\n")
cat("p值:", kendall_p_value, "\n\n")

## -----------------------------------------------------------------------------
n <- 1e3 
alpha <- 0.05
m <- 1e3

x <-runif(n, -1, 1)
y <-rnorm(n, mean=0, sd=1) 

pearson_res <- cor.test(x, y, method = "pearson")
pearson_p_value <- pearson_res$p.value

spearman_res <- cor.test(x, y, method = "spearman")
spearman_p_value <- spearman_res$p.value

kendall_res <- cor.test(x, y, method = "kendall")
kendall_p_value <- kendall_res$p.value

cat("Pearson相关性检验:\n")
cat("相关系数:", pearson_res$estimate, "\n")
cat("p值:", pearson_p_value, "\n\n")

cat("Spearman相关性检验:\n")
cat("相关系数:", spearman_res$estimate, "\n")
cat("p值:", spearman_p_value, "\n\n")

cat("Kendall相关性检验:\n")
cat("相关系数:", kendall_res$estimate, "\n")
cat("p值:", kendall_p_value, "\n\n")

## -----------------------------------------------------------------------------
set.seed(456)
N<-1000
m0<-950
m<-10000
alpha<-0.1
FWER <- function(R, FP) {
  mean(FP > 0)
  }
FDR <- function(R, FP) {
  if (R == 0) return(0)
  mean(FP / R)
}
TPR <- function(R, TP) {
  if (R == 0) return(0)
  mean(TP / (N-m0)) 
}
y<-matrix(nrow = m,ncol = 6)
for (i in 1:m) {
  p<-c(runif(m0), rbeta(N-m0, 0.1, 1))
p.adj_bonferroni<-p.adjust(p,method = 'bonferroni')
p.adj_fdr<-p.adjust(p,method = 'fdr')
bonferroni_R <- sum(p.adj_bonferroni < alpha)
bonferroni_FP <- sum(p.adj_bonferroni[1:950] < alpha)
bonferroni_TP <- sum(p.adj_bonferroni[951:N] < alpha)
fdr_R <- sum(p.adj_fdr < alpha)
fdr_FP <- sum(p.adj_fdr[1:950] < alpha)
fdr_TP <- sum(p.adj_fdr[951:N] < alpha)
y[i,1]<-FWER(bonferroni_R,bonferroni_FP)
y[i,2]<-FDR(bonferroni_R,bonferroni_FP)
y[i,3]<-TPR(bonferroni_R,bonferroni_TP)
y[i,4]<-FWER(fdr_R,fdr_FP)
y[i,5]<-FDR(fdr_R,fdr_FP)
y[i,6]<-TPR(fdr_R,fdr_TP)
}
bonferroni_FWER<-mean(y[,1])
bonferroni_FDR<-mean(y[,2])
bonferroni_TPR<-mean(y[,3])
fdr_FWER<-mean(y[,4])
fdr_FDR<-mean(y[,5])
fdr_TPR<-mean(y[,6])
output_table <- data.frame(
  Metric = c("FWER", "FDR", "TPR"),
  Bonferroni = c(bonferroni_FWER, bonferroni_FDR,bonferroni_TPR),
  BH = c(fdr_FWER, fdr_TPR, fdr_TPR)
)

print(output_table)

## -----------------------------------------------------------------------------
x=c(3,5,7,18,43,85,91,98,100,130,230,487)
library(boot)
#定义参数分布
gen_function <- function(x,mle) {
    data <- rexp(length(x),mle)
    return(data)}
#定义计算统计量的函数
theta_star_function <- function(x,i) 1/mean(x[i])
B <- boot(data =x, sim = "parametric", ran.gen =  gen_function, mle = 1/mean(x), statistic = theta_star_function, R=1000)
round(c(original=B$t0,bias=mean(B$t)-B$t0,se=sd(B$t)),5)


## -----------------------------------------------------------------------------
set.seed(123)
library(boot)
x=c(3,5,7,18,43,85,91,98,100,130,230,487)
mle = mean(x)
#定义统计量计算的函数
theta_star_function <- function(x,i) mean(x[i])
#执行自助法
B <- boot(data = x, statistic = theta_star_function, R=1000)
#计算置信区间
ci_norm <- boot.ci(B, type = "norm")
ci_basic <- boot.ci(B, type = "basic")
ci_perc <- boot.ci(B, type = "perc")
ci_bca <- boot.ci(B, type = "bca")
cat("Standard Normal Method CI:\n", ci_norm$normal[2:3], "\n","Basic Method CI:\n", ci_basic$basic[4:5], "\n",
"Percentile Method CI:\n", ci_perc$percent[4:5], "\n","BCa Method CI:\n", ci_bca$bca[4:5], "\n")

## -----------------------------------------------------------------------------
ci_norm_lenth=ci_norm$normal[3]-ci_norm$normal[2]
ci_basic_lenth=ci_basic$basic[5]-ci_basic$basic[4]
ci_perc_lenth=ci_perc$percent[5]-ci_perc$percent[4]
ci_bca_lenth=ci_bca$bca[5]-ci_bca$bca[4]
cat(ci_norm_lenth,ci_basic_lenth,ci_perc_lenth,ci_bca_lenth)

## -----------------------------------------------------------------------------
set.seed(456)
library(boot)
x=c(3,5,7,18,43,85,91,98,100,130,230,487)
mle = mean(x)
m=1000
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
for(i in 1:m){
  B <- boot(data = x, statistic = theta_star_function, R=1000)#执行自助法
  ci <- boot.ci(B,type=c("norm","basic","perc","bca"))
  ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5];ci.bca[i,]<-ci$bca[4:5]
}
cat('norm =',mean(ci.norm[,1]<=mle & ci.norm[,2]>=mle),
    'basic =',mean(ci.basic[,1]<=mle & ci.basic[,2]>=mle),
    'perc =',mean(ci.perc[,1]<=mle & ci.perc[,2]>=mle),
    'BCa =',mean(ci.bca[,1]<=mle & ci.bca[,2]>=mle))

## -----------------------------------------------------------------------------
library(bootstrap)
n <- nrow(scor)
theta <- function(x,i){
  val=eigen(cov(x[i,]))$values
  return(val[1]/sum(val))
}
theta.hat <- theta(scor,1:n)
theta.jack <- numeric(n)
for(i in 1:n){
  theta.jack[i] <- theta(scor,(1:n)[-i])
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(thetahat=theta.hat,bias.jack=bias.jack,
se.jack=se.jack),3)

## -----------------------------------------------------------------------------

library(DAAG);attach(ironslag)
n <- length(magnetic)
e1 <-e2 <-e3 <-e4 <-numeric(n)
for (k in 1:n){
  y <- magnetic[-k]
  x <- chemical[-k]
  
  J1 <- lm(y~x)
  yhat1 <- J1$coef[1]+J1$coef[2]*chemical[k]
  e1[k] <-magnetic[k] -yhat1
  
  J2 <- lm(y~x+I(x^2))
  yhat2 <- J2$coef[1]+J2$coef[2]*chemical[k]+J2$coef[3]*chemical[k]^2
  e2[k] <-magnetic[k] -yhat2
  
  J3 <- lm(log(y)~x)
  logyhat3 <- J3$coef[1]+J3$coef[2]*chemical[k]
  yhat3 <-exp(logyhat3)
  e3[k] <-magnetic[k] -yhat3
  
  J4 <- lm(y~x+I(x^2)+I(x^3))
  yhat4 <- J4$coef[1]+J4$coef[2]*chemical[k]+J4$coef[3]*chemical[k]^2+J4$coef[4]*chemical[k]^3
  e4[k] <-magnetic[k] -yhat4
}
c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))

## -----------------------------------------------------------------------------
J2

## -----------------------------------------------------------------------------
r1=summary(J1)$adj.r.squared
r2=summary(J2)$adj.r.squared
r3=summary(J3)$adj.r.squared
r4=summary(J4)$adj.r.squared
c(r1,r2,r3,r4)

## -----------------------------------------------------------------------------
J4

## -----------------------------------------------------------------------------
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
set.seed(123)
R <- 999
z <- c(x,y)
K <- 1:26
D <- numeric(R)
cm.test <- function(x,y){
    Fn=ecdf(x)
    Gm=ecdf(y)
    n <- length(x)
    m <- length(y)
    a <-numeric(n)
    b <-numeric(m)
    for (i in 1:n) {
        a[i]=(Fn(x[i])-Gm(x[i]))^2
    }
    for (i in 1:m) {
        b[i]=(Fn(y[i])-Gm(y[i]))^2
    }
    s=sum(a)+sum(b)
    W2=s*m*n/(m+n)^2
    return(W2)
}
D0 <- cm.test(x,y)
for (i in 1:R) {
    k <- sample(K, size = 14,replace = FALSE)
    x1 <- z[k]
    y1 <- z[-k] #complement of x1
    D[i] <- cm.test(x1, y1)
}
p <- mean(c(D0, D)>= D0)
p

## -----------------------------------------------------------------------------
set.seed(123)
n <- 100
x <- rnorm(n)
y <- rnorm(n)
R <- 999 #number of replicate
z <- c(x,y)#pooled sample
K <- 1:(n+n)
reps <- numeric(R)# storage for replicate
cor0 <- cor(x,y,method="spearman")
for(i in 1:R){
  k <- sample(K,size=n,replace=FALSE)
  x1 <- z[k]
  y1 <- z[-k] #complement of x1
  reps[i] <- cor(x1,y1,method="spearman")
}
p <- mean(c(cor0,reps)>=cor0)
p

## -----------------------------------------------------------------------------
cor.test(x,y)

## -----------------------------------------------------------------------------
library("coda")
f <- function(x) {
  return(1 / (pi * (1 + x^2)))
}
M_H <- function(N, initial_value) {
  x <- numeric(N)
  x[1] <- rt(1, df = 1)
  for (i in 2:N) {
    proposal <-rt(1, df = abs(x[i - 1]))
    alpha <- f(proposal) *dt(proposal,abs(x[i - 1]))/ f(x[i - 1])*dt(x[i - 1],abs(proposal))
    u <- runif(1)
    if (u<alpha) {
      x[i] <- proposal
    } 
       else {
      x[i] <- x[i - 1]
    }
  }
  return(x)
}
set.seed(123)
N <- 10000
initial_value <- 0
y<-M_H(N, initial_value)
x <- numeric(N-1000)
x<-y[1001:10000]
a1<-a2 <-numeric(10)
a1<-quantile(x,probs=seq(0.1,by=0.1))
a2<-qt(seq(0.1,by=0.1),1)
decile_comparison <- data.frame(
  Decile = seq(0.1, 1.0, by = 0.1),
  Observation_Deciles = a1,
  Theoretical_Deciles = a2
)
print(decile_comparison)

## -----------------------------------------------------------------------------
library(coda)
a <- 1
b <- 5
n <- 10
N <- 10000  
x <- 1
y <- 0
samples <- matrix(0, nrow = N, ncol = 2)
  
  for (i in 1:N) {
    x <- rbinom(1, n, y)
    y <- rbeta(1, x + a, n - x + b)
    samples[i, ] <- c(x, y)
  }

## -----------------------------------------------------------------------------
k <- 10 
chains <- lapply(1:k, function(i) M_H(N, initial_value))
mcmc_chains <- mcmc.list(lapply(chains, as.mcmc))
gelman_diag <- gelman.diag(mcmc_chains)
print(gelman_diag)

## -----------------------------------------------------------------------------
k <- 10
chains <- vector("list", k)
for (j in 1:k) {
  x <- 0
  y <- 0.5
  samples <- matrix(0, nrow = N, ncol = 2)
  
  for (i in 1:N) {
    x <- rbinom(1, n, y)
    y <- rbeta(1, x + a, n - x + b)
    samples[i, ] <- c(x, y)
  }
  chains[[j]] <- as.mcmc(samples)
}
gelman_diag <- gelman.diag(mcmc.list(chains))
print(gelman_diag)

## -----------------------------------------------------------------------------
c_k <- function(k,a){
  l <- sum(a^2)
  s <- seq(1,k,1)
  r <- sum(log(s))
  m <- (k+1)*log(l)-k*log(2)-log(2*k+1)-log(2*k+2)-r
  return(exp(m))
}
d_k <- function(k,a){
   d <- length(a)
   n <- lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1)
   return(exp(n))
}
a_k <- function(k,a){
  t <- (-1)^(k)*c_k(k,a)*d_k(k,a)
  return(t)
}

## -----------------------------------------------------------------------------
S_n <- function(n,a){
t <- numeric(n)
for (i in 1:n) {
    t[i] <- a_k(i,a)
}
d <- length(a)
s0 <- sum(a^2)*gamma((d+1)/2)*gamma(3/2)/(2*gamma((d+2)/2))
ss <- sum(t)+s0
return(ss)
}

## -----------------------------------------------------------------------------
a <- c(1,2)
n <- 100
S_n(n,a)

## -----------------------------------------------------------------------------
m = function (k) {g.integral = function(u, n) {
  (1 + u^2/(n-1))^(-n/2)
}
l = function (n, a) {
  sqrt(a^2 * n / (n + 1 - a^2))
}
g = function (n, a) {
    q.integral = function (u) {g.integral(u, n)}
    c = l(n - 1, a)
    2/sqrt(pi*(n-1)) * exp(lgamma(n/2)-lgamma((n-1)/2)) * integrate(q.integral, lower = 0, upper = c)$value}
  f = function (a) {
    t1 = g(k, a)
    t2 = g(k + 1, a)
    return (t1 - t2)
  }
  
  tol = 1e-2
  if (f(tol) < 0 && f(sqrt(k) - tol) > 0 || f(tol) > 0 && f(sqrt(k) - tol) < 0) {
    r = uniroot(f, interval = c(tol, sqrt(k)-tol))$root
  } else {
    r = NA
  }
  return(r)
}

s= sapply(c(5:20), function (k) {
  m(k)
})
s

## -----------------------------------------------------------------------------
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
tau <- 1  
lambda <- 1  
N <- 1000
tol <- 1e-6
for (iter in 1:N) {
  m <- Y[Y < tau]
  n <- Y[Y >= tau]
  E <- tau + 1 / lambda  
  lambda_new <- length(Y) / (sum(m) + length(n) * E)
  if (abs(lambda_new - lambda) < tol) {
    lambda <- lambda_new
    break
  }
  lambda <- lambda_new
}
lambda

## -----------------------------------------------------------------------------
library(boot) #for simplex function
A1 <- rbind(c(2,1,1),c(1,-1,3))
b1 <- c(2,3)
a <- c(4,2,9)
simplex(a=a,A1=A1,b1=b1,maxi = FALSE)

## -----------------------------------------------------------------------------

formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

## -----------------------------------------------------------------------------
data(mtcars)
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
for (i in formulas) {
  y <- lm(i,mtcars)
  print(summary(y))
}
models <- lapply(formulas, function(i) lm(i, data = mtcars))
lapply(models, summary)


## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})


## -----------------------------------------------------------------------------
data(mtcars)
f <- function() {
  i <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[i, ]
}
bootstraps <- lapply(1:10, function(i) f())
m <- function(data) {
  lm(mpg ~ disp, data = data)
}
x <- list()
for (i in 1:length(bootstraps)) {
  x[[i]] <- m(bootstraps[[i]])
}
y <- lapply(bootstraps, m)
lapply(x, summary)
lapply(y, summary)

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
x1 <- sapply(x, rsq)
y1 <- sapply(y, rsq)
print(x1)
print(y1)

## -----------------------------------------------------------------------------
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)

## -----------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
p <- sapply(trials, `[[`,"p.value")
print(p)

## -----------------------------------------------------------------------------
f <- function(FUN, ..., simplify = TRUE) {
  results <- Map(FUN, ...)
  vapply(results, identity, FUN.VALUE = simplify)
}

## -----------------------------------------------------------------------------
f <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y) || anyNA(x) || anyNA(y)) {
    break
  }
  m <- table(x, y)
  n <- outer(rowSums(m), colSums(m)) / sum(m)
  z <- sum((m - n)^2 / n)
  return(z)
}

## -----------------------------------------------------------------------------
fast_table <- function(x, y) {
  if (!is.integer(x) || !is.integer(y) || anyNA(x) || anyNA(y)) {
    break
  }
  m <- matrix(0, nrow = length(sort(unique(x))), ncol = length(sort(unique(y))),
                         dimnames = list(sort(unique(x)), sort(unique(y))))
  
  for (i in seq_along(x)) {
    m[as.character(x[i]), as.character(y[i])] <- m[as.character(x[i]), as.character(y[i])] + 1
  }
  
  return(m)
}

## -----------------------------------------------------------------------------
n <- 10  
a <- 1   
b <- 2   
iterations <- 10000 

x <- numeric(iterations)
y <- numeric(iterations)
x[1] <- sample(0:n, 1) 
y[1] <- runif(1)

for (i in 2:iterations) {
  
  y[i] <- rbeta(1, shape1 = x[i-1] + a, shape2 = n - x[i-1] + b)
  
  x[i] <- rbinom(1, size = n, prob = y[i])
}

plot(x, y, main = "Gibbs Sampler", xlab = "x", ylab = "y", pch = 16, col = "green")

## -----------------------------------------------------------------------------
library(microbenchmark)
n <- 10
a <- 1
b <- 2
iterations <- 10000

gibbs_x <- numeric(iterations)
gibbs_y <- numeric(iterations)
gibbs_x[1] <- sample(0:n, 1)
gibbs_y[1] <- runif(1)

for (i in 2:iterations) {
  gibbs_y[i] <- rbeta(1, shape1 = gibbs_x[i-1] + a, shape2 = n - gibbs_x[i-1] + b)
  gibbs_x[i] <- rbinom(1, size = n, prob = gibbs_y[i])
}

direct_y <- rbeta(iterations, shape1 = a, shape2 = n + b - a)
direct_x <- rbinom(iterations, size = n, prob = direct_y)

qqplot(gibbs_y, direct_y, main = "Gibbs采样与直接采样的y分布QQ-图", xlab = "Gibbs采样器", ylab = "直接采样")
abline(0, 1, col = "green")

qqplot(gibbs_x, direct_x, main = "Gibbs采样与直接采样的x分布QQ-图", xlab = "Gibbs采样器", ylab = "直接采样")
abline(0, 1, col = "green")

gibbs_time <- microbenchmark({
  gibbs_x <- numeric(iterations)
  gibbs_y <- numeric(iterations)
  gibbs_x[1] <- sample(0:n, 1)
  gibbs_y[1] <- runif(1)
  for (i in 2:iterations) {
    gibbs_y[i] <- rbeta(1, shape1 = gibbs_x[i-1] + a, shape2 = n - gibbs_x[i-1] + b)
    gibbs_x[i] <- rbinom(1, size = n, prob = gibbs_y[i])
  }
}, times = 10)

direct_time <- microbenchmark({
  direct_y <- rbeta(iterations, shape1 = a, shape2 = n + b - a)
  direct_x <- rbinom(iterations, size = n, prob = direct_y)
}, times = 10)

print(gibbs_time)
print(direct_time)

