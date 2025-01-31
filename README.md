#jackknife and bootstrap method of estimation
#q.1
data=c(8,26,6.33,10.4,5.27,5.35,5.61,6.12,6.19,5.2,7.01,8.74,7.78,7.01,6,6.5,8,5.12,7.41,6.52,6.21,12.28,5.6,5.38,6.6,8.74)
m=mean(data)
m
sd=sqrt(var(data))
sd
cv=(sd/m)*100
cv
n=1000
boot_mean=numeric(n)
boot_vars=numeric(n)
boot_cvs=numeric(n)
for (i in 1:n){
boot_sample=sample(data,size=length(data),replace=T)
boot_mean[i]=mean(boot_sample)
boot_vars[i]=var(boot_sample)
boot_cvs[i]=(sd(boot_sample))*100/mean(boot_sample)
}
boot_mean
boot_vars
boot_cvs
hist(boot_cvs)
lwq=quantile(boot_cvs,0.025)
lwq
upq=quantile(boot_cvs,0.095)
upq
bias=mean(boot_cvs)-cv
bias
#q.3
data=c(22,26,58,54,30,35,12,28)
m=mean(data)
m
install.packages("moments")
library(moments)
sd=sqrt(var(data))
sd
cv=(sd/m)*100
cv
n=8
sk=skewness(data)
sk
boot_mean=numeric(n)
boot_vars=numeric(n)
boot_cvs=numeric(n)
boot_skewness=numeric(n)
for (i in 1:n){
boot_sample=sample(data,size=length(data),replace=T)
boot_mean[i]=mean(boot_sample)
boot_vars[i]=var(boot_sample)
boot_cvs[i]=(sd(boot_sample))*100/mean(boot_sample)
boot_skewness[i]=skewness(boot_sample)
}
boot_mean
boot_vars
boot_cvs
boot_skewness
bias=mean(boot_skewness)-sk
bias
sd=sd(boot_skewness)
sd
#q.2
x=c(24,26,32,36,43,52,62,56,52,21)
x
y=c(22,28,5,18,14,14,8,8,10,24)
y
co=cor(x,y)
co
n=10
boot_co=numeric(n)
for (i in 1:n){
boot_samplex=x[-i]
boot_sampley=y[-i]
boot_co[i]=cor(boot_samplex,boot_sampley)
}
boot_co
m=mean(boot_co)
m
bias=m-co
bias
se=sqrt((n-1)/n*sum((boot_co-m)^2))
se
install.packages("boot")
library(boot)
jackknife_result <- jackknife(data, mean)

# Print jackknife result
print(jackknife_result)

#q.4
x=c(32,4,16,7,12,27)
y=c(2300,30,1500,150,700,1800)

