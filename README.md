# pract
#jackknife and bootstrap method of estimation
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
bias(data)
