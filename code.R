library(datasets)
data("airquality")
df=airquality
head(df)

dim(df)

#Converting days and months vars into factors
df$Month=as.factor(df$Month)
df$Day=as.factor(df$Day)

#getting summary of all vars
summary(df)
library(xtable)
for (i in 1:5){
  xtable(summary(list(df[,i])))
}


#histograms and boxplots for the 4 vars
col=colnames(df[1:4])
par(mfrow=c(2,4))
for (i in 1:4){   ### can also do: colSums(is.na(df))
  #print(paste("Number of NA in",as.character(colnames(df[1:4]))[i],"=",as.character(sum(is.na(df[,i]))),sep=" "))
  hist(df[,i],breaks=20,xlab=col[i],main="Histogram")
  boxplot(df[,i],xlab=col[i],main="Boxplot")
}
#Scatterplot with different colors
pairs(df[1:4],col=c("Blue","Red","Green","Orange"))

#Correlation Matrix
rows=rowSums(is.na(df))==0
corrmat=round(cor(df[rows,colnames(df)[1:4]]),2)
xtable(corrmat)

#heatmap correlation
library(heatmaply)
heatmaply_cor(corrmat,xlab="airquality Features",ylab="airquality Features",k_col=1,k_row=1)

par(mfrow=c(1,4))

#Comparing ozone by month
boxplot(df$Ozone~df$Month,main="Ozone",ylab="Ozone",xlab="Months")

#Comparing Solar radiation by month
boxplot(df$Solar.R~df$Month,main="Solar.R",ylab="Solar.R",xlab="Months")

#Comparing windspeed by month
boxplot(df$Wind~df$Month,main=" Wind",ylab="Wind",xlab="Months")

#Comparing temperature by month
boxplot(df$Temp~df$Month,main="Temperature",ylab="Temperature",xlab="Months")

#Temperature means for all months
c=1:5
par(bg="white")
for(i in 1:5){
  c[i]=mean(subset(df,Month==(i+4))$Temp)
  print(paste("Mean Temperature for Month",as.character(i+4),as.character(round(c[i],2),sep=" ")))
}
par(mfrow=c(1,1))
y_y=5:9
plot(y_y,c,xlab="Month",ylab="Temperature averages")

#Computing cdf, density and qqplots for our 4 vars
par(col="Black")
for (i in 1:4){
  par(mfrow=c(1,3))
  plot(ecdf(df[,i]))
  plot(density(df[(!is.na(df[,i])),i]))
  qqnorm(df[(!is.na(df[,i])),i])
  qqline(df[(!is.na(df[,i])),i],col="Red",lwd=2)
}

###gonna do Kolmogorov Smirnov test to compare 2 dists
## for normality

for(i in 1:4){
  temp=(df[,i]-mean(df[,i],na.rm=TRUE))/sd(df[,i],na.rm=TRUE)
  print(paste("KS test results for",as.character(colnames(df[1:4]))[i],":",sep=" "))
  print(ks.test(temp,"pnorm"))
}