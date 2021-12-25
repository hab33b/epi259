# SAS: https://odamid-usw2.oda.sas.com/SASStudio/main?locale=en_US&zone=GMT-08%253A00&ticket=ST-209379-K3HYTAFvaS7iHdaJnvLi-cas

# modules ----
library(Hmisc)
library(readxl)

# datasets ----
classdata <- read_excel("../../../Downloads/classdata.xlsx")
finaldata <- read_excel("../../../Downloads/finaldata_fall_2021.xlsx") # FINAL

# INTRODUCTION ------------------------------------------------------------
id <- c(1,2,3,4,5)
coffee <- c(16, 0, 2, 0 ,2)
coffee
max(coffee)
which.max(coffee)  #location
df <- data.frame(id, coffee) #dataframe

# directory
setwd()
getwd()

#exploring dataset
str(classdata)
head(classdata)
tail(classdata)
rownames(classdata)
colnames(classdata)
names(classdata)
nrow(classdata)
ncol(classdata)
dim(classdata)

#cleaning data
classdata$smoking_num <- ifelse(classdata$smoking=='Yes',1,0)

#simulation
repeats <- 10000
n <- 10
samplemean <- 0

for (i in 1:repeats){
  random_num <- rnorm(n,25,5)
  samplemean[i] <- mean(random_num)
}
head(samplemean)

# Unit 1 ----
# module 4 
mean(c(1, 5, 7, 8, 9))
median(c(-8, -10, -12, -16, -18, -20, -21, -24, -26, -30, +4, +3, 0, -3, -4, -5, -11, -14, -15, -300))
IQR(c(+4, +3, 0, -3, -4, -5, -11, -14, -15, -300))
sd(c(-8, -10, -12, -16, -18, -20, -21, -24, -26, -30))
IQR(c(-8, -10, -12, -16, -18, -20, -21, -24, -26, -30))
# wilcoxon rank sum test

# Unit 1 HW
x <- c(0,1)
freq <- c(47,53)
df1 <- data.frame(x, freq)
Mean=sum(df1$x*df1$freq)/sum(df1$freq)
mean(rep(df1$x, df1$freq))
summary(df1)
sd = sqrt(sum((df1$x −Mean)**2*df1$freq)/(sum(df1$freq) −1))
sd(rep(df1$x, df1$freq))
IQR(rep(df1$x, df1$freq))
var(rep(df1$x, df1$freq))

summary(df1)







# Unit 2 ------------------------------------------------------------------

# OR to RR
OR = 5.7
p = .17
RR = OR/((1-p) + (p*OR))

RR = R1/R2

# hw
rate_diff = 34/2.4e6*100000 - 6/1.5e6*100000
RR = (34/2.4e6*100000) / ( 6/1.5e6*100000)
rate_diff2 = 34/2.4e6*100000 - 1/1e6*100000
100000/rate_diff
data <- read.csv("../../../Downloads/results.csv")
lines(data$Year, data$Age.Adjusted.Rate)



t = pi/4
x <- c(1,0,0,0,cos(t), -sin(t), 0, sin(t), cos(t))
y <- c(cos(t), 0, sin(t), 0, 1, 0, -sin(t), 0, cos(t))
z <- c(cos(t), -sin(t), 0, sin(t), cos(t), 0, 0, 0, 1)

# matrices
Rx <<- matrix(x, nrow = 3, byrow = T)
Ry <<- matrix(y, nrow = 3, byrow = T)
Rz <<- matrix(z, nrow = 3, byrow = T)
p <<- matrix(c(1,-1,1))

# rotate
Rx %*% Ry %*% p
Ry %*% Rx %*% p

y <- c(cos(t), 0, sin(t),0, 0, 1, 0, 0, -sin(t), 0, cos(t),0,0,0,0,1)
Ry <- matrix(y, nrow = 4, byrow = T)
p <- matrix(c(3,1,3,1))

matches = 0
for (i in 1:10000) {
  dice = sample(365, size=100, replace=TRUE)
  matches[i] = any(table(dice) >= 4)
}
  
sum(matches)/10000


x <- c(0, 1, 2, 3, 4, 5)
px <- c(0, .2, .3, .3, .1, .1)

mean = sum(x*px)
var = 0

for (i in 1:length(px)) {
  var = var + (x[i] - mean) ^ 2 * px[i]
  
}
var
std = sqrt(var)


dbinom(1, 5, 0.5) #pdf
pbinom(2, 8, 0.25) #cdf

for (i in 1:30000) {

  x = x+ dbinom(1, 1000, 0.15) - dbinom(1, 1000, 0.05)
}




meandiff = 0
for (i in 1:10000) {
  girls = rnorm(30, 60, 10)
  boys = rnorm(30, 60, 10)
  meandiff[i] = mean(girls) - mean(boys)
}

count = 0
for (value in meandiff) {
  if (value >= 5 | value <= -5) {
    count = count + 1
  }
}

hist(meandiff)













# MIDTERM -----------------------------------------------------------------


Q1

sample = 0;
means = 0
for (i in 1:100000) {
  sample = sample(c(0:6), size =27,replace = TRUE)
  means[i] = mean(sample)
}

table(round(means, 2))


Q2

sample = 0;
count = 0
for (i in 1:100000) {
  sample = sample(rep(c("star", "-"), times=c(20,180)), size=30,replace = TRUE)
  if (sample[30] == "star") {
    count = count + 1
  }
}
prob = count/100000


Q7

pref = seq(0, 0.4, 0.01)
risk = 2
odds = vector()

for(p in pref){
  odd = (risk-risk*p)/(1-risk*p)
  odds <- append(odds, odd)
}
print(odds)

plot(pref, odds, type = "l")




comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}
x = 7
comb(x - 1,3)*(0.55)**4*(0.45)**(x-4) + comb(x - 1,3)*(0.45)**4*(0.55)**(x-4)

pvals = 0
for (i in 1:10000) {
  drug = rnorm(60, mean=240, sd=25)
  control = rnorm(60, mean=250, sd=25)
  pvals[i] = t.test(drug, control)$p.value
}
sum(pvals<0.01)/10000


# UNIT 7 ------------------------------------------------------------------
# wednesday exercise:
data <- as.data.frame(read_excel("../../../Downloads/Unit7Data.xlsx"))
length(which(data$fracture == 0))
length(which(data$lowbmd == 1))
# cor(data$fracture,data$bmdzscore) # wrong
mean(data$bmdzscore[data$fracture == 0])
sd(data$bmdzscore[data$fracture == 0])
mean(data$bmdzscore[data$fracture == 1])
sd(data$bmdzscore[data$fracture == 1])
x = data$bmdzscore[data$fracture == 0]
y = data$bmdzscore[data$fracture == 1]

library(beeswarm)
boxplot(bmdzscore ~ fracture, data, 
        cex.lab=1.5, cex.axis=1.5, 
        xlab='Street Smart=0, Book Smart=1',
        ylab='Homework hrs per week')
beeswarm(bmdzscore ~ fracture, data,
         col = rainbow(8), cex=2, pch = 16, add=TRUE)



dbinom(1, 6, 0.5) + dbinom(5, 6, 0.5)



# UNIT 8 ------------------------------------------------------------------
classdata <- as.data.frame(read_excel("../../../Downloads/classdata.xlsx"))
plot(classdata$bushsr, classdata$bushjr,
     col="red", pch=16)
lines(loess.smooth(classdata$bushsr, classdata$bushjr, span=.80), col="red")

model2 = glm(classdata$bushjr~classdata$bushsr+classdata$politics)
summary(model2)

obama = optimism
classdata$varsity <- as.factor(classdata$varsity)
qplot(alcohol, optimism, data=subset(classdata, !is.na(varsity)), 
      color=varsity, geom=c("point","smooth"), method="lm", se=FALSE, 
      ylab="optimism", xlab="alcohol (drinks/week)")

model = glm(optimism~varsity+alcohol+varsity*alcohol, data=classdata)
summary(model)

R = seq(0, 1, 0.01)
power = 0.5
ppv=0

PPVS = vector()
PPVS2 = vector()
PPVS3 = vector()

for(i in R){
  ppv = (power)*i/(i - power*i + 0.05)
  PPVS <- append(PPVS, ppv)
}
print(PPVS)

for(i in R){
  ppv = (power)*i/(i - power*i + 0.01)
  PPVS2 <- append(PPVS2, ppv)
}

for(i in R){
  ppv = (power)*i/(i - power*i + 0.005)
  PPVS3 <- append(PPVS3, ppv)
}

plot(R, PPVS, type = "l")
lines(R, PPVS2, col = "red")
lines(R, PPVS3, col = "green")

