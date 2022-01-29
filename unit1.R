
# SAS: https://welcome.oda.sas.com/login

# modules ----
library(Hmisc) #describe()
library(readxl)
library(beeswarm) # unit 7
library(epitools) # unit 7, riskratio(), oddsratio()
library(ggplot2)

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

#vector reference
attach(classdata)
# can just mention column vectors in here, don't need classdata$...
detach(classdata)

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

# Unit 1: simple stats ---------------------------------------------------------
summary() # boxplot info
describe() # n missing, distinct, info, mean, gmd
sd()
mean() # na.rm=TRUE is needed to remove NA values before calculating
median()
IQR()
hist(col="", xlim=c(), breaks=)
# wilcoxon rank sum test

# stats using frequency
mean(rep(df1$x, df1$freq))
sd(rep(df1$x, df1$freq))
IQR(rep(df1$x, df1$freq))
var(rep(df1$x, df1$freq))


# Unit 2: plotting ------------------------------------------------------------
plot(x, y, xlab, ylab, pch=16, col="") #type="b", type="l"
lines(x, y, type="l")
as.character()
as.numeric()
as.numeric(as.character()) # to convert factor to number

# OR to RR
RR = OR/((1-p) + (p*OR))


# Unit 3: prob sim --------------------------------------------------------
sample()
table()
any()
sum()

days = sample(365, size=30, replace=TRUE)
table(days)
any(table(days)>1)

matches = 0
for (i in 1:10000) {
  days = sample(365, size=274, replace=TRUE)
  matches[i] = any(table(days) > 1)
}
sum(matches)/10000



# Unit 4: prob distr ------------------------------------------------------
dbinom() # d...() probability density function
pbinom() # p...() cumulative distribution function (CDF)
         # q...() quantile function (the inverse of the CDF)
set.seed()
rbinom() # r...() gives random numbers from the distribution (use set.seed(n) first in this case)
hist()
mean()
sd()
sum()

dbinom(60,100,0.5)
1 - pbinom(59, 100, 0.5)
pbinom(59, 100, 0.5, lower.tail=FALSE)

# simulates results of 30,000 experiments with 100 virtual coin flips.
set.seed(1)
NumHeads = rbinom(30000, 100, 0.5)   
hist(NumHeads, breaks=seq(25,75,by=1), col="red", xlab="Number of Heads (per 100 Coin Tosses)")

# Normal Distribution
# P(X≤Z) = pnorm(Z)
# Z = (area) = qnorm(area) # Z value that corresponds to a given area of a standard normal (probit function):
# To generate n random numbers (μ=0, σ=1), rnorm(n)

# Exponential
# P(X=k) = dexp(k,)
# P(X≤k) = pexp(k,)
# To generate n random numbers, rexp(n)

# Uniform
# P(X=k) = dunif(k)
# P(X≤k) = punif(k)
# To generate n random numbers, runif(n)

# Binomial
# P(X=k) = dbinom(k,N,p)
# P(X≤k) = pbinom(k,N,p)
# To generate n random numbers  rbinom(k,N,p)

# Poisson
# P(X=k) = dpois(k,)
# P(X≤k) = ppois(k,)


# Unit 5: statistical inference -------------------------------------------
rnorm()
qt(p, df) # p = vector of probs
mean()
sd()
hist()

# Set the parameters of the simulation
repeats = 10000
n = 10 # Generate 10,000 samples of 10 

# population: µ = 65in & σ = 5in
# For each sample, calculate the µ and σ of height in sample
samplemean = 0
samplesd = 0
for (i in 1:repeats) {
  heights = rnorm(n,65,5)
  samplemean[i] = mean(heights)
  samplesd[i] = sd(heights)
}

# Explore the distribution of the sample means
hist(samplemean, col="red")
mean(samplemean)  # mean of sample means
sd(samplemean)    # std of the sample means (standard error?)

# small sample size -> use T rather than Z for building confidence intervals
t = qt(.975,(n-1))
z = -qnorm((1-.975)/2)

# For each sample, calculate the 95% confidence interval. 
# Calculate the percent of confidence intervals that contain the true mean (65 inches)
stderr = samplesd / sqrt(n)
lower = samplemean - t*stderr
upper = samplemean + t*stderr

mean(lower>65 | upper<65) # % of CI that miss the true mean

# Why a T rather than a Z?
Z = (samplemean-65) / (5/sqrt(n))
hist(Z, col="red") # follows a Z when you use the true SD (=5 here)
T = (samplemean-65) / stderr
hist(T, col="red") # follows a T when you use the sample standard deviation


# MIDTERM -----------------------------------------------------------------

# Q1
means=NA
for (i in 0:162) {
  means[i] = round(i/27, digits=2)
}


sample = 0;
means = 0
for (i in 1:100000) {
  sample = sample(c(0:6), size =27,replace = TRUE)
  means[i] = mean(sample)
}

table(round(means, 2))


# 2
sample = 0;
count = 0
for (i in 1:100000) {
  sample = sample(rep(c("star", "-"), times=c(20,180)), size=30,replace = TRUE)
  if (sample[30] == "star") {
    count = count + 1
  }
}
prob = count/100000

# 2, correct
p.final = NA   #initialize vector of probabilities
for (i in 1:100000) {
  #Set starting numbers of stars and chocolates
  stars=20
  chocolates=200
  
  #Simulate 30 chocolates being removed
  for (j in 1:30) {
    p = stars/chocolates     #current probability of a star
    pickstar = rbinom(1,1,p) #whether the current student gets a star
    
    #Update number of stars and chocolates
    if (pickstar==1) { stars = stars - 1 }
    chocolates = chocolates - 1
  }
  
  p.final[i] = p   #records the probability for the 30th chocolate
}
mean(p.final)

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

ORs = NA
for (i in 1:50) {
  p = i/100
  ORs[i] = 2 * ( (1-p) / (1-2*p) )
}
plot(ORs, xlab="Risk in the unexposed group (%)", ylab="Odds ratio", type="l")




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




# Unit 6: pvalues ---------------------------------------------------------
rnorm()
t.test() # $p.value
sum()
hist()
2*pnorm(Z, lower.tail=FALSE) # p-value (two-tailed)

n = 50 #sample size

# TYPE 1 ERROR - drug doesn't work
# H0 = no diff

pvals = 0    #Initialize an empty variable for the output

for (i in 1:10000) {
  #Create two randomly selected groups of size 50
  drug = rnorm(n, mean=250, sd=25)
  control = rnorm(n, mean=250, sd=25)
  
  #Apply the t.test() function, retrieve the p value with $p.value, and save 
  #the number in the pvals array at position i
  pvals[i] = t.test(drug,control)$p.value
}

sum(pvals < 0.05) / 10000
hist(pvals, col="red")


# Type II error (change to drug mean=240)
pvals = 0    #Initialize an empty variable for the output
u = 250 #mean

for (i in 1:10000) {
  #Create two randomly selected groups of size 50
  drug = rnorm(n, mean=u-10, sd=25) #10 points better
  control = rnorm(n, mean=u, sd=25)
  
  #Apply t.test() function, retrieve p value with $p.value
  pvals[i] = t.test(drug,control)$p.value
}

sum(pvals > 0.05) / 10000
hist(pvals, col="red")

#Retry with different sample sizes other than n=50 per group!


# Unit 7: statistical tests -----------------------------------------------
boxplot()
beeswarm()
shapiro.test()
t.test()
aov()
wilcox.test()
table()
prop.table()
chisq.test()
fisher.test()
riskratio()
oddsratio()

#Draw boxplot with jittered datapoints superimposed, homework times in book smart vs. street smart students
library(beeswarm)
boxplot(homework ~ IsBookSmart, data=classdata, cex.lab=.8, cex.axis=.8,  
        xlab='Street Smart=0, Book Smart=1', ylab='Homework hrs per week')
beeswarm(homework ~ IsBookSmart, data = classdata,
         col = rainbow(8), cex=1, pch = 16, add=TRUE)

#Test homework for normality
shapiro.test(classdata$homework) # significant p-values indicate non-normal data
# H0 = data are normally distributed
# if p < 0.05, then H0 is rejected
# bias by sample size. The larger the sample, the more likely you’ll get a statistically significant result

#t-test 
t.test(homework~IsBookSmart, data=classdata) # pooled
t.test(homework~IsBookSmart, data=classdata, var.equal=TRUE) # unpooled
# street smart does more homework, not significant

#ANOVA (equivalent to t-test, pooled)
model = aov(homework~IsBookSmart, data=classdata)
summary(model)

#Wilcoxon rank-sum test: Split data into booksmart, streetsmart
booksmart = classdata[classdata$IsBookSmart==1,]
streetsmart = classdata[classdata$IsBookSmart==0,]
wilcox.test(x=booksmart$homework, y=streetsmart$homework, exact=FALSE)

#Paired t-test, math love vs. writing love
t.test(classdata$Mathlove, classdata$WritingLove, paired=TRUE)
# Math love is higher than writing love, though not significantly so (p=.099)

#2x2 table, row variable = varsity sports; column variable = book smart vs. street smart
table = table(classdata$varsity, classdata$IsBookSmart)  #generates 2x2 table 
table				#displays table with counts

prop.table(table)		#displays table with frequency
chisq.test(table)		#perform a chi-squared test, warning for sparse data
fisher.test(table)	#perform a Fisher’s exact test

riskratio(table)
oddsratio(table)

# Unit 8: linear regression -----------------------------------------------
plot()
lines()
loess.smooth()
cor.test()
glm()
residulas()

#Draw a scatter plot of ratings of former Presidents Obama and Bush Jr; superimpose a smoothing line
plot(classdata$bushjr, classdata$obama,col="red", pch=16, cex=1, cex.lab=1.5,
     xlab="Ratings of Bush Jr", ylab="Ratings of Obama")
lines(loess.smooth(classdata$bushjr, classdata$obama, span=.8), col="red")
# homogeneity of variances looks reasonable (equal scatter around the line)

#Correlation coefficient
cor.test(classdata$bushjr, classdata$obama) # Pearson's is default

#Simple linear regression model
model = glm(obama~bushjr, data=classdata)
summary(model)
# Beta coefficient = -.37, p=.0002
# Interpretation: For every 1-point increase in liking of Bush Jr, 
# there is an average .37 point decrease in liking of Obama.

plot(model) #Residuals look reasonably normally distributed.
hist(residuals(model), col="red")
shapiro.test(residuals(model))

# Multiple linear regression model
model2 = glm(obama~bushjr+politics, data=classdata)
summary(model2)
# The beta coefficient for bushjr was -.37 before adjusting for politics; 
# it’s -.04 after adjusting for politics. This means that nearly the entire 
# relationship between bushjr and Obama ratings was driven by political leaning. 
# The beta coefficient for politics is .65,
# indicating that every 1-point increase in liberal leaning is 
# associated with a .65-point increase in liking for Obama.

plot(model2)
hist(residuals(model2), col="red")
shapiro.test(residuals(model2))


# Unit 9: prin of regression ----------------------------------------------
qplot()
glm()
as.factor()
subset()

#Make IsBookSmart a factor for graphing
classdata$IsBookSmart = as.factor(classdata$IsBookSmart) # improves auto-legend in qplot
qplot(politics, obama, data=subset(classdata, !is.na(IsBookSmart)), 
      color=IsBookSmart, geom=c("point","smooth"), method="lm", se=TRUE, 
      ylab="Obama Ratings (0-100)", 
      xlab="Political Leaning (0=conservative, 100=liberal)")

#Fit a linear regression model with interaction
model = glm(obama ~ IsBookSmart + politics + politics*IsBookSmart, 
            data=classdata) # var1*var2 specifies an interaction between the variables 
summary(model)

# Intercept (7.27): the intercept for the street-smart group.
# IsBookSmart (26.5): the difference in the intercepts of the book-smart and street-smart groups. 
#     Intercept for book smart group = 7.27 + 26.5 = 33.27
# politics (.996): slope in the street-smart group
# IsBookSmart*politics (-.404): diff in slopes of two groups. 
#     Slope for book smart group = .996 - .404 = .592


# FINAL -------------------------------------------------------------------
#Set up x-axis
odds = seq(0, 1, 0.001)
prob = odds / (1 + odds)

#Calculate PPVs for each significance level (can also use a for loop)
PPV1 = 0.5*prob / ( 0.5*prob + 0.05*(1-prob) )
PPV2 = 0.5*prob / ( 0.5*prob + 0.01*(1-prob) )
PPV3 = 0.5*prob / ( 0.5*prob + 0.005*(1-prob) )

#Create plot
plot(NULL, xlab="Pre-Study Odds", ylab="PPV", xlim=c(0,1), ylim=c(0,1))
points(odds, PPV1, pch=16, col="blue")
points(odds, PPV2, pch=16, col="green")
points(odds, PPV3, pch=16, col="red")



#################
#################