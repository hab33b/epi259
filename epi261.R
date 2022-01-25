# modules ----
library(readxl)
library(Hmisc) #describe()
library(beeswarm) # unit 7
library(epitools) # unit 1, riskratio(), oddsratio()
library(ggplot2)

# INTRODUCTION ------------------------------------------------------------
library(readxl)
formals() body()
data.frame()
classdata <- read_excel("Dataset.xls")
col1 = classdata[1] # col1_4 = classdata[1:4]
row1 = classdata[1,] # row1_4 = classdata[1:4,]
subset = classdata[classdata$Varsity==1,]
colnames = names(classdata) # names(classdata)[1] = "ID"
hist(classdata$coffee, breaks=seq(0,50,by=3), col="red", xlab="Coffee")

# Unit 1: contingency tables ----------------------------------------------
tt_table <- function(a,b,c,d) {
  data = matrix(c(a,b,c,d), nrow=2, ncol=2, byrow=TRUE)
  table = as.table(data)   #convert to a table
  rownames(table) = c("case", "control") # exposure/cases
  colnames(table) = c("+", "-") # disease/outcome
  table <<- table
  table
}
tt_table(30,500,50,1300)
addmargins(table) # generate the 2x2 contingency table
prop.table(table) # freq
chisq.test(table)
fisher.test(table)
oddsratio(table, rev="both") # equivalent??? to oddsratio(table, rev="both")
riskratio(table, rev="both") # rev="both": bc fx uses input r&c that are reversed relative to orig table

# Function to calculate XX% confidence limits for an odds ratio 
# for a given confidence level (entered as a whole number, eg “95”) 
# and the 2x2 cell sizes: a,b,c,d, where a is the diseased, exposed cell
ORfunction = function(confidence,a,b,c,d) {
  # enter confidence percent as a whole number, e.g. "95"
  OR = (a*d) / (b*c)
  lnOR = log(OR)
  error = sqrt(1/a + 1/b + 1/c + 1/d)
  Z = -qnorm((1 - confidence/100)/2) # gives left hand Z score, multiply by negative
  lower = exp(lnOR - Z*error)
  upper = exp(lnOR + Z*error)
  output = c(OR, lower, upper)
  names(output) = c("OR", "lower", "upper")
  output
}
ORfunction(95,30,500,50,1300)



# Unit 2: confounding,  interaction,  mediation,  mantel-haenszel ---------



# Unit 3: logistic regression I -------------------------------------------

# hw
temp <- c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58)
td <- c(0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1)
df <- data.frame(temp,td)
glm.fit <- glm(td~temp, family="binomial", data=df)
summary(glm.fit)
