
# SAS - https://welcom  e.oda.sas.com/home
# https://stackoverflow.com/questions/11561284/classes-in-r-from-a-python-background

# modules ----
library(readxl)
library(Hmisc) #describe()
library(beeswarm) # unit 7
library(epitools) # unit 1, riskratio(), oddsratio()
library(ggplot2)
library(abind) # unit 2

# datasets ----------------------------------------------------------------
library(readxl)
classdata <- read_excel("Dataset.xls")
kyph <- read_excel("kyphosis.xls")
hw5 <- read_excel("HOMEWORK5.xlsx")

# INTRODUCTION ------------------------------------------------------------
formals() body()
data.frame()
col1 = classdata[1] # col1_4 = classdata[1:4]
row1 = classdata[1,] # row1_4 = classdata[1:4,]
subset = classdata[classdata$Varsity==1,]
colnames = names(classdata) # names(classdata)[1] = "ID"
hist(classdata$coffee, breaks=seq(0,50,by=3), col="red", xlab="Coffee")

# Unit 1: contingency tables ----------------------------------------------
tt_table <- function(a,b,c,d,row="") {
  data = matrix(c(a,b,c,d), nrow=2, ncol=2, byrow=TRUE)
  table = as.table(data) # convert to a table
  rownames(table) = c("+", "-") # outcome/disease
  colnames(table) = c(paste("c+",row), "c-") # cases/exposure
  
  print(table) 
  cat("\n")
  
  # Fisher's Exact
  cat(fisher.test(table)$method, " (1-tailed)","\n")
  print(fisher.test(table)$p.value)
  
  # Odds Ratio CI
  cat("\n")
  cat("Odds Ratio")
  library(epitools)
  print(oddsratio.wald(table, rev="both")$measure)
  return(table)
}
table <-tt_table(28,511,53,1328, "(dep)")
table <-tt_table(7,30,24,45)

addmargins(table) # generate the 2x2 contingency table
prop.table(table) # freq

#tests
fisher.test(table)
chisq.test(table, correct=F)
mcnemar.test(table)
oddsratio.wald(table, rev="both") # equivalent??? to oddsratio(table, rev="both")
riskratio(table, rev="both") # rev="both": bc fx uses input r&c that are reversed relative to orig table
# what is your increased risk of outcome if you're case

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
ORfunction(95,8,32,88,1045)


# Unit 2: confounding,  interaction,  mediation,  mantel-haenszel ---------



# Unit 3: logistic regression I -------------------------------------------

# hw
temp <- c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58)
td <- c(0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1)
df <- data.frame(temp,td)
glm.fit <- glm(td~temp, family="binomial", data=df)
summary(glm.fit)
