#Logit plot function
  #author: Matthew Sigurdson (msigurds@stanford.edu)

#Three required arguments:
  #data: data frame containing data
  #predictor: the name of the column to use as the predictor (takes string input)
  #outcome: the name of the column to use as the outcome (takes string input)

#Three optional arguments:
  #numIntervals: the number of intervals to divide the data into (default is 10)
  #spline: whether a smoothing spline should be plotted to the data (default is TRUE)
  #spar: what the smoothing parameter should be for the spline (default is 0.7)

#To avoid interpreting random noise, trying several values for numIntervals is recommended. If fewer points than expected are seen on the plot, it is most likely due to infinite logits (that is, probabilities of 0 or 1) occurring for the current value of numIntervals.

#The function was designed and tested mainly for cases where numIntervals divides evenly into the number of columns in the dataset. If it doesn't, the function should drop the samples with the highest value of the predictor until it divides evenly. 

#Linearity in the logit requires the data to not contain missing values for either predictor or outcome.
#To select only rows with complete data, use the complete.cases() function. Syntax: data[complete.cases(data$predictorName,data$outcomeName),] - or data[complete.cases(data),] to apply to all columns at once. 

#Example usage: logit.plot(mydata, "predictorName", "outcomeName", 20)

logit.plot = function(data,predictor,outcome,numIntervals=10,spline=TRUE,spar=0.7) {
  #Check inputs
  if (!is.character(predictor)) {stop("The predictor must be a string.")}
  if (!is.character(outcome)) {stop("The outcome must be a string.")}
  
  #Initialize vectors
  logits = rep(0,numIntervals)
  means = rep(0,numIntervals)
  
  #Define outcome and predictor columns (renames them to make the code below simpler)
  outcomeIndex = which(colnames(data)==outcome)
  predictorIndex = which(colnames(data)==predictor)
  names(data)[outcomeIndex] = "outcome"
  names(data)[predictorIndex] = "predictor"
  
  #Check for missing data
  if (any(is.na(data$predictor))) {stop("The predictor contains missing data.")}
  if (any(is.na(data$outcome))) {stop("The outcome contains missing data.")}
  
  #Sort data in ascending order of predictor
  data=data[order(data$predictor),]
  
  #Define the intervals
  intervalLength = floor(dim(data)[1] / numIntervals)
  #floor() enforces rounding down, in case numIntervals doesn't divide evenly into dim(data)[1]
  
  if ((dim(data)[1] / numIntervals) - floor(dim(data)[1] / numIntervals) != 0) {
    warning("The number of intervals does not divide evenly into the size of the dataset. Some data points will be omitted.")
  }
  
  #Define the starting index of each section
  intervalStarts = c()
  for (k in 1:numIntervals) {
    intervalStarts[k] = 1 + (k-1)*intervalLength
  }
  
  #Loop over each section
  for (j in intervalStarts) {
    positive=0
    negative=0
    sum=0
    
    #Loop over each data point in the section
    for (i in c(j:(j+intervalLength-1))) {
      if (data$outcome[i] == 1) {             #outcome variable is 1
        positive = positive + 1
      } else if (data$outcome[i] == 0) {      #outcome variable is 0
        negative = negative + 1
      } else {
        stop("The outcome column must be binary.")
      }
      
      sum = sum + data$predictor[i]   #adding the predictor values so we can calculate the mean
    }
    
    x = positive/(positive+negative)        #calculates the probability
    logits[1+round(j/intervalLength)] = log(x/(1-x))        #puts the logit in the index of j
    means[1+round(j/intervalLength)] = sum/intervalLength   #puts the mean in the index of j 
    
  }
  
  #Check for infinite logits
  if (any(is.infinite(logits))) {
    warning("Infinite logits generated. Not all points will be plotted.")
  }
  
  #Plot each logit at the corresponding predictor value
  plot(means,logits,xlab=paste(predictor))
  
  #Plot a smoothing spline
  if (spline==TRUE) {
    spline = smooth.spline(means[!is.infinite(logits)], logits[!is.infinite(logits)], spar=spar)
    lines(spline)  
  }
}