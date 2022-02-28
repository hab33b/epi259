library(pROC)

#ROC curve function
  #author: Matthew Sigurdson (msigurds@stanford.edu), based on original calculation by Paul Riviere
  #requires pROC package

#Three required arguments: takes in a model from glm(), a data frame, and the name of the outcome column

#Three optional return arguments (select at most one; default value is FALSE for each):
  #returnValues outputs a data frame containing the sensitivity and specificity for each possible threshold
  #returnPred outputs the predicted probabilities from the model, with 95% confidence intervals
  #returnROC outputs the full roc object

#Optional weights argument: takes in a string identifying the name of the weights column
  #used if the model was created using the weights argument in the glm() function

#Rows containing NA values in the rows used to fit the model should be removed from the dataset before use

#Example usage: roc.curve(glm.fit, data, "outcomeName")

roc.curve = function(glm.fit,data,outcome,returnValues=FALSE,returnPred=FALSE,returnROC=FALSE,weights=NULL) {
  #Make sure pROC package is present
  if (!"package:pROC" %in% search()) {stop("This function requires the pROC package.")}
  
  #Check input
  if (!is.character(outcome)) {stop("Outcome must be a string.")}
  if (!is.null(weights) & !is.character(weights)) {stop("Weights argument must be a string.")}
  if (sum(returnValues,returnPred,returnROC)>1) {stop("Select one output at a time.")}
  
  #Define outcome column by renaming it
  outcomeIndex = which(colnames(data)==outcome)
  names(data)[outcomeIndex] = "outcome"
  
  #Make prediction based on the fit and add to data frame
  prediction = predict(glm.fit,type=c("response"))
  data = data.frame(data,prediction)
  
  #Multiply rows based on weights variable if present
  if (!is.null(weights)) {
    data = data[rep(row.names(data), unlist(data[weights])), -(names(data)==weights)]
  }
  
  #Calculate ROC curve
  curve = roc(outcome~prediction,data=data)
  plot(curve)    
  print(curve)   #Summarizes the result, including the area under the curve
    #By default, the plot function labels the x-axis as "Specificity" (from 1 to 0) instead of "1-specificity" from 0 to 1
  
  #Output, if requested
  if (returnValues==TRUE) {
    output = data.frame(curve$thresholds,curve$sensitivities,curve$specificities)
    names(output) = c("threshold","sensitivity","specificity")
    return(round(output,digits=10))   #rounding is to prevent display as scientific notation
  }
  
  if (returnPred==TRUE) {
    #For confidence intervals: calculate the predictions on the linear ("link") scale
    linkPred = predict(glm.fit, type=c("link"), se.fit=TRUE)
    
    #Calculate the 95% confidence intervals, then convert back to the logistic scale using the inverse logit function
    lower.limit = linkPred$fit - 1.96 * linkPred$se.fit
    lower.limit = exp(lower.limit)/(1+exp(lower.limit))
    upper.limit = linkPred$fit + 1.96 * linkPred$se.fit
    upper.limit = exp(upper.limit)/(1+exp(upper.limit))
    
    output = data.frame(prediction, lower.limit, upper.limit)
    return(round(output,digits=10))   #rounding is to prevent display as scientific notation
  }
  
  if (returnROC==TRUE) {return(curve)}
}
