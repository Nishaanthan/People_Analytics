# Install and declare packages
install.packages('arules')
install.packages('arulesViz')
library('arules')
library('arulesViz')

### Load data ###
data <- read.csv("https://raw.githubusercontent.com/Nishaanthan/People_Analytics/refs/heads/main/datasets/fau_clinic_recruitment.csv")
head(data)
summary(data)

relevant_data <- subset(data, select = -c(hired))
rules <- apriori(relevant_data, 
                 parameter = list(supp=0.02, conf=0.5, target="rules"),
                 appearance = list(default="lhs", rhs="critical_care_nursing"))
inspect(head(rules, n = 10, by = "lift"))

subrules <- head(rules, n = 10, by = "lift")
inspect(subrules)

### Case 2 ###

##  Functions for calculating support, confidence, and lift ##

supp <- function(mydata, premise, implication=list()) {
  count_all <- nrow(mydata)
  conditions <- append(premise, implication)
  for(condition in conditions)
    mydata <- mydata[mydata[condition[1]] == condition[2],]
  return (nrow(mydata) / count_all)
}

conf <- function(mydata, premise, implication) {
  num <- supp(mydata, premise, implication)
  den <- supp(mydata, premise)
  return (num/den)
}

lift <- function(mydata, premise, implication) {
  num <- conf(mydata, premise, implication)
  den <- supp(mydata, implication)
  return (num/den)
}


## Function that calculates right hand side of inequation ##

fairness_coeff <- function(mydata, a, b, c, d=NULL, alpha=1.25) {
  if (is.null(d)) {
    bd <- list(b)
  } else {
    bd <- list(b, d)
  }
  val1 <- supp(mydata, bd, list(a))
  val2 <- supp(mydata, list(b), list(a))
  val3 <- conf(mydata, bd, list(a))
  val4 <- conf(mydata, bd, list(c))
  return( ((val1/val2) * (val3+val4-1)) / (val2 * alpha) )
}


## Function that changes the label of one row in the data set to remove discrimination ##

change_label <- function(data, a, b, c, d=NULL) {
  mydata <- data
  mydata$id <- seq(1, nrow(mydata))
  if(c[2] == "TRUE")
    target <- FALSE
  else
    target <- TRUE
  c <- c(c[1], target)
  conditions <- list(a, b, c)
  if (!is.null(d))
    conditions <- append(conditions, list(d))
  for(condition in conditions)
    mydata <- mydata[mydata[condition[1]] == condition[2],]
  ids = mydata$id
  if(length(ids) > 0) {
    data[ids[1], c[1]] <- !target
    return(data)
  } else
    return(NULL)
}


## Function that removes discrimination for one rule ##

remove_discrimination <- function(data, a, b, c, d=NULL) {
  left <- conf(data, list(b), list(c))
  right <- fairness_coeff(data, a, b, c, d)
  message("Initial values of inequation: ", left, " >? ", right)
  if(left > right) {
    message("No action needed.")
    return(data)
  }
  message("Removing detected discrimination...")
  counter = 0
  while(left <= right) {
    new_data <- change_label(data, a, b, c, d)
    if(is.null(new_data)) {
      message("Changed ", counter, " labels. Unable to change more labels. ", 
              "New left value: ", left)
      return(data)
    }
    data <- new_data
    left <- conf(data, list(b), list(c))
    counter <- counter + 1
  }
  message("Changed ", counter, " labels. New left value: ", left, 
          "\nRule is now discrimination-free.")
  return(data)
}

a <- c("gender", "m")
b <- c("empathy", TRUE)
c <- c("critical_care_nursing", TRUE)
d <- c("professional", TRUE)

data <- remove_discrimination(data, a, b, c, d)
