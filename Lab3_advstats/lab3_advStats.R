#Name: Mayuri Patel
#Email: mpate131@uncc.edu
#title: "Lab3_AdvStats"

#Problem #1

#(1) You walk into the "occasionally dishonest casino"  with prior probabilities and likelihoods set to the values in slides 21-25 of lecture #4. (that is, a "loaded" die has a 10% chance of getting a 1-5 and a 50% chance of getting a 6 but 99% of the dice are fair)  

#You pick up one die and with it roll:  2 3 2 6 3 5 6 2 6 6 2 6 6 2 3 6 6 6 5 6 6 5 6 6 6 6 6 4 6 3 3 3 6 6 5 6 6

#Make a graph of the posterior probability that you have picked up a loaded die as a function of the number of times you have rolled the die.

#Show your code.

#You can represent the rolls as
#data<-c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)

#(2) How many times on average would you need to roll a loaded die to be 99.999%  sure that it was loaded?(Show your work)

#rolls used to roll a dice:
rolls <-c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)

#prior probability of loaded and fair dice
prior_prob <- c(0.01,0.99) # loaded, fair

#Likelihood probability of loaded and fair dice
likelihoodloaded <- c(0.1,0.1,0.1,0.1,0.1,0.5)
likelihoodfair <- c(1/6,1/6,1/6,1/6,1/6,1/6)

#Empty vector to store the posterior probability of loaded
post_probloaded <- vector();

#This is for the title to add the roll numbers
titleStr <- ""

#for loop for the length of the rolls
for (i in 1:length(rolls)){
  
  #taking posterior probability of loaded and saved in the empty vector post_probloaded
  post_probloaded[i] <- prior_prob[1]
  
  #Calculating the marginal probability for bayesian casino dishonest dice 
  denom <- prior_prob[1]*likelihoodloaded[rolls[i]]+prior_prob[2]*likelihoodfair[rolls[i]];
  
  #Calculating the posterior probability of loaded and fair dice
  prior_prob[1] = prior_prob[1]*likelihoodloaded[rolls[i]]/denom;
  prior_prob[2] = prior_prob[2]*likelihoodfair[rolls[i]]/denom;
  
  titleStr <- paste(titleStr,rolls[i],sep="") # this is to add the title for the graph
  Sys.sleep(1) # suspend execution of R for some time interval
  
}
#Plotting the graph for posterior probability of loaded dice: 
plot(1:i,post_probloaded,main = titleStr, ylim=c(0,1),xlim= c(1,length(rolls)), xlab = "rolls", ylab= "posterior probability loaded" )


#(2) On an average we need almost 27 rolls to roll a loaded die to be 99.999%  sure that it was loaded based on the above plot of posterior probability of loaded dice and also based on below R code. 


count = 1
for (i in post_probloaded){
  
  if (i >= 0.999){
    break
  }
  else{
    count = count + 1
    next
  }
}
print(paste("The number of rolls is:", count))


#Problem #2

#You are consulting for a hospital.  They have a diagnostic test for a disease with a known background prevalence of 0.1%.

#The test has the following properties:
#p(positive result | person has disease) = 0.91
#p(negative result| person does not have disease) = 0.84

#The cost of running the test one time is $1.  The test can be repeated for each patient and the results of the test are independent of one another allowing for Bayesian updates.  The test always yields a positive or negative result.

#The requirement of the hospital is that the test is repeated for each patient until a Bayesian posterior of at least 0.99999 is reached.

#(1) Run simulations for a patient with the disease.  About how many times on average must the test be repeated to achieve the hospital's requirements?  
  
#(2) Repeat the simulations for a patient without the disease.  About how many times on average must the test be repeated to achieve the hospital's requirements?
  
#(3) The hospital plans to run the test on one million patients per year.  At a cost of $1 per test, about how much should the hospital budget to run these tests?  (That is to say, for a million patients, how many tests can the hospital anticipate running?)

#Show your work/code/justification for all answers.  	

#-(1)
# prob with disease; prob without disease
prior_test <-  c(0.001,0.999)

# p(positive) , p(negative)
likelihooddisease <- c(0.91, 0.09)
likelihoodNotdisease<- c(0.16,0.84)

#A function which takes in likelihood number and number of test
getDataFromLikelihood <- function( likelihood, numPoints) 
{
  d <- vector(mode="integer", length=numPoints);
  
  #the 1 is for the positive result; 2 is for the negative result
  # for loop the number of test 
  for( i in 1:numPoints ) 
  {
    #This is for taking with disease as number 1.
    if( runif(1) <= likelihood[1] )
    {
      d[i] <- 1; #this is for with disease
    }
    else
    {
      d[i] <- 2; # this is for without the disease
    }
    
  }
  #return the vector
  return(d)
}

#get our simulated data for disease patients and passing the test number
data <- getDataFromLikelihood(likelihooddisease, 15)

#Empty vector to store the posterior probability with disease
probVals_disease <- vector();

#This is for the title to add with disease number 1 
titleStr <- ""

#for loop for the length of the data taken for stimulation
for( i in 1:length(data))
{
  #taking posterior probability of disease and saved in the empty vector probvals
  probVals_disease[i] <- prior_test[1];
  
  #Calculating the marginal probability for a test
  denom <- prior_test[1] * likelihooddisease[data[i]] + prior_test[2] * likelihoodNotdisease[data[i]];
  
  #Calculating the posterior probability with disease and without disease
  prior_test[1] = prior_test[1] * likelihooddisease[data[i]] / denom;
  prior_test[2] = prior_test[2] * likelihoodNotdisease[data[i]] / denom;
  
  
  titleStr <- paste(titleStr,data[i],sep="")# this is to add the title for the graph
  Sys.sleep(1); # suspend execution of R for some time interval
}
#Plotting the graph for posterior probability with disease: 
plot(1:i,probVals_disease, main = titleStr,ylim=c(0,1),xlim=c(1,length(data)+1),xlab = "test numbers", ylab= "With disease probVals")


#- Almost 15 tests must be on average be repeated to achieve the hospital's requirements of 0.99999 for a patient with the disease.  

count = 1
for (i in probVals_disease){
  if (i >= 0.9999){
    break
  }
  else{
    count = count + 1
    next
  }
}
print(paste("The number of test required for patients with disease:", count))

#-(2)

# prob with disease; prob without disease
prior_test <-  c(0.001,0.999)

# p(positive) , p(negative)
likelihooddisease <- c( 0.91, 0.09)
likelihoodNotdisease<- c(0.16,0.84)


#A function which takes in likelihood number and number of test
getDataFromLikelihood <- function( likelihood, numPoints) 
{
  d <- vector(mode="integer", length=numPoints);
  
  # for loop the number of test 
  #the 1 is for the positive result; 2 is for the negative result
  for( i in 1:numPoints ) 
  {
    #This is for taking with disease as number 1.
    if( runif(1) <= likelihood[1] )
    {
      d[i] <- 1; #this is for with disease
    }
    else
    {
      d[i] <- 2; # this is for without the disease
    }
    
  }
  
  return(d)
}

#get our simulated data for not disease patients and number of test be done
data <- getDataFromLikelihood(likelihoodNotdisease, 5)

#Empty vector to store the posterior probability without disease.
probVals_withoutDisease <- vector();

#This is for the title to add without disease number 2 
titleStr <- ""

#for loop for the length of the data taken for stimulation
for( i in 1:length(data))
{
  #taking posterior probability of without disease and saved in the empty vector probvals without disease
  probVals_withoutDisease[i] <- prior_test[2];
  
  #Calculating the marginal probability for a test
  denom <- prior_test[1] * likelihooddisease[data[i]] + prior_test[2] * likelihoodNotdisease[data[i]];
  
  #Calculating the posterior probability with disease and without disease
  prior_test[1] = prior_test[1] * likelihooddisease[data[i]] / denom;
  prior_test[2] = prior_test[2] * likelihoodNotdisease[data[i]] / denom;
  
  
  titleStr <- paste(titleStr,data[i],sep="")# this is to add the title for the graph
  Sys.sleep(1);# suspend execution of R for some time interval
}
#Plotting the graph for posterior probability without disease:
plot(1:i,probVals_withoutDisease, main = titleStr,ylim=c(0,1),xlim=c(1,length(data)),xlab = "test numbers", ylab= "Without disease probVals")

#- About 3 tests must be on average be repeated to achieve the hospital's requirements of 0.99999 for a patient without the disease. 

count = 1
for (i in probVals_withoutDisease){
  
  if (i >= 0.9999){
    break
  }
  else{
    count = count + 1
    next
  }
}
print(paste("The number of test required for patients without disease:", count))

#-(3) how much should the hospital budget to run these tests? 
#- 1 million patients/year, $1/ test, and budget =?; To solve this: 1 million * 0.001 * number of test with disease + 1 million * 0.999 * number of test without disease.
budget <- 1000000 * 0.001 * 15 + 1000000 * 0.999 * 3.
print(paste("The hospital budget required to run these tests are:", budget))

#- So, it is almost 3 million... 

#Problem #3

#Another manufacturer approaches the hospital with an improved, but more expensive, test with the following properties 

#p(positive result | person has disease) = 0.96
#p(negative result| person does not have disease) = 0.95

#(1) With this test, how many times on average must the test be repeated to achieve the hospital's requirements for patients with and without the disease?
  
#(2) Considering only the cost of the test, and assuming the hospital will screen one million patients with a background prevalence of 0.1%, at about what price point for running the test one time will the hospital save money by switching to the new test?  
  
#-(1)
#- For disease:
  
# prob with disease; prob without disease
prior_tests <-  c(0.001,0.999)

# p(positive) , p(negative)
likelihooddisease <- c( 0.96, 0.04)
likelihoodNotdisease<- c(0.05,0.95)

#A function which takes in likelihood number and number of test
getDataFromLikelihood <- function( likelihood, numPoints) 
{
  d <- vector(mode="integer", length=numPoints);
  
  # for loop the number of test 
  #the 1 is for the positive result; 2 is for the negative result
  for( i in 1:numPoints ) 
  {
    #This is for taking with disease as number 1.
    if( runif(1) <= likelihood[1] )
    {
      d[i] <- 1; #this is for with disease
    }
    else
    {
      d[i] <- 2; # this is for without the disease
    }
    
  }
  
  return(d)
}


#get our simulated data for disease patients and number of test be done
data <- getDataFromLikelihood(likelihooddisease, 10)

#Empty vector to store the posterior probability with disease.
probVals_Disease <- vector();

#This is for the title to add with disease number 1 
titleStr <- ""

#for loop for the length of the data taken for stimulation
for( i in 1:length(data))
{
  #taking posterior probability of with disease and saved in the empty vector probvals with disease
  probVals_Disease[i] <- prior_tests[1];
  
  #Calculating the marginal probability for a test
  denom <- prior_tests[1] * likelihooddisease[data[i]] + prior_tests[2] * likelihoodNotdisease[data[i]];
  
  #Calculating the posterior probability with disease and without disease
  prior_tests[1] = prior_tests[1] * likelihooddisease[data[i]] / denom;
  prior_tests[2] = prior_tests[2] * likelihoodNotdisease[data[i]] / denom;
  
  
  titleStr <- paste(titleStr,data[i],sep="")# this is to add the title for the graph
  Sys.sleep(1);# suspend execution of R for some time interval
}
#Plotting the graph for posterior probability with disease:
plot(1:i,probVals_Disease, main = titleStr,ylim=c(0,1),xlim=c(1,length(data)+1),xlab = "test numbers", ylab= "With disease probVals")

count = 1
for (i in probVals_Disease){
  if (i >= 0.9999){
    break
  }
  else{
    count = count + 1
    next
  }
}
print(paste("The number of test required for patients with disease:", count))


#- For without disease:
# prob with disease; prob without disease
prior_tests <-  c(0.001,0.999)

# p(positive) , p(negative)
likelihooddisease <- c( 0.96, 0.04)
likelihoodNotdisease<- c(0.05,0.95)

#A function which takes in likelihood number and number of test
getDataFromLikelihood <- function( likelihood, numPoints) 
{
  d <- vector(mode="integer", length=numPoints);
  
  # for loop the number of test 
  #the 1 is for the positive result; 2 is for the negative result
  for( i in 1:numPoints ) 
  {
    #This is for taking with disease as number 1.
    if( runif(1) <= likelihood[1] )
    {
      d[i] <- 1; #this is for with disease
    }
    else
    {
      d[i] <- 2; # this is for without the disease
    }
    
  }
  
  return(d)
}


#get our simulated data for without disease patients and number of test be done
data <- getDataFromLikelihood(likelihoodNotdisease, 5)

#Empty vector to store the posterior probability without disease.
probVals_notDisease <- vector();

#This is for the title to add without disease number 2
titleStr <- ""

#for loop for the length of the data taken for stimulation
for( i in 1:length(data))
{
  #taking posterior probability of without disease and saved in the empty vector probvals without disease
  probVals_notDisease[i] <- prior_tests[2];
  
  #Calculating the marginal probability for a test
  denom <- prior_tests[1] * likelihooddisease[data[i]] + prior_tests[2] * likelihoodNotdisease[data[i]];
  
  #Calculating the posterior probability with disease and without disease
  prior_tests[1] = prior_tests[1] * likelihooddisease[data[i]] / denom;
  prior_tests[2] = prior_tests[2] * likelihoodNotdisease[data[i]] / denom;
  
  
  titleStr <- paste(titleStr,data[i],sep="")# this is to add the title for the graph
  Sys.sleep(1);# suspend execution of R for some time interval
}
#Plotting the graph for posterior probability without disease:
plot(1:i,probVals_notDisease, main = titleStr,ylim=c(0,1),xlim=c(1,length(data)),xlab = "test numbers", ylab= "Without disease probVals")

count = 1
for (i in probVals_notDisease){
  if (i >= 0.9999){
    break
  }
  else{
    count = count + 1
    next
  }
}
print(paste("The number of test required for patients without disease:", count))

#- With the disease the number of tests that must be repeated are almost 7 and without the disease it should be 2 to meet the hospital requirement. 

#-(2) what price point for running the test one time will the hospital save money by switching to the new test?
#- For running the test one time:

budget <- 1000000 * 0.001 * 7 + 1000000 * 0.999 * 2
print(paste("The hospital budget required to run this test is:", budget))

#- So, the hospital save money by switching to the new test is almost 1 million as it comes from 3 million from question 2 test result output minus the 2 million from question 3 test result output. 