
#Name: Mayuri Patel
#Email: mpate131@uncc.edu
#title: "Lab2_AdvStats"


#Question #1

#In a population, there is 1/3 chance that a given person has a mutation in some gene.
#You sample 30 people; what are the odds that exactly 12 of the people have the mutation? 
#In R plot a probability density function (with dbinom) that shows the distribution for observing exactly (0,1,2,???30) people with the mutation.
#What is the mean and variance for the expected number of people with the mutation.

#- The probability to have a mutation is given as 1/3 (0.33).
#The odds that exactly 12 people have mutation is:
#dbinom(k/x,n,prob=0.33)

dbinom(12,30,prob=0.33)

#- So this means that there is a 10% chance that 12 people will be having the mutation from a sample of 30 people with a probability of 0.33.

#- The R plot for probability density function for 0-30 people with mutation:
#sample the data from 0-30 into a variable
sample <- 0:30
#PDF plot for the samples are:
plot(sample,dbinom(sample,30,0.33),main="probability density function plot")  

#- The mean and variance are:
#The mean:
#n*p
samp_mean <- 30 *0.33
print(paste("The Mean value is:", samp_mean))
#The variance:
#n*p*(1-p)
samp_var <- 30*0.33*(1-0.33)
print(paste("The Variance value is:", samp_var))

#Question #2

#The background expected survival rate for a disease is 0.4.  You are running a clinical trial. You have 100 patients on a new drug.  
#47 patients die (and 53 survive). From the bionomial test:
  
#(2A) Plot out the probability density function with the x-axis the number of patients that survive under the null hypothesis. 

#- The probability of expected survival rate for a disease is given as 0.4 with 53 patients surviving the disease.                                                                                       
#The number of patients survived is stored in a variable
survived <- c(0:53)
#Calculating the dbinom for each survived patients
dbinom(survived,100,prob=0.4)                                                                                       
#plot for survived patients in PDF:
plot(survived,dbinom(survived,100,0.4),type='h',main="probability density function plot", xlab="patients survived")
#Generating PDF plot for 100 patients
patients <- c(0:100)
#Calculating the dbinom for each survived patients
#plot for survived patients in PDF:
plot(patients,dbinom(patients,100,0.4),type='h',main="probability density function plot", xlab="patients survived")


#(2B) What is the p-value for a null hypothesis that the drug has no effect.(Show the one line of R code that produces this p-value)

#- The null hypothesis that drug has no effect and alternative being that drug has effect. Because of this, the alternative is two-tailed test (two.sided). The P-value for 53 patients is calculated using binom.test.
#Here, taking 53 surviving patients with its probability
binom.test(53,100,0.4,alternative = "two.sided")


#(2C) What is the p-value for a null hypothesis that the drug does not improve survival.(show you can get the same answer with binom.test(???.) and sum(dbinom(???.))

#- The null hypothesis that drug does not improve survival that means it is one tailed test depending on survival and death of the patients. The P-value for these is calculated using binom.test and sum(dbinom()). 
#The died patient pdf plot will be left skewed (left tailed test/less) as the survived patient pdf plot is right skewed from the above plot. So, I used alternative as less. 
#Here,taking 47 die patients with its probability as 0.6
binom.test(47,100,0.6,alternative = "less")
#Altenative to binom.test can be done using sum(binom())
sum(dbinom(0:47,100,0.6))
pbinom(47,100,0.6)
#Here, taking 53 survived patients with its probability as 0.4
binom.test(53,100,0.4,alternative = "greater")

#The resultant P-value from binom.test for survived and died patients are same.


#Question #3

#(3A)	Use the rbinom function to simulate 1,000 experiments in which 10,000 patients are sampled with a 1/2 chance of seeing a mutation. (You should get 1,000 numbers back with each # the # of patients from the 10,000 that had the mutation???) (What is the one line of r-code that would produce myVals?)

#- The probability of seeing a mutation is given as 0.5.
#The one line code:
#rbinom simulate the random numbers from 1 to 10000 with a limit of 1000 return numbers.
exp <- rbinom(1000,10000,p=0.5)
#This shows the first few numbers of exp variable
head(exp)
#This gives the total length of the variable exp
length(exp)


#(3B) What is the expected mean and variance of the vector in (3A).  Show that the actual mean and variance are close to the expected mean and variance.

#- The expected mean and variance of the vector exp is:      
#The mean is:
mean(exp)
#The variance is:
var(exp)
#- The actual mean and variance of the vector exp is:
#here, n= sample size, p=probability
#mean = np
exp_mean <- 10000*0.5
exp_mean
#variance = np(1-p)
exp_var <- 10000*0.5*(1-0.5)
exp_var

#- The actual mean and variance are close to the expected mean and variance as can be seen in the above code result.


#(3C) Take the vector that results from (3A). For each element in that vector, calculate a p-value with binom.test(???.)$p.value for the null hypothesis that the frequency of the allele in the population for that experiment is 1/2.
#Graph the histogram of all of those p-values. What distribution would you expect?  Is that what you see?
                                                                                                                                           
#exp variable that calculate rbinom
exp <- rbinom(1000,10000,p=0.5)
#An empty variable to store all the p-values generated from for loop
pval <- c()
#looping the exp sample
for (i in exp){
  test = binom.test(i,10000,0.5) # using binom.test to generate p-value
  pval <- c(pval, test$p.value) # taking p-value from the binom.test and stored into pval variable
}

#generating the histogram using all the p-values.
hist(pval, main="Histogram of P-values from binom.test",xlab="P-values")

#- It is expected to be a uniform distribution, but it is not so because it should be equal spread without peaks.


#(3D) Change the expected value of 1/2 in (3C) to some other value. 
#What happens to the p-values in the histogram. Would you expect the same shape of the p-value histogram with expected values of .49 as with .51?  Why or why not?
                                                                                                                                           
#- Changing the probability value from 0.5 to 0.49 in binom.test
#exp variable that calculate rbinom
exp <- rbinom(1000,10000,p=0.5)
#An empty variable to store all the p-values generated from for loop
pval <- c()
#looping the exp sample
for (i in exp){
  test = binom.test(i,10000,0.49) # using binom.test to generate p-value with change in probability value
  pval <- c(pval, test$p.value) # taking p-value from the binom.test and stored into pval variable
}

#generating the histogram using all the p-values.
hist(pval,main="Histogram of P-values from binom.test",xlab="P-values")

#- Changing the probability value from 0.5 to 0.51 in binom.test
#exp variable that calculate rbinom
exp <- rbinom(1000,10000,p=0.5)
#An empty variable to store all the p-values generated from for loop
pval <- c()
#looping the exp sample
for (i in exp){
  test = binom.test(i,10000,0.51) # using binom.test to generate p-value with change in probability value
  pval <- c(pval, test$p.value) # taking p-value from the binom.test and stored into pval variable
}

#generating the histogram using all the p-values.
hist(pval)

#- The p-values in the histogram changed with the change in the probability value in binom.test. The shape of p-values in the histogram is skewed away from the uniform distribution. 
