
#Name: Mayuri Patel
#Email: mpate131@uncc.edu
#title: "Lab5_AdvStats"


#(1)	We will utilize our RNA seq dataset of E. Coli genes from mice.  The URL is here: http://afodor.github.io/classes/stats2015/longitdunalRNASeqData.zip. Read and normalize the counts table ( "nc101_scaff_dataCounts.txt " into R).

#- This code is all about unzipping the file:
#set the working directory to the file location
setwd("C:/Users/mpate131/Desktop/AdvStats")

#set the variable of the zip file
zipFile<- "C:/Users/mpate131/Desktop/AdvStats/longitdunalRNASeqData.zip"

outDir<-"C:/Users/mpate131/Desktop/AdvStats" # Define the folder where the zip file should be unzipped to 

unzip(zipFile,exdir=outDir)  # unzip the file 


#- Reading the file of datacount from the directory:
#read the table
myTable<-read.table("nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)
head(myTable) #viewing the data using head function

#- (The first 3 columns are "day 2", the next 3 columns are "week 12" and the last 5 are "week 18").  Remember, that day 2 is before the mice have inflammation symptoms, week 12 is associated with inflammation and week 18 is associated with cancer.

#- Normalize the data: 
# remove rare genes
myTable <- myTable[ apply( myTable,1, median)> 5,]

myTNorm <- myTable
#For loop each column
for ( i in 1:ncol(myTable))
{
  colSum = sum(myTable[,i]) # all rows and each column and summing all the column
  myTNorm[,i] =myTNorm[,i]/colSum # normalizing the data and saving it in the empty variable myTNorm.
  
}
head(myTNorm)

#(2)	For every row in the normalized spreadsheet, run three t-tests ( "day 2" vs. "week 12", "day 2" vs. "week 18" and "week 12" vs. "week 18".  At a p < .05 threshold fill in the following table:
# of genes significant at p <0.05 uncorrected	# genes significant at p <0.05 BH FDR corrected	# genes significant at p <0.05 Bonferroni corrected
                                                                      
#- The three t-tests:
#here, mean of day2 , week12, week18 is taken for those columns for each gene
#then do t-test for mean of "day 2" vs. "week 12", "day 2" vs. "week 18" and "week 12" vs. "week 18"

#t-test
#stored all the p values for each gene into a variable
d2w12_pval <- apply(myTNorm,1,function(x){t.test(x[1:3],x[4:6])$p.value}) #"day 2" vs. "week 12"
d2w18_pval <- apply(myTNorm,1,function(x){t.test(x[1:3],x[7:11])$p.value}) #"day 2" vs. "week 18"
w12w18_pval <- apply(myTNorm,1,function(x){t.test(x[4:6],x[7:11])$p.value})#"week 12" vs. "week 18"

#- For p <0.05 uncorrected:
#for d2w12:
uncorrected_d2w12 <- sum(d2w12_pval <= 0.05)
print(paste("The uncorrected p-values <0.05 for d2w12 is:",uncorrected_d2w12))
#for d2w18:
uncorrected_d2w18 <- sum(d2w18_pval <= 0.05)
print(paste("The uncorrected p-values <0.05 for d2w18 is:",uncorrected_d2w18))
#for w12w18:
uncorrected_w12w18 <- sum(w12w18_pval <= 0.05)
print(paste("The uncorrected p-values <0.05 for w12w18 is:",uncorrected_w12w18))

#- For p <0.05 BH FDR corrected:
# using the p.adjust function we can find the p values of BH FDR corrected
# taking the method
#for d2w12:
d2w12_BH <- p.adjust(d2w12_pval, method="BH") 
d2w12_BHpval <- sum(d2w12_BH<0.05)
print(paste("The p-values <0.05 BH FDR corrected for d2w12 is:", d2w12_BHpval))
#for d2w18:
d2w18_BH <- p.adjust(d2w18_pval, method="BH")
d2w18_BHpval <- sum(d2w18_BH<0.05)
print(paste("The p-values <0.05 BH FDR corrected for d2w18 is:", d2w18_BHpval))
#for w12w18:
w12w18_BH <- p.adjust(w12w18_pval, method="BH")
w12w18_BHpval <- sum(w12w18_BH<0.05)
print(paste("The p-values <0.05 BH FDR corrected for w12w18 is:", w12w18_BHpval))

#- For p <0.05 Bonferroni corrected:
# using the p.adjust function we can find the p values of bonferroni corrected
# taking the method
#for d2w12:
d2w12_bon <- p.adjust(d2w12_pval,method="bonferroni")
d2w12_bonpval <- sum(d2w12_bon < 0.05)
print(paste("The p-values <0.05 Bonferroni corrected for d2w12 is:", d2w12_bonpval))
#for d2w18:
d2w18_bon <- p.adjust(d2w18_pval, method="bonferroni")
d2w18_bonpval <- sum(d2w18_bon<0.05)
print(paste("The p-values <0.05 Bonferroni corrected for d2w18 is:", d2w18_bonpval))
#for w12w18:
w12w18_bon <- p.adjust(w12w18_pval, method="bonferroni")
w12w18_bonpval <- sum(w12w18_bon<0.05)
print(paste("The p-values <0.05 Bonferroni corrected for w12w18 is:", w12w18_bonpval))

#- Generated a table using data.frame and knitr
#generated the data frame of the table which we have to use
df <- data.frame("sample" = c("day2.Vs.week12","day2.Vs.week18","week12.Vs.week18"),
                 "No. genes significant at p <0.05 uncorrected" = c(uncorrected_d2w12,uncorrected_d2w18,uncorrected_w12w18), 
                 "No. genes significant at p <0.05 BH FDR " = c(d2w12_BHpval,d2w18_BHpval,w12w18_BHpval),
                 "No. genes significant at p <0.05 Bonferroni" = c(d2w12_bonpval, d2w18_bonpval, w12w18_bonpval))
knitr::kable(df) #this prints the table of the dataframe


#(3)	Make histograms of all the uncorrected p-values for each of the three companions.  Are any of the distributions uniform?
#histogram of d2w12:
uncorrected_d2w12 <- c() #empty variable to save the values from for loop
#looping p values to check for uncorrected p-values <0.05
for (i in d2w12_pval){
  if (i <= 0.05){
    uncorrected_d2w12 <- c(uncorrected_d2w12,i)
    
  }
  else {
    next
  }
}
hist(uncorrected_d2w12, xlab = "P-values", main="Histogram of day2 and week12 t-test$pvalue")

#histogram of d2w18:
uncorrected_d2w18 <- c()#empty variable to save the values from for loop
#looping p values to check for uncorrected p-values <0.05
for (i in d2w18_pval){
  if (i <= 0.05){
    uncorrected_d2w18 <- c(uncorrected_d2w18,i)
  }
  else {
    next
  }
}
hist(uncorrected_d2w18, xlab = "P-values", main="Histogram of day2 and week18 t-test$pvalue")

#histogram of w12w18:
uncorrected_w12w18 <- c()#empty variable to save the values from for loop
#looping p values to check for uncorrected p-values <0.05
for (i in w12w18_pval){
  if (i <= 0.05){
    uncorrected_w12w18 <- c(uncorrected_w12w18,i)
  }
  else {
    next
  }
}
hist(uncorrected_w12w18, xlab = "P-values", main="Histogram of week12 and week18 t-test$pvalue")

#- None of the three companions show the uniform distribution. 


#(4)	Based on these data, when is the biggest shift in the transcriptome?  Which samples are most different from one another?
#- The biggest shift in the transcriptome is seen in day 2 and week 18 pair of condition as it shows the most number of significant changes in genes. The samples most different from one another is also day 2 and week 18 pair as there is significant changes in the genes number over the other two. 

